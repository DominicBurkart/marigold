//! Background telemetry lookups for the language server.
//!
//! A [`Lookups`] owns one worker thread that calls the [`TelemetrySource`], so the request loop
//! never waits on the network. [`Lookups::request`] returns the last stats known for the
//! document immediately and schedules a refresh when the cached answer for the same document,
//! program and node-id set is older than [`SUCCESS_TTL`]. Concurrent requests for a key that is
//! being fetched are coalesced, and a failing source is retried with exponential backoff capped
//! at [`BACKOFF_CAP`].

use crate::telemetry::{NodeStats, TelemetrySource};
use marigold_grammar::NodeId;
use std::collections::HashMap;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

pub const SUCCESS_TTL: Duration = Duration::from_secs(30);
pub const BACKOFF_BASE: Duration = Duration::from_secs(5);
pub const BACKOFF_CAP: Duration = Duration::from_secs(300);

pub type Clock = Arc<dyn Fn() -> Instant + Send + Sync>;
pub type Completion = Box<dyn Fn(bool) + Send>;

/// The delay before retrying after `failures` consecutive failures.
///
/// ```
/// use marigold_lsp::lookup::backoff_delay;
/// use std::time::Duration;
///
/// assert_eq!(backoff_delay(1), Duration::from_secs(5));
/// assert_eq!(backoff_delay(2), Duration::from_secs(10));
/// assert_eq!(backoff_delay(100), Duration::from_secs(300));
/// ```
pub fn backoff_delay(failures: u32) -> Duration {
    let shift = failures.saturating_sub(1).min(16);
    BACKOFF_BASE
        .checked_mul(1u32 << shift)
        .map_or(BACKOFF_CAP, |d| d.min(BACKOFF_CAP))
}

/// What a cached answer is for: one document, program and set of node ids.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key {
    uri: String,
    program: String,
    ids: Vec<NodeId>,
}

impl Key {
    pub fn new(uri: &str, program: &str, ids: impl IntoIterator<Item = NodeId>) -> Self {
        let mut ids: Vec<NodeId> = ids.into_iter().collect();
        ids.sort();
        ids.dedup();
        Self {
            uri: uri.to_string(),
            program: program.to_string(),
            ids,
        }
    }
}

struct Entry {
    next_attempt: Instant,
    failures: u32,
    in_flight: bool,
}

#[derive(Default)]
struct Inner {
    entries: HashMap<Key, Entry>,
    latest: HashMap<String, Vec<NodeStats>>,
}

pub struct Lookups {
    inner: Arc<Mutex<Inner>>,
    jobs: Sender<Key>,
    clock: Clock,
}

impl Lookups {
    /// Starts the worker. `on_complete` runs on the worker after every lookup, with whether the
    /// stats known for the document changed.
    pub fn spawn(
        source: Box<dyn TelemetrySource + Send>,
        clock: Clock,
        on_complete: Completion,
    ) -> Self {
        let inner = Arc::new(Mutex::new(Inner::default()));
        let (jobs, queue) = channel();
        let worker = Worker {
            inner: Arc::clone(&inner),
            clock: Arc::clone(&clock),
            source,
            on_complete,
        };
        std::thread::spawn(move || worker.run(queue));
        Self { inner, jobs, clock }
    }

    /// The last stats known for the key's document, scheduling a refresh when one is due.
    pub fn request(&self, key: Key) -> Vec<NodeStats> {
        let now = (self.clock)();
        let mut inner = lock(&self.inner);
        let due = inner
            .entries
            .get(&key)
            .is_none_or(|e| !e.in_flight && now >= e.next_attempt);
        let known = inner.latest.get(&key.uri).cloned().unwrap_or_default();
        if due {
            let uri = key.uri.clone();
            inner
                .entries
                .retain(|k, e| e.in_flight || k.uri != uri || *k == key);
            let failures = inner.entries.get(&key).map_or(0, |e| e.failures);
            inner.entries.insert(
                key.clone(),
                Entry {
                    next_attempt: now,
                    failures,
                    in_flight: true,
                },
            );
            if self.jobs.send(key.clone()).is_err() {
                if let Some(e) = inner.entries.get_mut(&key) {
                    e.in_flight = false;
                }
            }
        }
        known
    }
}

fn lock(inner: &Mutex<Inner>) -> std::sync::MutexGuard<'_, Inner> {
    inner.lock().unwrap_or_else(|p| p.into_inner())
}

struct Worker {
    inner: Arc<Mutex<Inner>>,
    clock: Clock,
    source: Box<dyn TelemetrySource + Send>,
    on_complete: Completion,
}

impl Worker {
    fn run(self, queue: Receiver<Key>) {
        while let Ok(first) = queue.recv() {
            let mut latest_per_uri: Vec<Key> = vec![first];
            while let Ok(next) = queue.try_recv() {
                match latest_per_uri.iter_mut().find(|k| k.uri == next.uri) {
                    Some(slot) => {
                        self.abandon(slot);
                        *slot = next;
                    }
                    None => latest_per_uri.push(next),
                }
            }
            for key in latest_per_uri {
                self.fetch(key);
            }
        }
    }

    fn abandon(&self, key: &Key) {
        let now = (self.clock)();
        if let Some(e) = lock(&self.inner).entries.get_mut(key) {
            e.in_flight = false;
            e.next_attempt = now;
        }
    }

    fn fetch(&self, key: Key) {
        let outcome = catch_unwind(AssertUnwindSafe(|| {
            self.source.node_stats(&key.program, &key.ids)
        }));
        let now = (self.clock)();
        let mut changed = false;
        {
            let mut inner = lock(&self.inner);
            let entry = inner.entries.entry(key.clone()).or_insert(Entry {
                next_attempt: now,
                failures: 0,
                in_flight: true,
            });
            entry.in_flight = false;
            match outcome {
                Ok(Ok(stats)) => {
                    entry.failures = 0;
                    entry.next_attempt = now + SUCCESS_TTL;
                    changed = inner.latest.get(&key.uri) != Some(&stats);
                    inner.latest.insert(key.uri.clone(), stats);
                }
                _ => {
                    entry.failures = entry.failures.saturating_add(1);
                    entry.next_attempt = now + backoff_delay(entry.failures);
                }
            }
        }
        (self.on_complete)(changed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::telemetry::TelemetryError;
    use marigold_grammar::marigold_node_ids;
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct Fake {
        calls: Arc<AtomicUsize>,
        gate: Option<Mutex<Receiver<()>>>,
        entered: Mutex<Sender<()>>,
        fail: Arc<std::sync::atomic::AtomicBool>,
        panic: bool,
    }

    impl TelemetrySource for Fake {
        fn node_stats(
            &self,
            _program: &str,
            node_ids: &[NodeId],
        ) -> Result<Vec<NodeStats>, TelemetryError> {
            self.calls.fetch_add(1, Ordering::SeqCst);
            let _ = self.entered.lock().unwrap().send(());
            if let Some(gate) = &self.gate {
                let _ = gate.lock().unwrap().recv();
            }
            if self.panic {
                panic!("fake source panic");
            }
            if self.fail.load(Ordering::SeqCst) {
                return Err(TelemetryError::Unavailable("down".into()));
            }
            Ok(node_ids.iter().map(stat).collect())
        }
    }

    fn stat(id: &NodeId) -> NodeStats {
        NodeStats {
            node_id: id.clone(),
            observed_inputs: 1,
            observed_runs: 1,
            errors: 0,
            last_seen: None,
            window: Duration::from_secs(60),
            content_hash: None,
        }
    }

    #[derive(Clone)]
    struct TestClock {
        base: Instant,
        offset: Arc<Mutex<Duration>>,
    }

    impl TestClock {
        fn new() -> Self {
            Self {
                base: Instant::now(),
                offset: Arc::new(Mutex::new(Duration::ZERO)),
            }
        }
        fn advance(&self, by: Duration) {
            *self.offset.lock().unwrap() += by;
        }
        fn clock(&self) -> Clock {
            let this = self.clone();
            Arc::new(move || this.base + *this.offset.lock().unwrap())
        }
    }

    struct Harness {
        lookups: Lookups,
        calls: Arc<AtomicUsize>,
        done: Receiver<bool>,
        clock: TestClock,
        fail: Arc<std::sync::atomic::AtomicBool>,
        gate: Sender<()>,
        entered: Receiver<()>,
        syncs: AtomicUsize,
    }

    fn harness(gated: bool, panic: bool) -> Harness {
        let calls = Arc::new(AtomicUsize::new(0));
        let fail = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let (gate_tx, gate_rx) = channel();
        let (entered_tx, entered) = channel();
        let fake = Fake {
            entered: Mutex::new(entered_tx),
            calls: Arc::clone(&calls),
            gate: gated.then(|| Mutex::new(gate_rx)),
            fail: Arc::clone(&fail),
            panic,
        };
        let (done_tx, done) = channel();
        let done_tx = Mutex::new(done_tx);
        let clock = TestClock::new();
        let lookups = Lookups::spawn(
            Box::new(fake),
            clock.clock(),
            Box::new(move |changed| {
                let _ = done_tx.lock().unwrap().send(changed);
            }),
        );
        Harness {
            lookups,
            calls,
            done,
            clock,
            fail,
            gate: gate_tx,
            entered,
            syncs: AtomicUsize::new(0),
        }
    }

    fn key(uri: &str, src: &str) -> Key {
        Key::new(
            uri,
            "demo",
            marigold_node_ids(src, "demo").into_iter().map(|n| n.id),
        )
    }

    impl Harness {
        fn wait(&self) -> bool {
            self.done
                .recv_timeout(Duration::from_secs(30))
                .expect("lookup did not complete")
        }

        fn sync(&self) {
            let n = self.syncs.fetch_add(1, Ordering::SeqCst);
            self.lookups
                .request(key(&format!("file:///sync{n}"), "range(0, 1).return"));
            self.wait();
        }
    }

    const SRC: &str = "range(0, 3).map(f).return";

    #[test]
    fn request_returns_without_waiting_for_a_blocked_source() {
        let h = harness(true, false);
        let first = h.lookups.request(key("file:///a", SRC));
        assert!(first.is_empty());
        let still = h.lookups.request(key("file:///a", SRC));
        assert!(still.is_empty());
        h.gate.send(()).unwrap();
        assert!(h.wait());
        let later = h.lookups.request(key("file:///a", SRC));
        assert_eq!(later.len(), marigold_node_ids(SRC, "demo").len());
        assert_eq!(h.calls.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn concurrent_requests_for_one_key_coalesce() {
        let h = harness(true, false);
        for _ in 0..10 {
            h.lookups.request(key("file:///a", SRC));
        }
        h.gate.send(()).unwrap();
        h.wait();
        h.gate.send(()).unwrap();
        h.sync();
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn identical_id_sets_hit_the_cache_even_when_text_differs() {
        let h = harness(false, false);
        h.lookups.request(key("file:///a", SRC));
        h.wait();
        let spaced = "range(0, 3)  .map(f)\n.return";
        assert_eq!(key("file:///a", SRC), key("file:///a", spaced));
        let found = h.lookups.request(key("file:///a", spaced));
        assert!(!found.is_empty());
        h.sync();
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn changed_id_sets_miss_but_keep_serving_the_last_known_stats() {
        let h = harness(true, false);
        h.lookups.request(key("file:///a", SRC));
        h.gate.send(()).unwrap();
        h.wait();
        let edited = "range(0, 3).map(g).filter(h).return";
        assert_ne!(key("file:///a", SRC), key("file:///a", edited));
        let served = h.lookups.request(key("file:///a", edited));
        assert_eq!(served.len(), marigold_node_ids(SRC, "demo").len());
        h.gate.send(()).unwrap();
        h.wait();
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn different_programs_and_documents_do_not_share_entries() {
        let h = harness(false, false);
        let ids: Vec<NodeId> = marigold_node_ids(SRC, "demo")
            .into_iter()
            .map(|n| n.id)
            .collect();
        h.lookups.request(Key::new("file:///a", "one", ids.clone()));
        h.wait();
        h.lookups.request(Key::new("file:///a", "two", ids.clone()));
        h.wait();
        h.lookups.request(Key::new("file:///b", "two", ids));
        h.wait();
        assert_eq!(h.calls.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn fresh_answers_are_reused_until_the_ttl_expires() {
        let h = harness(false, false);
        h.lookups.request(key("file:///a", SRC));
        h.wait();
        h.clock.advance(SUCCESS_TTL - Duration::from_secs(1));
        h.lookups.request(key("file:///a", SRC));
        h.sync();
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
        h.clock.advance(Duration::from_secs(1));
        h.lookups.request(key("file:///a", SRC));
        assert!(!h.wait());
        assert_eq!(h.calls.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn failures_back_off_exponentially_and_recover() {
        let h = harness(false, false);
        h.fail.store(true, Ordering::SeqCst);
        let k = || key("file:///a", SRC);
        h.lookups.request(k());
        h.wait();
        let mut expected = 1;
        for failures in 1..=4u32 {
            let delay = backoff_delay(failures);
            h.clock.advance(delay - Duration::from_secs(1));
            h.lookups.request(k());
            h.sync();
            expected += 1;
            assert_eq!(h.calls.load(Ordering::SeqCst), expected, "early {failures}");
            h.clock.advance(Duration::from_secs(1));
            h.lookups.request(k());
            h.wait();
            expected += 1;
            assert_eq!(h.calls.load(Ordering::SeqCst), expected, "due {failures}");
        }
        h.fail.store(false, Ordering::SeqCst);
        h.clock.advance(BACKOFF_CAP);
        h.lookups.request(k());
        assert!(h.wait());
        h.clock.advance(SUCCESS_TTL - Duration::from_secs(1));
        h.lookups.request(k());
        h.sync();
        assert_eq!(h.calls.load(Ordering::SeqCst), expected + 2);
    }

    #[test]
    fn backoff_delay_doubles_and_is_capped() {
        let secs = |n| Duration::from_secs(n);
        assert_eq!(backoff_delay(0), secs(5));
        assert_eq!(backoff_delay(1), secs(5));
        assert_eq!(backoff_delay(2), secs(10));
        assert_eq!(backoff_delay(3), secs(20));
        assert_eq!(backoff_delay(6), secs(160));
        assert_eq!(backoff_delay(7), secs(300));
        assert_eq!(backoff_delay(u32::MAX), secs(300));
    }

    #[test]
    fn a_failure_keeps_the_last_good_stats() {
        let h = harness(false, false);
        h.lookups.request(key("file:///a", SRC));
        h.wait();
        h.fail.store(true, Ordering::SeqCst);
        h.clock.advance(SUCCESS_TTL);
        let served = h.lookups.request(key("file:///a", SRC));
        assert!(!served.is_empty());
        assert!(!h.wait());
        assert!(!h.lookups.request(key("file:///a", SRC)).is_empty());
    }

    #[test]
    fn a_panicking_source_is_a_failure_and_the_worker_survives() {
        let h = harness(false, true);
        h.lookups.request(key("file:///a", SRC));
        assert!(!h.wait());
        h.lookups.request(key("file:///b", SRC));
        assert!(!h.wait());
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn unchanged_answers_are_reported_as_unchanged() {
        let h = harness(false, false);
        h.lookups.request(key("file:///a", SRC));
        assert!(h.wait());
        h.clock.advance(SUCCESS_TTL);
        h.lookups.request(key("file:///a", SRC));
        assert!(!h.wait());
    }

    #[test]
    fn superseded_queued_lookups_for_one_document_are_dropped() {
        let h = harness(true, false);
        h.lookups.request(key("file:///a", SRC));
        h.entered.recv().unwrap();
        let one = "range(0, 3).map(g).return";
        let two = "range(0, 3).map(g).filter(h).return";
        h.lookups.request(key("file:///a", one));
        h.lookups.request(key("file:///a", two));
        for _ in 0..3 {
            h.gate.send(()).unwrap();
        }
        h.wait();
        h.wait();
        assert_eq!(h.calls.load(Ordering::SeqCst), 2);
        h.sync();
        assert_eq!(h.calls.load(Ordering::SeqCst), 3);
    }
}
