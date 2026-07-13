//! Deadlock-prevention regression tests for marigold's fan-out machinery.
//!
//! Marigold's concurrency model (described in "Preventing Deadlocks in Marigold",
//! <https://dominic.computer/blog/2022/preventing_deadlocks_in_marigold>) guarantees
//! deadlock freedom structurally rather than through runtime detection:
//!
//! 1. Every stream in a compiled program — consumers and `MultiConsumerStream`
//!    runners alike — is merged into a single `futures::stream::select_all` and
//!    polled concurrently, so no stream can starve another by monopolizing the
//!    driver. (When a runtime feature is enabled the runner additionally spawns,
//!    but the select_all wiring alone must be sufficient: these tests pass with
//!    no runtime feature enabled.)
//! 2. Fan-out channels are bounded, and the producer feeds every consumer channel
//!    concurrently via `FuturesUnordered`, so a slow, stalled, or dropped consumer
//!    only ever exerts backpressure on the pipeline; it can never wedge it
//!    permanently.
//!
//! Each test drives many more items than the internal channel buffers hold (the
//! fan-out buffer is a single item), so any regression to sequential sends or
//! sequential stream launching deadlocks immediately. A timeout converts such a
//! deadlock into a test failure instead of a hung test runner.

use std::pin::Pin;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use futures::stream::{Stream, StreamExt};
use marigold_impl::multi_consumer_stream::{MultiConsumerStream, RunFutureAsStream};

const DEADLOCK_TIMEOUT: Duration = Duration::from_secs(30);

/// The dynamically-typed stream collection compiled marigold programs pass to
/// `select_all`, with items tagged by consumer index.
type TaggedStreams = Vec<Pin<Box<dyn Stream<Item = (usize, u32)>>>>;

async fn assert_completes<F: std::future::Future>(f: F) -> F::Output {
    tokio::time::timeout(DEADLOCK_TIMEOUT, f)
        .await
        .expect("pipeline did not terminate: deadlock-freedom invariant violated")
}

/// Replicates the exact wiring of compiled marigold programs: the consumers and
/// the fan-out runner are merged into one `select_all` and polled together. With
/// 1,000 items against a 1-item channel buffer, this deadlocks unless the
/// select_all keeps every stream live.
#[tokio::test]
async fn fan_out_via_select_all_terminates_with_more_items_than_buffer() {
    assert_completes(async {
        const N: u32 = 1_000;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..N));
        let c0 = mcs.get().map(|v| (0usize, v));
        let c1 = mcs.get().map(|v| (1usize, v));
        let runner = RunFutureAsStream::<(usize, u32), _, _>::new(Box::pin(mcs.run()));

        let streams: TaggedStreams = vec![Box::pin(c0), Box::pin(c1), Box::pin(runner)];
        let merged = futures::stream::select_all(streams)
            .collect::<Vec<_>>()
            .await;

        let mut per_consumer = [Vec::new(), Vec::new()];
        for (tag, v) in merged {
            per_consumer[tag].push(v);
        }
        for received in per_consumer {
            assert_eq!(received, (0..N).collect::<Vec<_>>());
        }
    })
    .await;
}

/// Same wiring on a multi-threaded runtime: deadlock freedom must not depend on
/// the runtime flavor.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fan_out_via_select_all_terminates_on_multi_thread_runtime() {
    assert_completes(async {
        const N: u32 = 1_000;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..N));
        let c0 = mcs.get().map(|v| (0usize, v));
        let c1 = mcs.get().map(|v| (1usize, v));
        let runner = RunFutureAsStream::<(usize, u32), _, _>::new(Box::pin(mcs.run()));

        let streams: TaggedStreams = vec![Box::pin(c0), Box::pin(c1), Box::pin(runner)];
        let merged = futures::stream::select_all(streams)
            .collect::<Vec<_>>()
            .await;
        assert_eq!(merged.len(), 2 * N as usize);
    })
    .await;
}

/// Diamond topology: a fan-out stream feeding another fan-out stream (the shape
/// generated for `b = a.map(f)` following `a = range(...)`), with consumers
/// hanging off both levels. Both runners and all consumers share one select_all.
#[tokio::test]
async fn chained_fan_out_diamond_terminates() {
    assert_completes(async {
        const N: u32 = 500;
        let mut digits = MultiConsumerStream::new(futures::stream::iter(0..N));
        let direct = digits.get().map(|v| (0usize, v));

        let mut doubled = MultiConsumerStream::new(digits.get().map(|v| v * 2));
        let d1 = doubled.get().map(|v| (1usize, v));
        let d2 = doubled.get().map(|v| (2usize, v));

        let digits_runner = RunFutureAsStream::<(usize, u32), _, _>::new(Box::pin(digits.run()));
        let doubled_runner = RunFutureAsStream::<(usize, u32), _, _>::new(Box::pin(doubled.run()));

        let streams: TaggedStreams = vec![
            Box::pin(direct),
            Box::pin(d1),
            Box::pin(d2),
            Box::pin(digits_runner),
            Box::pin(doubled_runner),
        ];
        let merged = futures::stream::select_all(streams)
            .collect::<Vec<_>>()
            .await;

        let mut per_consumer = [Vec::new(), Vec::new(), Vec::new()];
        for (tag, v) in merged {
            per_consumer[tag].push(v);
        }
        assert_eq!(per_consumer[0], (0..N).collect::<Vec<_>>());
        let expected_doubled = (0..N).map(|v| v * 2).collect::<Vec<_>>();
        assert_eq!(per_consumer[1], expected_doubled);
        assert_eq!(per_consumer[2], expected_doubled);
    })
    .await;
}

/// Deadlock freedom comes from backpressure, not from unbounded buffering: while
/// a consumer stalls, the producer must stop within the (small, bounded) channel
/// capacity instead of buffering the rest of the stream — and must resume cleanly
/// once the consumer polls again.
#[tokio::test]
async fn bounded_channels_backpressure_producer_without_deadlock() {
    assert_completes(async {
        const N: usize = 10_000;
        let produced = Arc::new(AtomicUsize::new(0));
        let counter = Arc::clone(&produced);
        let source = futures::stream::iter(0..N).inspect(move |_| {
            counter.fetch_add(1, Ordering::SeqCst);
        });

        let mut mcs = MultiConsumerStream::new(source);
        let mut consumer = mcs.get();
        let runner = mcs.run();

        let consume = async move {
            for expected in 0..3 {
                assert_eq!(consumer.next().await, Some(expected));
            }
            // Stall. The producer may only run ahead by the channel capacity
            // plus the in-flight item; it must not race through the stream.
            tokio::time::sleep(Duration::from_millis(100)).await;
            let ran_ahead = produced.load(Ordering::SeqCst);
            assert!(
                ran_ahead <= 16,
                "producer ran {ran_ahead} items ahead of a stalled consumer; \
                 bounded-channel backpressure is broken"
            );
            // The stall was backpressure, not deadlock: draining resumes and
            // completes the stream.
            let rest = consumer.collect::<Vec<_>>().await;
            assert_eq!(rest, (3..N).collect::<Vec<_>>());
            assert_eq!(produced.load(Ordering::SeqCst), N);
        };

        futures::join!(runner, consume);
    })
    .await;
}

/// A consumer that is not polled at all initially must only pause the pipeline
/// (backpressure on its 1-item buffer), never wedge it: once it starts polling,
/// every consumer still receives the complete stream.
#[tokio::test]
async fn unpolled_consumer_pauses_pipeline_until_polled() {
    assert_completes(async {
        const N: u32 = 1_000;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..N));
        let mut eager = mcs.get();
        let lazy = mcs.get();
        let runner = mcs.run();

        let (release_lazy, lazy_gate) = futures::channel::oneshot::channel::<()>();

        let eager_task = async move {
            let mut release_lazy = Some(release_lazy);
            let mut received = Vec::new();
            while let Some(v) = eager.next().await {
                received.push(v);
                // While `lazy` is unpolled, `eager` can only get as far as the
                // bounded channels allow before the producer stalls. Release
                // `lazy` after that point so the pipeline can finish.
                if received.len() == 2 {
                    if let Some(gate) = release_lazy.take() {
                        let _ = gate.send(());
                    }
                }
            }
            received
        };

        let lazy_task = async move {
            lazy_gate.await.expect("eager consumer dropped the gate");
            lazy.collect::<Vec<_>>().await
        };

        let (_, eager_received, lazy_received) = futures::join!(runner, eager_task, lazy_task);
        assert_eq!(eager_received, (0..N).collect::<Vec<_>>());
        assert_eq!(lazy_received, (0..N).collect::<Vec<_>>());
    })
    .await;
}

/// A consumer dropped before the pipeline runs must not wedge the producer:
/// sends to it fail immediately and the remaining consumers see the full stream.
#[tokio::test]
async fn consumer_dropped_before_run_does_not_wedge_pipeline() {
    assert_completes(async {
        const N: u32 = 1_000;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..N));
        let live = mcs.get();
        let dropped = mcs.get();
        drop(dropped);

        let (_, received) = futures::join!(mcs.run(), live.collect::<Vec<_>>());
        assert_eq!(received, (0..N).collect::<Vec<_>>());
    })
    .await;
}

/// A consumer dropped mid-stream (after consuming a few items) must not wedge
/// the pipeline for the surviving consumers.
#[tokio::test]
async fn consumer_dropped_mid_stream_does_not_wedge_pipeline() {
    assert_completes(async {
        const N: u32 = 1_000;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..N));
        let survivor = mcs.get();
        let mut quitter = mcs.get();
        let runner = mcs.run();

        let quitter_task = async move {
            for expected in 0..5 {
                assert_eq!(quitter.next().await, Some(expected));
            }
            drop(quitter);
        };

        let (_, received, ()) = futures::join!(runner, survivor.collect::<Vec<_>>(), quitter_task);
        assert_eq!(received, (0..N).collect::<Vec<_>>());
    })
    .await;
}
