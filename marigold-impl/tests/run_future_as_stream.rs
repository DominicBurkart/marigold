//! Tests for `marigold_impl::multi_consumer_stream::RunFutureAsStream`.
//!
//! `RunFutureAsStream` is a small adapter that wraps a `Future<Output = O>` in
//! a `Stream<Item = T>`. It never produces items: its only purpose is to drive
//! a future to completion inside a stream-shaped pipeline. The invariants are:
//!
//! 1. While the wrapped future is `Pending`, `poll_next` returns `Pending`.
//! 2. Once the wrapped future completes, `poll_next` returns `Ready(None)`
//!    (i.e. the stream terminates immediately, without ever yielding an item).
//! 3. Driving the stream to completion drives the wrapped future to
//!    completion (observable via side effects).
//! 4. `size_hint` is always `(0, None)`.
//! 5. The adapter is generic over the future's `Output` type; the output value
//!    is discarded by design.
//!
//! These tests are deterministic: they use `noop_waker` and manual polling
//! rather than timers or task spawning, so they have no flakiness surface.

use futures::stream::{Stream, StreamExt};
use futures::task::{noop_waker, Context, Poll};
use marigold_impl::multi_consumer_stream::RunFutureAsStream;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// A future that returns `Pending` for its first `pending_polls` polls and
/// then returns `Ready(value)` on subsequent polls. Used to exercise the
/// `Pending -> Ready(None)` transition without timers.
struct PendingThenReady<O> {
    pending_polls: usize,
    polled: usize,
    value: Option<O>,
}

impl<O: Unpin> PendingThenReady<O> {
    fn new(pending_polls: usize, value: O) -> Self {
        Self {
            pending_polls,
            polled: 0,
            value: Some(value),
        }
    }
}

impl<O: Unpin> Future for PendingThenReady<O> {
    type Output = O;

    fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.polled < self.pending_polls {
            self.polled += 1;
            Poll::Pending
        } else {
            Poll::Ready(self.value.take().expect("polled after completion"))
        }
    }
}

/// Invariants 1, 2, 4 — pending future yields Pending; once the future
/// completes the stream yields exactly `Ready(None)` and `size_hint` is
/// `(0, None)` throughout.
#[test]
fn pending_then_ready_transitions_to_none_without_yielding_items() {
    let fut = PendingThenReady::new(/* pending_polls = */ 3, /* value = */ 42_i32);
    let mut stream: RunFutureAsStream<(), i32, _> = RunFutureAsStream::new(Box::pin(fut));

    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);

    // Invariant 4: size_hint is (0, None) before any poll.
    assert_eq!(stream.size_hint(), (0, None));

    // Invariant 1: while the inner future is pending, poll_next is Pending.
    for i in 0..3 {
        let poll = Pin::new(&mut stream).poll_next(&mut cx);
        assert!(
            matches!(poll, Poll::Pending),
            "poll #{i} expected Pending while inner future is Pending, got Ready"
        );
        // size_hint stays (0, None) across polls.
        assert_eq!(stream.size_hint(), (0, None));
    }

    // Invariant 2: when the inner future is ready, the stream yields Ready(None)
    // — note: NOT Ready(Some(_)). The stream never produces an item.
    let poll = Pin::new(&mut stream).poll_next(&mut cx);
    assert!(
        matches!(poll, Poll::Ready(None)),
        "expected Ready(None) once inner future completed, got {poll:?}",
    );

    // Invariant 4 again: size_hint remains (0, None) after termination.
    assert_eq!(stream.size_hint(), (0, None));
}

/// Invariant 2 — an immediately-ready future causes the stream to terminate
/// on the very first poll with `Ready(None)`.
#[test]
fn immediately_ready_future_yields_none_on_first_poll() {
    let fut = async { 7_u64 };
    let mut stream: RunFutureAsStream<(), u64, _> = RunFutureAsStream::new(Box::pin(fut));

    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);

    let poll = Pin::new(&mut stream).poll_next(&mut cx);
    assert!(
        matches!(poll, Poll::Ready(None)),
        "expected Ready(None) on first poll of a ready future, got {poll:?}",
    );
}

/// Invariant 3 — fully draining the stream actually drives the inner future
/// to completion (observable side effect on a shared counter).
#[tokio::test]
async fn collecting_stream_drives_future_to_completion() {
    let counter = Arc::new(AtomicUsize::new(0));
    let counter_inner = counter.clone();
    let fut = async move {
        counter_inner.fetch_add(1, Ordering::SeqCst);
        // The output is intentionally non-unit to verify the adapter is
        // generic over Output.
        "done"
    };

    let stream: RunFutureAsStream<(), &'static str, _> = RunFutureAsStream::new(Box::pin(fut));
    let collected: Vec<()> = stream.collect().await;

    // The stream itself yields no items.
    assert!(
        collected.is_empty(),
        "RunFutureAsStream must never yield items; got {} items",
        collected.len()
    );
    // But the wrapped future ran exactly once.
    assert_eq!(counter.load(Ordering::SeqCst), 1);
}

/// Invariant 3 — even when the inner future stays pending across multiple
/// polls, the side effect inside it is observed (i.e. the future is being
/// polled by the stream, not bypassed).
#[test]
fn polling_runs_the_inner_future_each_poll() {
    let counter = Arc::new(AtomicUsize::new(0));
    let counter_inner = counter.clone();

    // Future increments the counter each poll, returning Pending the first
    // 4 polls and Ready on the 5th.
    struct CountingFuture {
        counter: Arc<AtomicUsize>,
        budget: usize,
    }
    impl Future for CountingFuture {
        type Output = ();
        fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<()> {
            self.counter.fetch_add(1, Ordering::SeqCst);
            if self.budget == 0 {
                Poll::Ready(())
            } else {
                self.budget -= 1;
                Poll::Pending
            }
        }
    }

    let fut = CountingFuture {
        counter: counter_inner,
        budget: 4,
    };
    let mut stream: RunFutureAsStream<(), (), _> = RunFutureAsStream::new(Box::pin(fut));

    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);

    // 4 pending polls then 1 ready poll.
    for _ in 0..4 {
        assert!(matches!(
            Pin::new(&mut stream).poll_next(&mut cx),
            Poll::Pending
        ));
    }
    assert!(matches!(
        Pin::new(&mut stream).poll_next(&mut cx),
        Poll::Ready(None)
    ));
    // Each of the 5 stream polls must have polled the inner future exactly once.
    assert_eq!(counter.load(Ordering::SeqCst), 5);
}

/// Invariant 5 — the adapter accepts a future with a non-unit Output and
/// discards it. (Compile-time assurance plus a runtime smoke check.)
#[tokio::test]
async fn discards_non_unit_output() {
    // The Output type here is a heap-allocated String, distinct from the
    // stream Item type `()`. Compilation alone is meaningful, but we also
    // assert the stream produces no items at runtime.
    let fut = async { String::from("ignored") };
    let stream: RunFutureAsStream<(), String, _> = RunFutureAsStream::new(Box::pin(fut));
    let items: Vec<()> = stream.collect().await;
    assert!(items.is_empty());
}
