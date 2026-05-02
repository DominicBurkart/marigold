//! Tests for `MultiConsumerStream` and `RunFutureAsStream`.
//!
//! These types implement the fan-out / driver primitives used to compile
//! Marigold programs that bind a stream to a variable and consume it from
//! multiple downstream pipelines (see `marigold-grammar/tests/programs/multi_consumer.marigold`).
//! They are not exercised by any direct unit/integration test in the
//! workspace; they are only covered transitively through codegen, which
//! makes regressions hard to localize. The tests below pin down the
//! observable invariants of these primitives directly.
//!
//! Invariants exercised:
//! 1. Every registered consumer observes the full input sequence in order.
//! 2. After the upstream stream is exhausted, every receiver yields `None`
//!    (senders are disconnected).
//! 3. `run()` terminates cleanly when no consumers were registered.
//! 4. A single consumer is equivalent to the underlying stream.
//! 5. `RunFutureAsStream` yields zero items and reports `None` once the
//!    wrapped future resolves; its `size_hint` is `(0, None)`.

use core::pin::Pin;
use core::task::{Context, Poll};
use futures::stream::{Stream, StreamExt};
use futures::task::noop_waker_ref;
use marigold_impl::multi_consumer_stream::{MultiConsumerStream, RunFutureAsStream};

#[tokio::test]
async fn fans_out_full_sequence_to_each_consumer() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(0i32..5));

    let r1 = mcs.get();
    let r2 = mcs.get();
    let r3 = mcs.get();

    // Drive collection concurrently with run() so the spawned task and the
    // receivers are scheduled together. (run() spawns when feature=tokio is
    // enabled, but we tolerate either path by joining all four futures.)
    let (_, c1, c2, c3) = tokio::join!(
        mcs.run(),
        r1.collect::<Vec<_>>(),
        r2.collect::<Vec<_>>(),
        r3.collect::<Vec<_>>(),
    );

    let expected: Vec<i32> = (0..5).collect();
    assert_eq!(
        c1, expected,
        "consumer 1 must see the full sequence in order"
    );
    assert_eq!(
        c2, expected,
        "consumer 2 must see the full sequence in order"
    );
    assert_eq!(
        c3, expected,
        "consumer 3 must see the full sequence in order"
    );
}

#[tokio::test]
async fn single_consumer_matches_underlying_stream() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![10u8, 20, 30, 40]));
    let r1 = mcs.get();

    let (_, items) = tokio::join!(mcs.run(), r1.collect::<Vec<_>>());
    assert_eq!(items, vec![10, 20, 30, 40]);
}

#[tokio::test]
async fn empty_upstream_yields_no_items_to_consumers() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<i32>::new()));
    let r1 = mcs.get();
    let r2 = mcs.get();

    let (_, c1, c2) = tokio::join!(mcs.run(), r1.collect::<Vec<_>>(), r2.collect::<Vec<_>>());

    assert!(c1.is_empty(), "consumer 1 must receive no items");
    assert!(c2.is_empty(), "consumer 2 must receive no items");
}

#[tokio::test]
async fn run_with_zero_consumers_terminates() {
    // No call to get(): the senders vector is empty, but run() must still
    // complete the upstream and not panic on shrink_to_fit/iter_mut.
    let mcs = MultiConsumerStream::new(futures::stream::iter(0..3));
    // Wrap in a timeout: a deadlock here would otherwise hang the suite.
    let res = tokio::time::timeout(std::time::Duration::from_secs(5), mcs.run()).await;
    assert!(res.is_ok(), "run() with zero consumers must terminate");
}

#[tokio::test]
async fn many_consumers_each_get_full_sequence() {
    // Probe the fan-out path with more receivers than the BUFFER_SIZE so the
    // FuturesUnordered feed-loop must actually wait on every sender.
    let n_consumers = 16;
    let n_items: i32 = 50;
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(0..n_items));

    let receivers: Vec<_> = (0..n_consumers).map(|_| mcs.get()).collect();

    let collect_all =
        futures::future::join_all(receivers.into_iter().map(|r| r.collect::<Vec<_>>()));

    let (_, results) = tokio::join!(mcs.run(), collect_all);

    let expected: Vec<i32> = (0..n_items).collect();
    for (i, got) in results.iter().enumerate() {
        assert_eq!(got, &expected, "consumer {i} did not see the full sequence");
    }
}

#[tokio::test]
async fn item_counts_are_preserved_under_fan_out() {
    // Property check: total received == n_consumers * n_items.
    let n_consumers = 4;
    let items: Vec<u32> = (0..1_000u32).collect();
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(items.clone()));
    let receivers: Vec<_> = (0..n_consumers).map(|_| mcs.get()).collect();

    let counts = futures::future::join_all(
        receivers
            .into_iter()
            .map(|r| async move { r.fold(0usize, |acc, _| async move { acc + 1 }).await }),
    );

    let (_, counts) = tokio::join!(mcs.run(), counts);
    let total: usize = counts.iter().sum();
    assert_eq!(total, items.len() * n_consumers);
    for c in counts {
        assert_eq!(c, items.len(), "every consumer must see exactly n items");
    }
}

#[tokio::test]
async fn run_future_as_stream_yields_no_items_when_future_ready() {
    // Future is immediately ready: the stream must yield Ready(None) on the
    // first poll and never produce an item.
    let fut = Box::pin(async {});
    let mut s: RunFutureAsStream<i32, (), _> = RunFutureAsStream::new(fut);

    assert_eq!(s.size_hint(), (0, None));

    let collected: Vec<i32> = (&mut s).collect().await;
    assert!(
        collected.is_empty(),
        "RunFutureAsStream must produce zero items"
    );
}

#[test]
fn run_future_as_stream_pending_until_inner_future_ready() {
    // Driving a pending future through poll_next must report Pending; once
    // the wrapped future resolves, poll_next must return Ready(None) and
    // the stream must remain terminated.
    let (tx, rx) = futures::channel::oneshot::channel::<()>();
    let fut = Box::pin(async move {
        let _ = rx.await;
    });
    let mut s: RunFutureAsStream<i32, (), _> = RunFutureAsStream::new(fut);

    let waker = noop_waker_ref();
    let mut cx = Context::from_waker(waker);

    match Pin::new(&mut s).poll_next(&mut cx) {
        Poll::Pending => {}
        other => panic!("expected Pending before inner future resolves, got {other:?}"),
    }

    // Resolve the inner future.
    tx.send(()).expect("oneshot receiver still alive");

    match Pin::new(&mut s).poll_next(&mut cx) {
        Poll::Ready(None) => {}
        other => panic!("expected Ready(None) once inner future resolves, got {other:?}"),
    }
}
