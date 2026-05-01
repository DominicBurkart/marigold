//! Tests for `MultiConsumerStream` and `RunFutureAsStream`.

use futures::StreamExt;
use marigold_impl::multi_consumer_stream::{MultiConsumerStream, RunFutureAsStream};

/// Single consumer receives all items from the stream.
#[tokio::test]
async fn single_consumer_receives_all_items() {
    let source = futures::stream::iter(vec![1u32, 2, 3, 4, 5]);
    let mut mcs = MultiConsumerStream::new(source);
    let mut rx = mcs.get();

    mcs.run().await;

    let mut received = Vec::new();
    while let Some(v) = rx.next().await {
        received.push(v);
    }

    assert_eq!(received, vec![1, 2, 3, 4, 5]);
}

/// Two consumers both receive a copy of every item (broadcast behaviour).
///
/// Both receivers must be drained concurrently: the channel buffer is size 1,
/// so the sender task can stall if one receiver falls behind while we drain the
/// other one sequentially.  `tokio::join!` drives both drains in parallel.
#[tokio::test]
async fn two_consumers_both_receive_all_items() {
    let source = futures::stream::iter(vec![10u32, 20, 30]);
    let mut mcs = MultiConsumerStream::new(source);
    let mut rx1 = mcs.get();
    let mut rx2 = mcs.get();

    mcs.run().await;

    let (received1, received2) = tokio::join!(
        async {
            let mut buf = Vec::new();
            while let Some(v) = rx1.next().await {
                buf.push(v);
            }
            buf
        },
        async {
            let mut buf = Vec::new();
            while let Some(v) = rx2.next().await {
                buf.push(v);
            }
            buf
        },
    );

    assert_eq!(received1, vec![10, 20, 30]);
    assert_eq!(received2, vec![10, 20, 30]);
}

/// An empty source stream terminates cleanly — the receiver gets `None` immediately.
#[tokio::test]
async fn empty_stream_terminates_cleanly() {
    let source = futures::stream::iter(Vec::<u32>::new());
    let mut mcs = MultiConsumerStream::new(source);
    let rx = mcs.get();

    mcs.run().await;

    let received: Vec<u32> = rx.collect().await;
    assert!(received.is_empty());
}

/// `RunFutureAsStream` drives the future to completion but yields zero stream items.
#[tokio::test]
async fn run_future_as_stream_yields_no_items_but_runs_future() {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    let flag = Arc::new(AtomicBool::new(false));
    let flag_clone = flag.clone();

    let fut = Box::pin(async move {
        flag_clone.store(true, Ordering::SeqCst);
    });

    let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
    let items: Vec<u32> = stream.collect().await;

    assert!(items.is_empty(), "RunFutureAsStream should yield no items");
    assert!(flag.load(Ordering::SeqCst), "Future should have been driven to completion");
}
