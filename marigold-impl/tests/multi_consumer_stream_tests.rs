//! Tests for `MultiConsumerStream`.
//!
//! Scope note: PR #221 introduces inline unit tests in
//! `marigold-impl/src/multi_consumer_stream.rs` that cover (a) the empty-source
//! case, (b) `run()` with no receivers, and (c) `RunFutureAsStream`. To avoid
//! duplicate coverage, this integration suite focuses on cases #221 does not
//! exercise:
//!
//!  * a single-consumer happy path that exercises ordering end-to-end;
//!  * a two-consumer broadcast that verifies both receivers see every item in
//!    order (drained concurrently to avoid the BUFFER_SIZE=1 stall);
//!  * a slow-consumer fan-out test that checks the producer waits for the
//!    slowest receiver — i.e. no item is dropped under backpressure.
//!
//! When #221 lands, the empty-source / RunFutureAsStream cases here have been
//! intentionally removed in favour of the inline versions in #221.

use futures::StreamExt;
use marigold_impl::multi_consumer_stream::MultiConsumerStream;

/// Single consumer receives every item in source order.
#[tokio::test]
async fn single_consumer_receives_all_items_in_order() {
    let source = futures::stream::iter(vec![1u32, 2, 3, 4, 5]);
    let mut mcs = MultiConsumerStream::new(source);
    let rx = mcs.get();

    mcs.run().await;

    let received: Vec<u32> = rx.collect().await;
    assert_eq!(received, vec![1, 2, 3, 4, 5]);
}

/// Two consumers both receive a copy of every item (broadcast behaviour),
/// in the original source order.
///
/// Both receivers are drained concurrently via `tokio::join!`: the channel
/// buffer is size 1, so the producer task will stall if one receiver falls
/// behind while we drain the other one sequentially.
#[tokio::test]
async fn two_consumers_both_receive_all_items_in_order() {
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

/// Slow-consumer fan-out: a deliberately slow receiver must not cause items
/// to be dropped for any consumer, and the fast receiver still observes every
/// item in source order. This pins down the (currently desired) backpressure
/// semantic: the producer waits for the slowest receiver rather than dropping
/// values.
///
/// We pace the slow receiver with `tokio::time::sleep` between reads. With
/// BUFFER_SIZE=1 in the producer-to-consumer channel, this exercises the
/// fan-out fairness path: the producer's `feed` future for the slow sender
/// stays pending until the slow side polls, so no item is silently lost.
#[tokio::test]
async fn slow_consumer_does_not_lose_items_for_anyone() {
    use std::time::Duration;

    let source = futures::stream::iter(0u32..16);
    let mut mcs = MultiConsumerStream::new(source);
    let mut fast = mcs.get();
    let mut slow = mcs.get();

    mcs.run().await;

    let (fast_buf, slow_buf) = tokio::join!(
        async {
            let mut buf = Vec::new();
            while let Some(v) = fast.next().await {
                buf.push(v);
            }
            buf
        },
        async {
            let mut buf = Vec::new();
            while let Some(v) = slow.next().await {
                buf.push(v);
                // pace the slow consumer; producer must wait, not drop.
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            buf
        },
    );

    let expected: Vec<u32> = (0..16).collect();
    assert_eq!(fast_buf, expected, "fast consumer lost items");
    assert_eq!(slow_buf, expected, "slow consumer lost items");
}
