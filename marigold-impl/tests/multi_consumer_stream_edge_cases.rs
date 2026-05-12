use futures::stream::StreamExt;
use marigold_impl::multi_consumer_stream::{MultiConsumerStream, RunFutureAsStream};

#[tokio::test]
async fn fans_out_every_value_to_every_consumer() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..5));
    let a = mcs.get();
    let b = mcs.get();
    let c = mcs.get();

    let runner = mcs.run();

    let (a, b, c, _) = futures::join!(
        a.collect::<Vec<_>>(),
        b.collect::<Vec<_>>(),
        c.collect::<Vec<_>>(),
        runner,
    );

    let expected: Vec<u32> = (0..5).collect();
    assert_eq!(a, expected);
    assert_eq!(b, expected);
    assert_eq!(c, expected);
}

#[tokio::test]
async fn empty_inner_yields_empty_consumer_streams() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<i32>::new()));
    let only = mcs.get();

    let (collected, _) = futures::join!(only.collect::<Vec<i32>>(), mcs.run());
    assert!(collected.is_empty());
}

#[tokio::test]
async fn no_consumers_drains_inner_stream() {
    // Even with zero consumers, run() must complete (and not deadlock) once the
    // inner stream is exhausted.
    let mcs = MultiConsumerStream::new(futures::stream::iter(0_u8..3));
    mcs.run().await;
}

#[tokio::test]
async fn run_future_as_stream_terminates_when_future_resolves() {
    let fut = Box::pin(async { 7_i32 });
    let s: RunFutureAsStream<i32, i32, _> = RunFutureAsStream::new(fut);
    // The stream itself never yields T values; it ends as soon as the wrapped
    // future resolves. Collect should produce an empty Vec.
    let collected: Vec<i32> = s.collect().await;
    assert!(collected.is_empty());
}
