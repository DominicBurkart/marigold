use futures::{Stream, StreamExt};
use marigold_impl::multi_consumer_stream::{MultiConsumerStream, RunFutureAsStream};

#[tokio::test]
async fn run_future_as_stream_produces_no_items() {
    let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(Box::pin(async { () }));
    let results: Vec<u32> = stream.collect().await;
    assert!(results.is_empty());
}

#[test]
fn run_future_as_stream_size_hint() {
    let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(Box::pin(async { () }));
    assert_eq!(stream.size_hint(), (0, None));
}

#[tokio::test]
async fn multi_consumer_empty_stream_no_consumers() {
    let mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<u32>::new()));
    mcs.run().await;
}

#[tokio::test]
async fn multi_consumer_empty_stream_with_consumer() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<u32>::new()));
    let rx = mcs.get();
    mcs.run().await;
    let results: Vec<u32> = rx.collect().await;
    assert!(results.is_empty());
}

#[tokio::test]
async fn multi_consumer_single_consumer_gets_all_items() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![1u32, 2, 3]));
    let rx = mcs.get();
    // Use tokio::spawn so the run loop is concurrent with the collect, avoiding
    // the deadlock that would occur when BUFFER_SIZE=1 without a runtime spawn.
    tokio::spawn(mcs.run());
    let results: Vec<u32> = rx.collect().await;
    assert_eq!(results, vec![1, 2, 3]);
}

#[tokio::test]
async fn multi_consumer_two_consumers_each_get_all_items() {
    let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![10u32, 20, 30]));
    let rx1 = mcs.get();
    let rx2 = mcs.get();
    tokio::spawn(mcs.run());
    let (r1, r2) = tokio::join!(rx1.collect::<Vec<_>>(), rx2.collect::<Vec<_>>());
    assert_eq!(r1, vec![10, 20, 30]);
    assert_eq!(r2, vec![10, 20, 30]);
}
