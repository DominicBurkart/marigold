use core::marker::PhantomData;
use core::pin::Pin;
use futures::channel::mpsc::Receiver;
use futures::channel::mpsc::Sender;
use futures::future::Future;
use futures::sink::SinkExt;
use futures::stream::FuturesUnordered;
use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::task::Context;
use futures::task::Poll;

const BUFFER_SIZE: usize = 1;

pub struct MultiConsumerStream<
    T: std::marker::Send + 'static,
    S: Stream<Item = T> + std::marker::Unpin + std::marker::Send + 'static,
> {
    inner_stream: S,
    senders: Vec<Sender<T>>,
}

impl<
        T: std::marker::Send + Copy + 'static,
        S: Stream<Item = T> + std::marker::Unpin + std::marker::Send + 'static,
    > MultiConsumerStream<T, S>
{
    pub fn new(s: S) -> Self {
        MultiConsumerStream {
            inner_stream: s,
            senders: Vec::new(),
        }
    }

    pub fn get(&mut self) -> Receiver<T> {
        let (sender, receiver) = futures::channel::mpsc::channel(BUFFER_SIZE);
        self.senders.push(sender);
        receiver
    }

    pub async fn run(mut self) {
        self.senders.shrink_to_fit();

        #[cfg(any(feature = "async-std", feature = "tokio"))]
        crate::async_runtime::spawn(async move {
            while let Some(v) = self.inner_stream.next().await {
                let mut futures = self
                    .senders
                    .iter_mut()
                    .map(|sender| sender.feed(v))
                    .collect::<FuturesUnordered<_>>();
                while let Some(_result) = futures.next().await {}
            }
            self.senders.iter_mut().for_each(|s| s.disconnect());
        });

        #[cfg(not(any(feature = "async-std", feature = "tokio")))]
        {
            while let Some(v) = self.inner_stream.next().await {
                let mut futures = self
                    .senders
                    .iter_mut()
                    .map(|sender| sender.feed(v))
                    .collect::<FuturesUnordered<_>>();
                while let Some(_result) = futures.next().await {}
            }
            self.senders.iter_mut().for_each(|s| s.disconnect());
        }
    }
}

pub struct RunFutureAsStream<T: Unpin, O, F: Future<Output = O>> {
    future: Pin<Box<F>>,
    t: PhantomData<T>,
}

impl<T: Unpin, O, F: Future<Output = O>> RunFutureAsStream<T, O, F> {
    pub fn new(f: Pin<Box<F>>) -> RunFutureAsStream<T, O, F> {
        RunFutureAsStream {
            future: f,
            t: PhantomData,
        }
    }
}

impl<T: std::marker::Send + Unpin + 'static, O, F: Future<Output = O>> Stream
    for RunFutureAsStream<T, O, F>
{
    type Item = T;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let future = &mut self.future;
        match Pin::new(future).poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(_) => Poll::Ready(None),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

// These tests use `#[tokio::test]` and depend on tokio's multi-threaded runtime to drive
// `run()` (which spawns via `crate::async_runtime::spawn`) concurrently with the receivers.
// Without the tokio (or async-std) feature the spawn call is absent and tests would deadlock.
#[cfg(all(test, any(feature = "tokio", feature = "async-std")))]
mod tests {
    use super::*;
    use futures::stream::StreamExt;

    // Helper: run the MultiConsumerStream and collect from a single receiver concurrently.
    // Without a spawning async runtime the `run()` future and the `collect()` future must
    // be polled together; otherwise `run()` blocks on a full channel while the receiver
    // is not yet being polled, causing a deadlock.
    async fn run_and_collect_one(
        source: impl Stream<Item = i32> + Unpin + Send + 'static,
    ) -> Vec<i32> {
        let mut mcs = MultiConsumerStream::new(source);
        let rx = mcs.get();
        futures::join!(mcs.run(), rx.collect::<Vec<i32>>()).1
    }

    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = futures::stream::iter(vec![1, 2, 3]);
        let result = run_and_collect_one(source).await;
        assert_eq!(result, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn two_consumers_both_receive_all_items() {
        let source = futures::stream::iter(vec![10, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);
        let rx1 = mcs.get();
        let rx2 = mcs.get();
        // run() and both collects must be driven concurrently to avoid a channel
        // backpressure deadlock (BUFFER_SIZE = 1 means run() blocks as soon as the
        // buffer fills up if the receivers are not being polled).
        let (_, r1, r2): ((), Vec<i32>, Vec<i32>) = futures::join!(
            mcs.run(),
            rx1.collect::<Vec<i32>>(),
            rx2.collect::<Vec<i32>>(),
        );
        assert_eq!(r1, vec![10, 20, 30]);
        assert_eq!(r2, vec![10, 20, 30]);
    }

    #[tokio::test]
    async fn empty_stream_produces_no_items() {
        let source = futures::stream::iter(Vec::<i32>::new());
        let result = run_and_collect_one(source).await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn single_consumer_no_items_dropped() {
        // Verifies that run() delivers all items and completes without panic with a
        // single consumer. (Note: this tests the single-consumer send path, not a
        // zero-consumer scenario. The zero-consumer case is untested here because
        // panics in a spawned task would be silently swallowed before surfacing.)
        let source = futures::stream::iter(vec![1, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let rx = mcs.get();
        let (_, items) = futures::join!(mcs.run(), rx.collect::<Vec<i32>>());
        assert_eq!(items, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn run_future_as_stream_yields_none_when_future_completes() {
        // `RunFutureAsStream<T, O, F>`: T is the stream *item* type (never actually
        // yielded — the stream always terminates with None), O is the future's output
        // type. The stream only drives the future to completion.
        let fut = Box::pin(async { 42 });
        let mut stream = RunFutureAsStream::<i32, _, _>::new(fut);
        // When the future is ready, poll_next should return Ready(None)
        let result: Vec<i32> = (&mut stream).collect().await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream = RunFutureAsStream::<i32, _, _>::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
