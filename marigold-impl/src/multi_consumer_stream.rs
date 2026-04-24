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

#[cfg(test)]
mod tests {
    use super::{MultiConsumerStream, RunFutureAsStream};
    use futures::stream::{self, StreamExt};

    /// Invariant: every registered consumer observes every value emitted by
    /// the inner stream, in the same order, and then the receiver terminates
    /// once the inner stream is exhausted.
    #[tokio::test]
    async fn every_consumer_sees_every_value_in_order() {
        let inner = stream::iter(0u32..10);
        let mut mcs = MultiConsumerStream::new(inner);

        let r1 = mcs.get();
        let r2 = mcs.get();
        let r3 = mcs.get();

        let h1 = tokio::spawn(r1.collect::<Vec<_>>());
        let h2 = tokio::spawn(r2.collect::<Vec<_>>());
        let h3 = tokio::spawn(r3.collect::<Vec<_>>());

        mcs.run().await;

        let expected: Vec<u32> = (0..10).collect();
        assert_eq!(h1.await.unwrap(), expected);
        assert_eq!(h2.await.unwrap(), expected);
        assert_eq!(h3.await.unwrap(), expected);
    }

    /// Invariant: a MultiConsumerStream with no registered consumers still
    /// drains its inner stream to completion without panicking or hanging.
    #[tokio::test]
    async fn run_with_no_consumers_does_not_hang() {
        let inner = stream::iter(0u32..5);
        let mcs = MultiConsumerStream::new(inner);

        // run spawns the drain when the tokio/async-std feature is on, so this
        // returns promptly; under the no-runtime fallback it drains in-line.
        // Either way, completion within 5s guarantees no hang.
        tokio::time::timeout(std::time::Duration::from_secs(5), mcs.run())
            .await
            .expect("run() should complete without a registered consumer");
    }

    /// Invariant: a single-consumer MultiConsumerStream behaves like a pass-
    /// through of the underlying stream (including an empty stream).
    #[tokio::test]
    async fn single_consumer_passthrough_and_empty_stream() {
        // Empty stream: receiver should immediately terminate with no items.
        let inner = stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(inner);
        let r = mcs.get();
        let h = tokio::spawn(r.collect::<Vec<_>>());
        mcs.run().await;
        assert_eq!(h.await.unwrap(), Vec::<u32>::new());

        // Non-empty stream: single consumer gets every item in order.
        let inner = stream::iter(vec![42u32, 7, 99, 0, 1]);
        let mut mcs = MultiConsumerStream::new(inner);
        let r = mcs.get();
        let h = tokio::spawn(r.collect::<Vec<_>>());
        mcs.run().await;
        assert_eq!(h.await.unwrap(), vec![42u32, 7, 99, 0, 1]);
    }

    /// Invariant: a slow consumer must not cause a fast consumer to lose
    /// values. The broadcast semantics (feed on every sender before advancing
    /// the inner stream) mean both consumers see the full sequence.
    #[tokio::test]
    async fn slow_consumer_does_not_drop_values_for_fast_consumer() {
        let inner = stream::iter(0u32..20);
        let mut mcs = MultiConsumerStream::new(inner);

        let fast = mcs.get();
        let slow = mcs.get();

        let fast_handle = tokio::spawn(fast.collect::<Vec<_>>());
        let slow_handle = tokio::spawn(async move {
            let mut out = Vec::new();
            let mut s = slow;
            while let Some(v) = s.next().await {
                // Simulate a slow consumer.
                tokio::task::yield_now().await;
                out.push(v);
            }
            out
        });

        mcs.run().await;

        let expected: Vec<u32> = (0..20).collect();
        assert_eq!(fast_handle.await.unwrap(), expected);
        assert_eq!(slow_handle.await.unwrap(), expected);
    }

    /// Invariant: `RunFutureAsStream` yields `Poll::Ready(None)` as soon as
    /// the wrapped future completes; it never produces any items of type T.
    #[tokio::test]
    async fn run_future_as_stream_terminates_when_future_completes() {
        let fut = Box::pin(async { 123u64 });
        let s: RunFutureAsStream<u32, u64, _> = RunFutureAsStream::new(fut);
        let collected: Vec<u32> = s.collect().await;
        assert!(collected.is_empty());
    }

    /// Invariant: `RunFutureAsStream::size_hint` reports no lower bound and
    /// no upper bound, matching its "adapter that yields nothing" contract.
    #[test]
    fn run_future_as_stream_size_hint_is_unbounded() {
        use futures::Stream;
        let fut = Box::pin(async {});
        let s: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(s.size_hint(), (0, None));
    }
}
