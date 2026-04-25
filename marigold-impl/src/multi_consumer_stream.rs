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
    use super::*;
    use futures::stream::StreamExt;

    /// One consumer should observe every item produced by the inner stream.
    #[tokio::test]
    async fn multi_consumer_single_consumer_passthrough() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..5_u32));
        let consumer = mcs.get();
        tokio::spawn(mcs.run());
        assert_eq!(consumer.collect::<Vec<_>>().await, vec![0, 1, 2, 3, 4]);
    }

    /// Every registered consumer should observe every item, in order.
    #[tokio::test]
    async fn multi_consumer_broadcast_to_three() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(10_u32..14_u32));
        let a = mcs.get();
        let b = mcs.get();
        let c = mcs.get();
        tokio::spawn(mcs.run());

        let (va, vb, vc) = tokio::join!(
            a.collect::<Vec<_>>(),
            b.collect::<Vec<_>>(),
            c.collect::<Vec<_>>(),
        );
        assert_eq!(va, vec![10, 11, 12, 13]);
        assert_eq!(vb, vec![10, 11, 12, 13]);
        assert_eq!(vc, vec![10, 11, 12, 13]);
    }

    /// With no consumers, run() should still complete cleanly.
    #[tokio::test]
    async fn multi_consumer_no_consumers_completes() {
        let mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..3_u32));
        let handle = tokio::spawn(mcs.run());
        handle.await.expect("run() should complete cleanly");
    }

    /// An empty inner stream should yield zero items to a registered consumer
    /// and still cause the consumer's stream to terminate.
    #[tokio::test]
    async fn multi_consumer_empty_inner_terminates() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(std::iter::empty::<u32>()));
        let consumer = mcs.get();
        tokio::spawn(mcs.run());
        assert_eq!(consumer.collect::<Vec<_>>().await, Vec::<u32>::new());
    }

    /// `RunFutureAsStream` adapts a Future into a Stream that yields no items but
    /// terminates after the future resolves.
    #[tokio::test]
    async fn run_future_as_stream_yields_nothing_then_terminates() {
        let fut = Box::pin(async { 42_u32 });
        let stream: RunFutureAsStream<(), u32, _> = RunFutureAsStream::new(fut);
        let collected: Vec<()> = stream.collect().await;
        assert!(collected.is_empty());
    }

    /// `size_hint` should report (0, None) — the adapter never produces items.
    #[test]
    fn run_future_as_stream_size_hint_is_zero_unbounded() {
        let fut = Box::pin(async {});
        let stream: RunFutureAsStream<(), (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
