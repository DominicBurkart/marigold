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

#[cfg(all(test, any(feature = "tokio", feature = "async-std")))]
mod tests {
    use super::MultiConsumerStream;
    use futures::stream::StreamExt;

    /// Each consumer must observe the entire source stream in source order.
    #[tokio::test]
    async fn single_consumer_sees_full_stream_in_order() {
        let source = futures::stream::iter(0u32..16);
        let mut mcs = MultiConsumerStream::new(source);
        let consumer = mcs.get();
        mcs.run().await;

        let collected: Vec<u32> = consumer.collect().await;
        assert_eq!(collected, (0u32..16).collect::<Vec<_>>());
    }

    /// Two consumers attached to the same source must each see every item
    /// in the same order as the source emitted them.
    #[tokio::test]
    async fn multiple_consumers_each_see_full_stream_in_order() {
        let source = futures::stream::iter(0u32..32);
        let mut mcs = MultiConsumerStream::new(source);
        let a = mcs.get();
        let b = mcs.get();
        let c = mcs.get();
        mcs.run().await;

        let (va, vb, vc) = futures::join!(
            a.collect::<Vec<_>>(),
            b.collect::<Vec<_>>(),
            c.collect::<Vec<_>>(),
        );
        let expected: Vec<u32> = (0u32..32).collect();
        assert_eq!(va, expected);
        assert_eq!(vb, expected);
        assert_eq!(vc, expected);
    }

    /// An empty source stream must terminate every attached consumer
    /// without producing any items.
    #[tokio::test]
    async fn empty_source_disconnects_all_consumers() {
        let source = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let a = mcs.get();
        let b = mcs.get();
        mcs.run().await;

        let (va, vb) = futures::join!(a.collect::<Vec<_>>(), b.collect::<Vec<_>>());
        assert!(va.is_empty());
        assert!(vb.is_empty());
    }

    /// Running with no consumers attached should still drain the source
    /// without panicking.
    #[tokio::test]
    async fn run_with_no_consumers_does_not_panic() {
        let source = futures::stream::iter(0u32..8);
        let mcs = MultiConsumerStream::new(source);
        // No `.get()` calls.
        mcs.run().await;
    }

    /// Each consumer must own an independent copy of every item: dropping or
    /// fully draining one consumer must not affect the items observed by the
    /// others (modulo ordering, which is preserved per-consumer).
    #[tokio::test]
    async fn consumers_observe_independent_item_copies() {
        let source = futures::stream::iter(vec![10u64, 20, 30, 40, 50]);
        let mut mcs = MultiConsumerStream::new(source);
        let a = mcs.get();
        let b = mcs.get();
        mcs.run().await;

        let (va, vb) = futures::join!(a.collect::<Vec<_>>(), b.collect::<Vec<_>>());
        assert_eq!(va, vec![10, 20, 30, 40, 50]);
        assert_eq!(vb, vec![10, 20, 30, 40, 50]);
        // If items were moved instead of copied, only one consumer could see them.
    }
}
