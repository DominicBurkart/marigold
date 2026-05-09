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
    use futures::stream::{self, StreamExt};

    #[tokio::test]
    async fn test_run_future_as_stream_is_always_empty() {
        let fut = Box::pin(async { 42u32 });
        let s: RunFutureAsStream<u32, u32, _> = RunFutureAsStream::new(fut);
        let items: Vec<u32> = s.collect().await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn test_run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let s: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(s.size_hint(), (0, None));
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_empty_source_no_receivers() {
        let inner = stream::iter(Vec::<u32>::new());
        let mcs = MultiConsumerStream::new(inner);
        mcs.run().await;
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_empty_source_with_receiver() {
        let inner = stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(inner);
        let mut rx = mcs.get();
        mcs.run().await;
        assert!(rx.next().await.is_none());
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_single_item_single_receiver() {
        let inner = stream::iter(vec![42u32]);
        let mut mcs = MultiConsumerStream::new(inner);
        let mut rx = mcs.get();
        mcs.run().await;
        assert_eq!(rx.next().await, Some(42));
        assert!(rx.next().await.is_none());
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_multiple_items_single_receiver() {
        let items = vec![1u32, 2, 3, 4, 5];
        let inner = stream::iter(items.clone());
        let mut mcs = MultiConsumerStream::new(inner);
        let rx = mcs.get();
        // Spawn consumer concurrently to avoid deadlock with BUFFER_SIZE=1 in the inline path
        let consumer = tokio::spawn(async move { rx.collect::<Vec<_>>().await });
        mcs.run().await;
        let received = consumer.await.unwrap();
        assert_eq!(received, items);
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_multiple_items_two_receivers() {
        let items = vec![10u32, 20, 30];
        let inner = stream::iter(items.clone());
        let mut mcs = MultiConsumerStream::new(inner);
        let rx1 = mcs.get();
        let rx2 = mcs.get();
        let consumer1 = tokio::spawn(async move { rx1.collect::<Vec<_>>().await });
        let consumer2 = tokio::spawn(async move { rx2.collect::<Vec<_>>().await });
        mcs.run().await;
        let received1 = consumer1.await.unwrap();
        let received2 = consumer2.await.unwrap();
        assert_eq!(received1, items);
        assert_eq!(received2, items);
    }

    #[tokio::test]
    async fn test_multi_consumer_stream_no_receivers_completes() {
        let inner = stream::iter(vec![1u32, 2, 3]);
        let mcs = MultiConsumerStream::new(inner);
        mcs.run().await;
    }
}
