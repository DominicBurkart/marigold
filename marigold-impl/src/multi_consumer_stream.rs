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

    #[tokio::test]
    async fn multi_consumer_stream_no_consumers() {
        let mcs = MultiConsumerStream::new(futures::stream::iter(vec![1u32, 2, 3]));
        mcs.run().await;
    }

    #[tokio::test]
    async fn multi_consumer_stream_single_consumer_receives_all() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![1u32, 2, 3]));
        let receiver = mcs.get();
        let (_, items) = tokio::join!(mcs.run(), receiver.collect::<Vec<u32>>());
        assert_eq!(items, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn multi_consumer_stream_two_consumers_both_receive_all() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![10u32, 20, 30]));
        let receiver1 = mcs.get();
        let receiver2 = mcs.get();
        let (_, items1, items2) = tokio::join!(
            mcs.run(),
            receiver1.collect::<Vec<u32>>(),
            receiver2.collect::<Vec<u32>>(),
        );
        assert_eq!(items1, vec![10, 20, 30]);
        assert_eq!(items2, vec![10, 20, 30]);
    }

    #[tokio::test]
    async fn multi_consumer_stream_empty_stream() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<u32>::new()));
        let receiver = mcs.get();
        let (_, items) = tokio::join!(mcs.run(), receiver.collect::<Vec<u32>>());
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn multi_consumer_stream_single_item() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![42u32]));
        let receiver = mcs.get();
        let (_, items) = tokio::join!(mcs.run(), receiver.collect::<Vec<u32>>());
        assert_eq!(items, vec![42]);
    }

    #[tokio::test]
    async fn run_future_as_stream_produces_no_items() {
        let future = Box::pin(async { "side_effect_completed" });
        let stream: RunFutureAsStream<u32, &str, _> = RunFutureAsStream::new(future);
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint() {
        let future = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(future);
        assert_eq!(stream.size_hint(), (0, None));
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }
}
