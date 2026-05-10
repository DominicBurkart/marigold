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
    async fn run_future_as_stream_yields_nothing() {
        let fut = Box::pin(async { 42u32 });
        let stream = RunFutureAsStream::<u32, u32, _>::new(fut);
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream = RunFutureAsStream::<u32, (), _>::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }

    #[tokio::test]
    async fn multi_consumer_stream_no_consumers_runs_without_panic() {
        let inner = futures::stream::iter(vec![1u32, 2, 3]);
        let mcs = MultiConsumerStream::new(inner);
        mcs.run().await;
    }

    #[tokio::test]
    async fn multi_consumer_stream_empty_inner_stream() {
        let inner = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(inner);
        let rx = mcs.get();
        // Spawn consumer before run() so the channel is drained concurrently
        let h = tokio::spawn(async move { rx.collect::<Vec<_>>().await });
        mcs.run().await;
        let items = h.await.unwrap();
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn multi_consumer_stream_one_consumer_receives_all_items() {
        let inner = futures::stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(inner);
        let rx = mcs.get();
        // Spawn consumer before run() to avoid deadlock with BUFFER_SIZE=1
        let h = tokio::spawn(async move { rx.collect::<Vec<_>>().await });
        mcs.run().await;
        let items = h.await.unwrap();
        assert_eq!(items, vec![1u32, 2, 3]);
    }

    #[tokio::test]
    async fn multi_consumer_stream_two_consumers_both_receive_all_items() {
        let inner = futures::stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(inner);
        let rx1 = mcs.get();
        let rx2 = mcs.get();
        // Spawn both consumers before run() to avoid deadlock with BUFFER_SIZE=1
        let h1 = tokio::spawn(async move { rx1.collect::<Vec<_>>().await });
        let h2 = tokio::spawn(async move { rx2.collect::<Vec<_>>().await });
        mcs.run().await;
        let items1 = h1.await.unwrap();
        let items2 = h2.await.unwrap();
        assert_eq!(items1, vec![1u32, 2, 3]);
        assert_eq!(items2, vec![1u32, 2, 3]);
    }
}
