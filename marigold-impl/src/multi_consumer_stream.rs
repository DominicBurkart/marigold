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

    /// A single consumer receives all items from the inner stream.
    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![1u32, 2, 3]));
        let receiver = mcs.get();
        mcs.run().await;
        let items: Vec<u32> = receiver.collect().await;
        assert_eq!(items, vec![1, 2, 3]);
    }

    /// Two consumers each receive every item (broadcast semantics).
    #[tokio::test]
    async fn two_consumers_both_receive_all_items() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![10u32, 20, 30]));
        let r1 = mcs.get();
        let r2 = mcs.get();
        mcs.run().await;

        let (items1, items2): (Vec<u32>, Vec<u32>) =
            futures::future::join(r1.collect(), r2.collect()).await;
        assert_eq!(items1, vec![10, 20, 30]);
        assert_eq!(items2, vec![10, 20, 30]);
    }

    /// An empty inner stream terminates all consumers immediately.
    #[tokio::test]
    async fn empty_stream_terminates_consumers() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(std::iter::empty::<u32>()));
        let receiver = mcs.get();
        mcs.run().await;
        let items: Vec<u32> = receiver.collect().await;
        assert!(items.is_empty());
    }

    /// RunFutureAsStream produces no items and terminates after the future completes.
    #[tokio::test]
    async fn run_future_as_stream_yields_no_items() {
        let fut = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    /// RunFutureAsStream reports size_hint of (0, None).
    #[test]
    fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
