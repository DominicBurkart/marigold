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

    // ---- MultiConsumerStream ----

    /// A single consumer receives all items produced by the inner stream.
    #[tokio::test]
    async fn multi_consumer_single_consumer_receives_all_items() {
        let items = vec![1u32, 2, 3];
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(items.clone()));
        let receiver = mcs.get();
        mcs.run().await;

        let collected: Vec<u32> = receiver.collect().await;
        assert_eq!(collected, items);
    }

    /// Two consumers each independently receive every item (broadcast semantics).
    #[tokio::test]
    async fn multi_consumer_two_consumers_both_receive_all_items() {
        let items = vec![10u32, 20, 30];
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(items.clone()));
        let r1 = mcs.get();
        let r2 = mcs.get();
        mcs.run().await;

        let (c1, c2): (Vec<u32>, Vec<u32>) =
            futures::future::join(r1.collect(), r2.collect()).await;
        assert_eq!(c1, items);
        assert_eq!(c2, items);
    }

    /// An empty inner stream delivers no items and closes the receiver immediately.
    #[tokio::test]
    async fn multi_consumer_empty_stream() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<u32>::new()));
        let receiver = mcs.get();
        mcs.run().await;

        let collected: Vec<u32> = receiver.collect().await;
        assert!(collected.is_empty());
    }

    // ---- RunFutureAsStream ----

    /// RunFutureAsStream always produces zero items: it drives the future to completion
    /// and then signals end-of-stream without ever yielding an item.
    #[tokio::test]
    async fn run_future_as_stream_yields_no_items() {
        // The future does some work (here a trivial async block) but the stream
        // surface never surfaces an Item.
        let fut = Box::pin(async { 42u32 });
        let stream: RunFutureAsStream<u32, _, _> = RunFutureAsStream::new(fut);
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    /// size_hint always reports (0, None) since no items are ever produced.
    #[test]
    fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, _, _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
