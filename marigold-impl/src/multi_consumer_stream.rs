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

    // ── MultiConsumerStream ──────────────────────────────────────────────────

    /// A single consumer receives every item from the source stream, in order.
    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = futures::stream::iter(0u32..5);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();

        mcs.run().await;

        let got: Vec<u32> = rx.by_ref().collect().await;
        assert_eq!(got, vec![0, 1, 2, 3, 4]);
    }

    /// Two consumers each independently receive every item — the stream is
    /// truly broadcast, not partitioned.
    #[tokio::test]
    async fn two_consumers_each_receive_all_items() {
        let source = futures::stream::iter(0u32..4);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();

        mcs.run().await;

        let got1: Vec<u32> = rx1.by_ref().collect().await;
        let got2: Vec<u32> = rx2.by_ref().collect().await;
        assert_eq!(got1, vec![0, 1, 2, 3]);
        assert_eq!(got2, vec![0, 1, 2, 3]);
    }

    /// Three consumers work correctly — validates that the FuturesUnordered
    /// fan-out scales beyond two senders.
    #[tokio::test]
    async fn three_consumers_each_receive_all_items() {
        let source = futures::stream::iter(10u32..14);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();
        let mut rx3 = mcs.get();

        mcs.run().await;

        let got1: Vec<u32> = rx1.by_ref().collect().await;
        let got2: Vec<u32> = rx2.by_ref().collect().await;
        let got3: Vec<u32> = rx3.by_ref().collect().await;
        assert_eq!(got1, vec![10, 11, 12, 13]);
        assert_eq!(got2, vec![10, 11, 12, 13]);
        assert_eq!(got3, vec![10, 11, 12, 13]);
    }

    /// An empty source produces no items on any consumer channel; both
    /// channels close cleanly (collect() terminates).
    #[tokio::test]
    async fn empty_source_closes_consumer_channels() {
        let source = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();

        mcs.run().await;

        let got1: Vec<u32> = rx1.by_ref().collect().await;
        let got2: Vec<u32> = rx2.by_ref().collect().await;
        assert!(got1.is_empty());
        assert!(got2.is_empty());
    }

    /// A single-element source delivers that element to every consumer.
    #[tokio::test]
    async fn single_item_source_delivered_to_all_consumers() {
        let source = futures::stream::iter(std::iter::once(42u32));
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();

        mcs.run().await;

        let got1: Vec<u32> = rx1.by_ref().collect().await;
        let got2: Vec<u32> = rx2.by_ref().collect().await;
        assert_eq!(got1, vec![42]);
        assert_eq!(got2, vec![42]);
    }

    /// Items are delivered in source order to every consumer. The channel is
    /// bounded (BUFFER_SIZE = 1) but ordering must still be preserved because
    /// the inner loop feeds each item synchronously before advancing.
    #[tokio::test]
    async fn items_delivered_in_order() {
        let items: Vec<u32> = (0..20).collect();
        let source = futures::stream::iter(items.clone());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();

        mcs.run().await;

        let got1: Vec<u32> = rx1.by_ref().collect().await;
        let got2: Vec<u32> = rx2.by_ref().collect().await;
        assert_eq!(got1, items);
        assert_eq!(got2, items);
    }

    // ── RunFutureAsStream ────────────────────────────────────────────────────

    /// RunFutureAsStream wraps a future and yields no items — it is used
    /// purely to drive a side-effecting future as a stream combinator.
    /// Once the future resolves, poll_next must return Poll::Ready(None).
    #[tokio::test]
    async fn run_future_as_stream_yields_no_items() {
        let fut = Box::pin(async { /* side effect */ });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    /// size_hint always reports (0, None) because the stream never yields items.
    #[tokio::test]
    async fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { 99u32 });
        let stream: RunFutureAsStream<u32, u32, _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
