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

    // --- MultiConsumerStream ---

    /// A single consumer receives every item from the source stream.
    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = futures::stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        let got = rx.collect::<Vec<_>>().await;
        assert_eq!(got, vec![1, 2, 3]);
    }

    /// Two consumers each independently receive every item (fan-out invariant).
    #[tokio::test]
    async fn two_consumers_each_receive_all_items() {
        let source = futures::stream::iter(vec![10u32, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();
        mcs.run().await;

        let got1 = rx1.collect::<Vec<_>>().await;
        let got2 = rx2.collect::<Vec<_>>().await;
        assert_eq!(got1, vec![10, 20, 30]);
        assert_eq!(got2, vec![10, 20, 30]);
    }

    /// An empty source produces no items for any consumer and terminates cleanly.
    #[tokio::test]
    async fn empty_source_terminates_consumers() {
        let source = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        let got = rx.collect::<Vec<_>>().await;
        assert!(got.is_empty());
    }

    // --- RunFutureAsStream ---

    /// RunFutureAsStream always produces zero items: it drives the future as a side
    /// effect and signals stream termination when the future resolves.
    #[tokio::test]
    async fn run_future_as_stream_emits_no_items() {
        let fut = Box::pin(async { /* side-effecting future that returns () */ });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        let items = stream.collect::<Vec<u32>>().await;
        assert!(
            items.is_empty(),
            "RunFutureAsStream should always produce an empty item sequence"
        );
    }

    /// size_hint reports (0, None) because the number of items is unknown until
    /// the future resolves (and in practice it is always zero items).
    #[test]
    fn run_future_as_stream_size_hint_is_zero_none() {
        let fut = Box::pin(async {});
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
