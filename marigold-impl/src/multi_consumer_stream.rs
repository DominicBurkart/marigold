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

#[cfg(test)]
mod tests {
    use super::*;
    use futures::stream;
    use futures::StreamExt;

    /// A single consumer receives all items from the underlying stream.
    ///
    /// The producer (`run`) and consumer must execute concurrently because the
    /// channel's buffer is only 1 slot; `tokio::join!` interleaves both sides.
    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = stream::iter(vec![1u32, 2, 3, 4, 5]);
        let mut mcs = MultiConsumerStream::new(source);
        let receiver = mcs.get();

        let (_, collected): ((), Vec<u32>) = tokio::join!(mcs.run(), receiver.collect());

        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
    }

    /// Two consumers each receive every item (broadcast semantics).
    ///
    /// All three futures (producer + 2 consumers) must run concurrently to
    /// avoid deadlocking on the small channel buffer.
    #[tokio::test]
    async fn two_consumers_both_receive_all_items() {
        let source = stream::iter(vec![10u32, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);
        let rx1 = mcs.get();
        let rx2 = mcs.get();

        let (_, got1, got2): ((), Vec<u32>, Vec<u32>) =
            tokio::join!(mcs.run(), rx1.collect(), rx2.collect());

        assert_eq!(got1, vec![10, 20, 30]);
        assert_eq!(got2, vec![10, 20, 30]);
    }

    /// An empty source stream produces no items and closes the receiver cleanly.
    #[tokio::test]
    async fn empty_source_stream_closes_receiver() {
        let source = stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let receiver = mcs.get();

        let (_, collected): ((), Vec<u32>) = tokio::join!(mcs.run(), receiver.collect());

        assert!(
            collected.is_empty(),
            "expected no items from empty stream, got {:?}",
            collected
        );
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
