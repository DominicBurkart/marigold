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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------
//
// These tests require the `tokio` feature because `MultiConsumerStream::run()`
// spawns a background task when `tokio` (or `async-std`) is enabled. Without a
// spawn, the inline fan-out loop must complete before the receivers are polled,
// which deadlocks once the channel buffer fills up with more than one item.
// Gating on `feature = "tokio"` ensures the spawned background task and the
// collecting futures run concurrently on the tokio multi-thread scheduler.
#[cfg(all(test, feature = "tokio"))]
mod tests {
    use super::MultiConsumerStream;
    use futures::stream::StreamExt;

    /// A single consumer receives all items from the source stream, in order.
    #[tokio::test]
    async fn single_consumer_receives_all() {
        let source = futures::stream::iter(vec![1i32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let receiver = mcs.get();

        mcs.run().await;

        let items: Vec<i32> = receiver.collect().await;
        assert_eq!(items, vec![1, 2, 3]);
    }

    /// Two independent consumers both receive every item from the source stream.
    /// This validates the broadcast / fan-out semantics of `MultiConsumerStream`.
    #[tokio::test]
    async fn two_consumers_both_receive_all() {
        let source = futures::stream::iter(vec![1i32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let receiver_a = mcs.get();
        let receiver_b = mcs.get();

        // `run()` spawns the fan-out background task (tokio feature).
        mcs.run().await;

        // Collect from both receivers concurrently so neither blocks the other.
        let (items_a, items_b) = tokio::join!(
            receiver_a.collect::<Vec<i32>>(),
            receiver_b.collect::<Vec<i32>>()
        );

        assert_eq!(items_a, vec![1, 2, 3]);
        assert_eq!(items_b, vec![1, 2, 3]);
    }

    /// A `MultiConsumerStream` with no consumers completes without error.
    #[tokio::test]
    async fn no_consumers_completes_cleanly() {
        let source = futures::stream::iter(vec![1i32, 2, 3]);
        let mcs = MultiConsumerStream::new(source);
        // Should not panic or deadlock.
        mcs.run().await;
    }

    /// A `MultiConsumerStream` over an empty stream delivers no items and
    /// closes receivers cleanly.
    #[tokio::test]
    async fn empty_stream_single_consumer() {
        let source = futures::stream::iter(Vec::<i32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let receiver = mcs.get();

        mcs.run().await;

        let items: Vec<i32> = receiver.collect().await;
        assert!(
            items.is_empty(),
            "Expected no items from empty stream, got: {:?}",
            items
        );
    }

    /// A single consumer receives all items from a larger stream, verifying
    /// that the channel buffer (size 1) does not cause items to be dropped.
    #[tokio::test]
    async fn single_consumer_larger_stream() {
        let expected: Vec<i32> = (0..20).collect();
        let source = futures::stream::iter(expected.clone());
        let mut mcs = MultiConsumerStream::new(source);
        let receiver = mcs.get();

        mcs.run().await;

        let items: Vec<i32> = receiver.collect().await;
        assert_eq!(items, expected);
    }
}
