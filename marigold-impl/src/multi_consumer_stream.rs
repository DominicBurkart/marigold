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

#[cfg(all(test, feature = "tokio"))]
mod tests {
    use super::*;

    /// Each registered consumer receives every item from the source stream,
    /// in source order, and the receivers terminate once the source is drained.
    #[tokio::test]
    async fn multi_consumer_fans_out_to_each_receiver() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_i32..5));
        let r1 = mcs.get();
        let r2 = mcs.get();
        let r3 = mcs.get();

        let runner = tokio::spawn(mcs.run());

        let collected: (Vec<_>, Vec<_>, Vec<_>) =
            futures::join!(r1.collect(), r2.collect(), r3.collect());

        runner.await.unwrap();

        let expected: Vec<i32> = (0..5).collect();
        assert_eq!(collected.0, expected);
        assert_eq!(collected.1, expected);
        assert_eq!(collected.2, expected);
    }

    /// run() with no registered consumers must still drain the source without
    /// blocking or panicking.
    #[tokio::test]
    async fn multi_consumer_no_receivers_drains_cleanly() {
        let mcs = MultiConsumerStream::new(futures::stream::iter(0_i32..3));
        // run() returns once the source is exhausted; no receivers registered.
        mcs.run().await;
    }

    /// An empty source stream produces empty output for every registered
    /// consumer (zero items, terminated promptly).
    #[tokio::test]
    async fn multi_consumer_empty_source_yields_empty_receivers() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<u8>::new()));
        let r1 = mcs.get();
        let r2 = mcs.get();

        let runner = tokio::spawn(mcs.run());

        let (a, b): (Vec<_>, Vec<_>) = futures::join!(r1.collect(), r2.collect());
        runner.await.unwrap();

        assert!(a.is_empty());
        assert!(b.is_empty());
    }

    /// RunFutureAsStream wraps a future and exposes it as a Stream that yields
    /// no items: it returns Ready(None) once the wrapped future completes.
    #[tokio::test]
    async fn run_future_as_stream_yields_no_items() {
        let fut = Box::pin(async { 42_u32 });
        let s: RunFutureAsStream<u32, u32, _> = RunFutureAsStream::new(fut);
        let collected: Vec<u32> = s.collect().await;
        assert!(collected.is_empty());
    }
}
