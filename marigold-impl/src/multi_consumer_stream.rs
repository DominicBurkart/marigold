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

    // -----------------------------------------------------------------------
    // MultiConsumerStream tests
    // -----------------------------------------------------------------------

    /// A single receiver gets every item from the source stream in order.
    #[tokio::test]
    async fn single_receiver_gets_all_items() {
        let source = futures::stream::iter(vec![1u32, 2, 3, 4, 5]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        let result: Vec<u32> = rx.collect().await;
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }

    /// Two receivers each receive every item (fan-out copy semantics).
    #[tokio::test]
    async fn two_receivers_each_get_all_items() {
        let source = futures::stream::iter(vec![10u32, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();
        mcs.run().await;

        let r1: Vec<u32> = rx1.collect().await;
        let r2: Vec<u32> = rx2.collect().await;
        assert_eq!(r1, vec![10, 20, 30]);
        assert_eq!(r2, vec![10, 20, 30]);
    }

    /// Three receivers all get the same items — fan-out scales beyond two.
    #[tokio::test]
    async fn three_receivers_each_get_all_items() {
        let source = futures::stream::iter(0u32..5);
        let mut mcs = MultiConsumerStream::new(source);
        let mut receivers: Vec<_> = (0..3).map(|_| mcs.get()).collect();
        mcs.run().await;

        for rx in &mut receivers {
            let items: Vec<u32> = rx.collect().await;
            assert_eq!(items, vec![0, 1, 2, 3, 4]);
        }
    }

    /// Running with no receivers attached completes without panic or deadlock.
    #[tokio::test]
    async fn zero_receivers_does_not_panic() {
        let source = futures::stream::iter(vec![1u32, 2, 3]);
        let mcs = MultiConsumerStream::new(source);
        // No calls to get() — no senders registered.
        mcs.run().await;
        // Reaching here means run() completed successfully.
    }

    /// An empty source stream closes the receiver immediately (no items, clean EOF).
    #[tokio::test]
    async fn empty_source_closes_receiver() {
        let source = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        let result: Vec<u32> = rx.collect().await;
        assert!(result.is_empty());
    }

    /// After the source is exhausted the receiver channel is closed (returns None).
    /// This verifies that senders are properly disconnected so consumers terminate.
    #[tokio::test]
    async fn receiver_closes_after_source_exhausted() {
        let source = futures::stream::iter(vec![42u32]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        // Drain the single item.
        let first = rx.next().await;
        assert_eq!(first, Some(42u32));

        // Channel should now be closed — next() must return None.
        let after_eof = rx.next().await;
        assert_eq!(after_eof, None);
    }

    /// Receiver order matches source order (items are fed sequentially).
    #[tokio::test]
    async fn items_arrive_in_source_order() {
        let items: Vec<u32> = (0..20).collect();
        let source = futures::stream::iter(items.clone());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;

        let received: Vec<u32> = rx.collect().await;
        assert_eq!(received, items);
    }

    // -----------------------------------------------------------------------
    // RunFutureAsStream tests
    // -----------------------------------------------------------------------

    /// A completed future produces an immediately-terminated stream (zero items).
    /// This is the core contract: RunFutureAsStream is used only for its side
    /// effects; it never yields actual stream values.
    #[tokio::test]
    async fn run_future_as_stream_yields_no_items() {
        // A future that resolves immediately with a value.
        let fut = Box::pin(async { 99u32 });
        let stream: RunFutureAsStream<u32, u32, _> = RunFutureAsStream::new(fut);

        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty(),
            "RunFutureAsStream should never emit items; got {:?}", items);
    }

    /// A future with async work (simulated via a ready future) still produces no items.
    #[tokio::test]
    async fn run_future_as_stream_with_unit_future_yields_no_items() {
        let mut side_effect = 0u32;
        // We cannot capture &mut in an async block that outlives this scope in a
        // Box::pin, so use an atomic / owned value instead.
        let counter = std::sync::Arc::new(std::sync::atomic::AtomicU32::new(0));
        let counter_clone = counter.clone();
        let fut = Box::pin(async move {
            counter_clone.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);

        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
        // The future body ran exactly once.
        assert_eq!(counter.load(std::sync::atomic::Ordering::SeqCst), 1);
        drop(side_effect); // suppress unused warning
    }

    /// size_hint always reports (0, None) — the stream length is unknown/zero.
    #[test]
    fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
