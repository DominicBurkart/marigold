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

// These tests use #[tokio::test], tokio::spawn, and tokio::task::yield_now(),
// which are all tokio-specific. They must only compile when the tokio feature
// is active; gating on `any(feature = "tokio", feature = "async-std")` would
// cause compilation failures under --features async-std (no tokio runtime).
#[cfg(all(test, feature = "tokio"))]
mod tests {
    use super::*;
    use futures::stream::StreamExt;

    /// Invariant: every consumer receives every item produced by the inner
    /// stream, in source order. Receivers must be drained concurrently —
    /// `BUFFER_SIZE = 1` means the producer blocks until every consumer has
    /// taken the current value, so polling them sequentially would deadlock.
    #[tokio::test]
    async fn fans_out_all_items_in_order_to_each_consumer() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..32_u32));
        let r1 = mcs.get();
        let r2 = mcs.get();
        let r3 = mcs.get();
        mcs.run().await;

        let (v1, v2, v3) = futures::join!(
            r1.collect::<Vec<_>>(),
            r2.collect::<Vec<_>>(),
            r3.collect::<Vec<_>>(),
        );

        let expected: Vec<u32> = (0..32).collect();
        assert_eq!(v1, expected);
        assert_eq!(v2, expected);
        assert_eq!(v3, expected);
    }

    /// Invariant: a `MultiConsumerStream` with no consumers must not panic and
    /// must drain the inner stream cleanly.
    #[tokio::test]
    async fn run_with_no_consumers_completes() {
        let mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..8_u32));
        // Just exercising `run` — spawned task should not panic.
        mcs.run().await;
        // Yield so the spawned task has a chance to finish before the test ends.
        tokio::task::yield_now().await;
    }

    /// Invariant: after the inner stream is exhausted, all receivers observe
    /// end-of-stream (the senders are disconnected).
    #[tokio::test]
    async fn receivers_close_after_source_exhausted() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_u8..4_u8));
        let mut r = mcs.get();
        mcs.run().await;

        // Drain everything, then confirm the next poll yields None (closed).
        let mut items = Vec::new();
        while let Some(v) = r.next().await {
            items.push(v);
        }
        assert_eq!(items, vec![0, 1, 2, 3]);
        assert!(r.next().await.is_none());
    }

    /// Invariant: an empty source still produces a closed receiver per consumer
    /// (no hangs, no panics).
    #[tokio::test]
    async fn empty_source_closes_each_receiver() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(Vec::<i32>::new()));
        let r1 = mcs.get();
        let r2 = mcs.get();
        mcs.run().await;
        assert!(r1.collect::<Vec<_>>().await.is_empty());
        assert!(r2.collect::<Vec<_>>().await.is_empty());
    }

    /// Invariant: a single consumer is a degenerate but supported case.
    #[tokio::test]
    async fn single_consumer_receives_everything() {
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(vec![10_i64, 20, 30]));
        let r = mcs.get();
        mcs.run().await;
        assert_eq!(r.collect::<Vec<_>>().await, vec![10, 20, 30]);
    }

    /// Invariant: fan-out must not silently drop items even when consumers read
    /// at very different rates. With BUFFER_SIZE = 1 the producer is forced to
    /// wait for the slower consumer, so the slow consumer must still observe
    /// every value.
    #[tokio::test]
    async fn slow_consumer_does_not_lose_items() {
        let n: u32 = 64;
        let mut mcs = MultiConsumerStream::new(futures::stream::iter(0_u32..n));
        let fast = mcs.get();
        let mut slow = mcs.get();
        mcs.run().await;

        // Drive the fast consumer to completion in the background.
        let fast_handle = tokio::spawn(async move { fast.collect::<Vec<_>>().await });

        // Read the slow consumer with explicit yields between items so the
        // producer is repeatedly forced to wait on us.
        let mut got = Vec::with_capacity(n as usize);
        while let Some(v) = slow.next().await {
            got.push(v);
            tokio::task::yield_now().await;
        }

        let expected: Vec<u32> = (0..n).collect();
        assert_eq!(got, expected);
        assert_eq!(fast_handle.await.unwrap(), expected);
    }
}
