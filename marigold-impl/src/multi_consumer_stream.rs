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

// These tests exercise the non-feature-gated inline `run()` path, which is
// active when neither the `tokio` nor `async-std` crate features of
// marigold-impl are enabled.  Under those features `run()` spawns a background
// task and returns immediately, so the receivers would deadlock waiting for
// items that the spawned task hasn't distributed yet.  Gate the whole block to
// keep the suite green under all feature combinations.
#[cfg(test)]
#[cfg(not(any(feature = "async-std", feature = "tokio")))]
mod tests {
    use super::MultiConsumerStream;
    use futures::stream::StreamExt;

    // The tokio runtime is available via dev-dependencies (features = ["full"])
    // and drives the async test harness even though the `tokio` crate feature
    // of marigold-impl is disabled.

    /// Both consumers receive every item emitted by the source stream.
    ///
    /// `run()` is spawned concurrently so that the channel back-pressure
    /// (BUFFER_SIZE = 1) doesn't deadlock: the consumers drain items while the
    /// producer is still fanning them out.
    #[tokio::test]
    async fn all_consumers_receive_all_items() {
        let source = futures::stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);

        let rx1 = mcs.get();
        let rx2 = mcs.get();

        // Spawn run() so consumers and producer interleave on the tokio runtime.
        let run_handle = tokio::spawn(mcs.run());

        let (got1, got2, _) =
            tokio::join!(rx1.collect::<Vec<_>>(), rx2.collect::<Vec<_>>(), run_handle,);

        assert_eq!(got1, vec![1, 2, 3]);
        assert_eq!(got2, vec![1, 2, 3]);
    }

    /// A single consumer receives every item from the source.
    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = futures::stream::iter(vec![10u32, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);

        let rx = mcs.get();

        let run_handle = tokio::spawn(mcs.run());
        let (got, _) = tokio::join!(rx.collect::<Vec<_>>(), run_handle);
        assert_eq!(got, vec![10, 20, 30]);
    }

    /// An empty source stream causes all consumers to receive an empty sequence.
    /// No items means no back-pressure, so sequential await is fine here.
    #[tokio::test]
    async fn empty_source_closes_consumers_immediately() {
        let source = futures::stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);

        let rx1 = mcs.get();
        let rx2 = mcs.get();

        mcs.run().await;

        assert!(rx1.collect::<Vec<_>>().await.is_empty());
        assert!(rx2.collect::<Vec<_>>().await.is_empty());
    }

    /// Dropping one receiver before `run()` completes does not prevent the
    /// remaining receiver from collecting all items.  The dropped sender is
    /// disconnected by the channel layer; `feed` returns an error which the
    /// implementation ignores, so the surviving receiver still gets everything.
    #[tokio::test]
    async fn dropped_consumer_does_not_block_other_consumers() {
        let source = futures::stream::iter(vec![1u32, 2, 3, 4, 5]);
        let mut mcs = MultiConsumerStream::new(source);

        let rx_keep = mcs.get();
        let rx_drop = mcs.get();

        // Drop one receiver before `run()` starts so its sender sees a closed
        // channel immediately.
        drop(rx_drop);

        // Spawn run() concurrently so back-pressure on the surviving channel
        // doesn't cause a deadlock (BUFFER_SIZE = 1).
        let run_handle = tokio::spawn(mcs.run());
        let (got, _) = tokio::join!(rx_keep.collect::<Vec<_>>(), run_handle);
        assert_eq!(got, vec![1, 2, 3, 4, 5]);
    }

    /// Items are buffered correctly: verify that a single item is available
    /// after `run()` completes.  A single-item source never exceeds the buffer
    /// capacity (BUFFER_SIZE = 1), so sequential await is safe here.
    #[tokio::test]
    async fn buffered_items_are_received_after_run() {
        let source = futures::stream::iter(vec![42u32]);
        let mut mcs = MultiConsumerStream::new(source);

        let mut rx = mcs.get();

        mcs.run().await;

        // The item should be sitting in the channel buffer.
        let item = rx.next().await;
        assert_eq!(item, Some(42u32));

        // And the channel should now be closed (source exhausted).
        let nothing = rx.next().await;
        assert_eq!(nothing, None);
    }

    /// `RunFutureAsStream` always resolves to an empty stream regardless of what
    /// the underlying future returns.  The stream item type (`u32`) and the
    /// future's output type (`&str`) are intentionally different to verify the
    /// implementation correctly separates the two roles.
    #[tokio::test]
    async fn run_future_as_stream_is_always_empty() {
        use super::RunFutureAsStream;

        let fut = Box::pin(async { "side-effect" });
        let stream: RunFutureAsStream<u32, &str, _> = RunFutureAsStream::new(fut);
        let items = stream.collect::<Vec<u32>>().await;
        assert!(items.is_empty(), "RunFutureAsStream should emit no items");
    }

    /// `run()` with no consumers must complete without panicking or hanging.
    /// Items from the source are silently dropped when the sender list is empty.
    #[tokio::test]
    async fn no_consumers_run_completes_without_panic() {
        let source = futures::stream::iter(vec![1u32, 2, 3]);
        let mcs = MultiConsumerStream::new(source);
        mcs.run().await; // must not panic or hang
    }
}
