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

    /// Helper: build a MultiConsumerStream over `items`, register `n_consumers`,
    /// then drive `run()` concurrently with draining each consumer.
    ///
    /// `run()` spawns the broadcaster task internally and returns immediately, so
    /// we spawn it on a separate tokio task and join all tasks concurrently via
    /// `tokio::join!`-equivalent logic. This avoids a race where sequencing
    /// `run().await` before joining drain tasks could let consumers finish their
    /// `.collect()` before the broadcaster has pushed all items through the
    /// bounded (size-1) mpsc channel.
    async fn broadcast<T: Copy + Send + 'static>(
        items: Vec<T>,
        n_consumers: usize,
    ) -> Vec<Vec<T>> {
        let source = futures::stream::iter(items);
        let mut multi = MultiConsumerStream::new(source);
        let receivers: Vec<_> = (0..n_consumers).map(|_| multi.get()).collect();

        // Spawn each consumer drain as a tokio task so that slow consumers
        // cannot deadlock the others through the bounded mpsc channel.
        let drain_tasks: Vec<_> = receivers
            .into_iter()
            .map(|r| tokio::spawn(async move { r.collect::<Vec<T>>().await }))
            .collect();

        // Spawn run() as its own task so the broadcaster and all drain tasks
        // proceed concurrently; awaiting run() directly first would race
        // against consumers on the size-1 buffer.
        let run_handle = tokio::spawn(async move { multi.run().await });

        let mut outputs = Vec::with_capacity(drain_tasks.len());
        for t in drain_tasks {
            outputs.push(t.await.expect("consumer task panicked"));
        }
        run_handle.await.expect("run task panicked");
        outputs
    }

    /// Every consumer receives every item in the original order.
    #[tokio::test]
    async fn every_consumer_receives_full_stream_in_order() {
        let items: Vec<u32> = (0..50).collect();
        let outputs = broadcast(items.clone(), 3).await;

        assert_eq!(outputs.len(), 3);
        for out in &outputs {
            assert_eq!(out, &items);
        }
    }

    /// A single consumer still gets the full stream.
    #[tokio::test]
    async fn single_consumer_gets_all_items() {
        let items: Vec<i64> = vec![-3, 0, 7, 42, 100];
        let outputs = broadcast(items.clone(), 1).await;
        assert_eq!(outputs, vec![items]);
    }

    /// run() terminates cleanly when there are zero consumers.
    #[tokio::test]
    async fn no_consumers_run_completes() {
        let source = futures::stream::iter(0..5_u32);
        let multi = MultiConsumerStream::new(source);
        // With zero consumers, run() iterates the source and exits immediately
        // (microseconds). Use a short timeout so a regression fails loudly
        // without bloating CI wall-time.
        tokio::time::timeout(std::time::Duration::from_millis(100), multi.run())
            .await
            .expect("run() hung with zero consumers");
    }

    /// Once the inner stream is exhausted, consumers observe end-of-stream.
    #[tokio::test]
    async fn consumers_see_end_of_stream() {
        let outputs = broadcast::<u8>(vec![], 4).await;
        assert_eq!(outputs.len(), 4);
        for out in &outputs {
            assert!(out.is_empty(), "expected empty stream, got {out:?}");
        }
    }

    /// Many consumers all receive identical, full output.
    #[tokio::test]
    async fn many_consumers_receive_identical_streams() {
        let items: Vec<u32> = (0..200).collect();
        let outputs = broadcast(items.clone(), 16).await;
        assert_eq!(outputs.len(), 16);
        for (i, out) in outputs.iter().enumerate() {
            assert_eq!(out, &items, "consumer {i} saw a divergent stream");
        }
    }

    /// RunFutureAsStream yields no items and completes Ready(None) once the
    /// wrapped future resolves.
    #[tokio::test]
    async fn run_future_as_stream_yields_none_on_completion() {
        let fut = Box::pin(async { 42_u32 });
        let stream: RunFutureAsStream<u8, u32, _> = RunFutureAsStream::new(fut);
        let collected: Vec<u8> = stream.collect().await;
        assert!(collected.is_empty());
    }

    /// RunFutureAsStream size_hint is always (0, None).
    ///
    /// `None` as the upper bound is deliberate: this stream never yields items,
    /// but advertising `None` rather than `Some(0)` keeps the contract
    /// forward-compatible if the type is extended to emit items in the future.
    /// Changing it to `Some(0)` would be a narrowing (breaking) contract change.
    #[test]
    fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async {});
        let stream: RunFutureAsStream<u8, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
