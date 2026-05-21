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

        let task = async move {
            while let Some(v) = self.inner_stream.next().await {
                let mut futures = self
                    .senders
                    .iter_mut()
                    .map(|sender| sender.feed(v))
                    .collect::<FuturesUnordered<_>>();
                while let Some(_result) = futures.next().await {}
            }
            self.senders.iter_mut().for_each(|s| s.disconnect());
        };

        #[cfg(any(feature = "async-std", feature = "tokio"))]
        crate::async_runtime::spawn(task);

        #[cfg(not(any(feature = "async-std", feature = "tokio")))]
        task.await;
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

    // `RunFutureAsStream` should always yield `None` once the wrapped future
    // resolves and otherwise propagates `Pending`. These invariants matter
    // because in generated marigold code the future drives a
    // `MultiConsumerStream::run`, and downstream consumers poll the wrapper
    // alongside their receivers.
    #[tokio::test]
    async fn run_future_as_stream_completes_with_none() {
        let fut = Box::pin(async {});
        let mut s: RunFutureAsStream<u8, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(s.next().await, None);
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint_is_unbounded() {
        let fut = Box::pin(async {});
        let s: RunFutureAsStream<u8, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(s.size_hint(), (0, None));
    }

    // The remaining tests exercise concurrent fan-out, which only happens
    // when an async runtime is enabled (otherwise `MultiConsumerStream::run`
    // is sequential and would deadlock against the BUFFER_SIZE-1 channel
    // when consumers haven't been awaited yet).
    #[cfg(any(feature = "tokio", feature = "async-std"))]
    mod runtime {
        use super::super::MultiConsumerStream;
        use futures::stream::StreamExt;

        // Single-consumer fan-out should be a faithful relay of the source
        // stream, in order, with end-of-stream signalled by the receiver
        // closing.
        #[tokio::test]
        async fn single_consumer_receives_all_items_in_order() {
            let source = futures::stream::iter(0u32..10);
            let mut mcs = MultiConsumerStream::new(source);
            let recv = mcs.get();
            mcs.run().await;
            let collected: Vec<u32> = recv.collect().await;
            assert_eq!(collected, (0u32..10).collect::<Vec<_>>());
        }

        // Every registered consumer must observe the entire source stream.
        #[tokio::test]
        async fn multiple_consumers_each_receive_full_stream() {
            let source = futures::stream::iter(vec![10i32, 20, 30, 40, 50]);
            let mut mcs = MultiConsumerStream::new(source);
            let r1 = mcs.get();
            let r2 = mcs.get();
            let r3 = mcs.get();
            mcs.run().await;
            let (a, b, c) = futures::join!(
                r1.collect::<Vec<_>>(),
                r2.collect::<Vec<_>>(),
                r3.collect::<Vec<_>>()
            );
            let expected = vec![10, 20, 30, 40, 50];
            assert_eq!(a, expected);
            assert_eq!(b, expected);
            assert_eq!(c, expected);
        }

        // An empty source yields empty consumers but must still terminate
        // (no hang waiting on items that never arrive).
        #[tokio::test]
        async fn empty_source_produces_empty_consumers() {
            let source = futures::stream::iter(Vec::<u8>::new());
            let mut mcs = MultiConsumerStream::new(source);
            let r1 = mcs.get();
            let r2 = mcs.get();
            mcs.run().await;
            let (a, b) = futures::join!(r1.collect::<Vec<_>>(), r2.collect::<Vec<_>>());
            assert!(a.is_empty());
            assert!(b.is_empty());
        }

        // Registering zero consumers is allowed; `run` must still drain the
        // source and return.
        #[tokio::test]
        async fn no_consumers_still_terminates() {
            let source = futures::stream::iter(0u32..3);
            let mcs = MultiConsumerStream::new(source);
            // No `.get()` calls. If run() ever blocked without consumers,
            // tokio::test's default timeout would fail this test.
            mcs.run().await;
        }

        // Each consumer must terminate (receiver closes) once the source
        // stream is exhausted, so downstream `.collect()` does not hang.
        #[tokio::test]
        async fn consumers_terminate_after_source_exhaustion() {
            let source = futures::stream::iter(vec![1u8]);
            let mut mcs = MultiConsumerStream::new(source);
            let mut r = mcs.get();
            mcs.run().await;
            assert_eq!(r.next().await, Some(1));
            assert_eq!(r.next().await, None);
        }

        // Consumers must be able to poll concurrently without one blocking
        // another beyond the BUFFER_SIZE backpressure window. This guards
        // against a regression where `feed` is replaced with a serial
        // `send_all`-style call that would prevent fast consumers from
        // making progress when a slow consumer exists.
        #[tokio::test]
        async fn slow_consumer_does_not_lose_items() {
            let source = futures::stream::iter(0u32..50);
            let mut mcs = MultiConsumerStream::new(source);
            let fast = mcs.get();
            let slow = mcs.get();
            mcs.run().await;

            // Slow consumer yields between items but must still get all of them.
            let slow_task = async {
                let mut out = Vec::new();
                let mut s = slow;
                while let Some(v) = s.next().await {
                    tokio::task::yield_now().await;
                    out.push(v);
                }
                out
            };
            let (fast_out, slow_out) = futures::join!(fast.collect::<Vec<_>>(), slow_task);
            let expected: Vec<u32> = (0u32..50).collect();
            assert_eq!(fast_out, expected);
            assert_eq!(slow_out, expected);
        }
    }
}
