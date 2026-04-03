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

/// A `Stream` adapter that drives a `Future` to completion and then immediately
/// ends the stream (yields no items).
///
/// # Purpose
///
/// This type exists to bridge `MultiConsumerStream::run()` (which is a `Future`
/// that drives a fan-out stream but produces no output values) into the stream
/// composition model used by the Marigold code-generator in `nodes.rs`.
///
/// `StreamVariableNode::runner_code()` and
/// `StreamVariableFromPriorStreamVariableNode::runner_code()` both emit code
/// that wraps the `run()` future in a `RunFutureAsStream`.  The generated code
/// then chains the runner stream with other consumer streams so that the
/// runtime drives both concurrently.
///
/// # Polling behaviour
///
/// - While the inner future is pending, `poll_next` returns `Poll::Pending`.
/// - Once the inner future resolves (i.e. the fan-out is complete), `poll_next`
///   returns `Poll::Ready(None)`, ending the stream.  This is intentional: the
///   stream is only a side-effecting driver and never produces items of type `T`.
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
            // The future completed; signal end-of-stream.  We never yield items
            // because this stream's sole purpose is to drive the future.
            Poll::Ready(_) => Poll::Ready(None),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}
