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

    #[tokio::test]
    async fn single_consumer_receives_all_items() {
        let source = futures::stream::iter(vec![1, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let rx = mcs.get();
        mcs.run().await;

        let collected: Vec<i32> = rx.collect().await;
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn two_consumers_both_receive_all_items() {
        let source = futures::stream::iter(vec![10, 20, 30]);
        let mut mcs = MultiConsumerStream::new(source);
        let rx1 = mcs.get();
        let rx2 = mcs.get();
        mcs.run().await;

        let (r1, r2): (Vec<i32>, Vec<i32>) =
            futures::future::join(rx1.collect(), rx2.collect()).await;
        assert_eq!(r1, vec![10, 20, 30]);
        assert_eq!(r2, vec![10, 20, 30]);
    }

    #[tokio::test]
    async fn empty_stream_produces_no_items() {
        let source = futures::stream::iter(Vec::<i32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let rx = mcs.get();
        mcs.run().await;

        let collected: Vec<i32> = rx.collect().await;
        assert!(collected.is_empty());
    }

    #[tokio::test]
    async fn no_consumers_does_not_panic() {
        let source = futures::stream::iter(vec![1, 2, 3]);
        let mcs = MultiConsumerStream::new(source);
        // Should not panic even with no receivers registered
        mcs.run().await;
    }

    #[tokio::test]
    async fn run_future_as_stream_yields_none_when_future_completes() {
        let fut = Box::pin(async { 42 });
        let mut stream = RunFutureAsStream::<i32, _, _>::new(fut);
        // When the future is ready, poll_next should return Ready(None)
        let result: Vec<i32> = (&mut stream).collect().await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream = RunFutureAsStream::<i32, _, _>::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
