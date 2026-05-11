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
    use futures::stream;
    use futures::StreamExt;

    #[tokio::test]
    async fn test_empty_stream_no_items() {
        let s = stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(s);
        let mut rx = mcs.get();
        mcs.run().await;
        assert!(rx.next().await.is_none());
    }

    #[tokio::test]
    async fn test_single_receiver_collects_all_items() {
        let s = stream::iter(vec![10u32, 20, 30]);
        let mut mcs = MultiConsumerStream::new(s);
        let mut rx = mcs.get();
        // Run producer and consumer concurrently to avoid blocking on the
        // bounded channel (BUFFER_SIZE = 1).
        let (_, collected) = tokio::join!(
            mcs.run(),
            async move {
                let mut out = vec![];
                while let Some(v) = rx.next().await {
                    out.push(v);
                }
                out
            },
        );
        assert_eq!(collected, vec![10u32, 20, 30]);
    }

    #[tokio::test]
    async fn test_two_receivers_each_get_all_items() {
        let s = stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(s);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();

        let collect1 = async move {
            let mut out = vec![];
            while let Some(v) = rx1.next().await {
                out.push(v);
            }
            out
        };
        let collect2 = async move {
            let mut out = vec![];
            while let Some(v) = rx2.next().await {
                out.push(v);
            }
            out
        };

        let (_, c1, c2) = tokio::join!(mcs.run(), collect1, collect2);
        assert_eq!(c1, vec![1u32, 2, 3]);
        assert_eq!(c2, vec![1u32, 2, 3]);
    }

    #[tokio::test]
    async fn test_run_future_as_stream_yields_nothing() {
        let fut = Box::pin(async { 42u32 });
        let mut stream: RunFutureAsStream<u32, u32, _> = RunFutureAsStream::new(fut);
        let result = stream.next().await;
        assert!(result.is_none());
    }

    #[test]
    fn test_run_future_as_stream_size_hint() {
        let fut = Box::pin(async { () });
        let stream: RunFutureAsStream<u32, (), _> = RunFutureAsStream::new(fut);
        assert_eq!(stream.size_hint(), (0, None));
    }
}
