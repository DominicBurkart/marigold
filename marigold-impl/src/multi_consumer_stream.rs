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
    async fn run_future_as_stream_completes_without_items() {
        let stream = RunFutureAsStream::<u32, _, _>::new(Box::pin(async {}));
        let items: Vec<u32> = stream.collect().await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn run_future_as_stream_size_hint_is_zero_upper_none() {
        let stream = RunFutureAsStream::<u32, _, _>::new(Box::pin(async {}));
        assert_eq!(stream.size_hint(), (0, None));
    }

    #[tokio::test]
    async fn run_future_as_stream_executes_future_side_effect() {
        use std::sync::{Arc, Mutex};
        let flag = Arc::new(Mutex::new(false));
        let flag_clone = flag.clone();
        let stream = RunFutureAsStream::<u32, _, _>::new(Box::pin(async move {
            *flag_clone.lock().unwrap() = true;
        }));
        let _: Vec<u32> = stream.collect().await;
        assert!(*flag.lock().unwrap());
    }

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn multi_consumer_single_subscriber_receives_all_items() {
        use futures::stream;
        let source = stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;
        let mut results = Vec::new();
        while let Some(v) = rx.next().await {
            results.push(v);
        }
        assert_eq!(results, vec![1, 2, 3]);
    }

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn multi_consumer_empty_stream_closes_receiver() {
        use futures::stream;
        let source = stream::iter(Vec::<u32>::new());
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx = mcs.get();
        mcs.run().await;
        assert!(rx.next().await.is_none());
    }

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn multi_consumer_no_subscribers_completes_without_panic() {
        use futures::stream;
        let source = stream::iter(vec![1u32, 2, 3]);
        let mcs = MultiConsumerStream::new(source);
        mcs.run().await;
    }

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn multi_consumer_two_subscribers_each_receive_all_items() {
        use futures::stream;
        let source = stream::iter(vec![1u32, 2, 3]);
        let mut mcs = MultiConsumerStream::new(source);
        let mut rx1 = mcs.get();
        let mut rx2 = mcs.get();
        mcs.run().await;
        let t1 = tokio::spawn(async move {
            let mut v = Vec::new();
            while let Some(x) = rx1.next().await {
                v.push(x);
            }
            v
        });
        let t2 = tokio::spawn(async move {
            let mut v = Vec::new();
            while let Some(x) = rx2.next().await {
                v.push(x);
            }
            v
        });
        let (r1, r2) = tokio::join!(t1, t2);
        assert_eq!(r1.unwrap(), vec![1, 2, 3]);
        assert_eq!(r2.unwrap(), vec![1, 2, 3]);
    }
}
