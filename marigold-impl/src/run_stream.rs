use core::pin::Pin;
use futures::Stream;

#[cfg(any(feature = "async-std", feature = "tokio"))]
use futures::StreamExt;

#[cfg(any(feature = "async-std", feature = "tokio"))]
#[inline]
pub fn run_stream<
    T: std::marker::Send + 'static,
    S: Stream<Item = T> + 'static + std::marker::Send + std::marker::Unpin,
>(
    s: S,
) -> Pin<Box<dyn Stream<Item = T>>> {
    let (sender, receiver) = futures::channel::mpsc::channel(std::cmp::max(num_cpus::get() - 1, 2));
    crate::async_runtime::spawn(async move {
        let mut moved_sender = sender;
        s.map(Ok)
            .forward(&mut moved_sender)
            .await
            .expect("Internal marigold error: could not write stream results to channel");
    });
    Box::pin(receiver)
}

#[cfg(not(any(feature = "async-std", feature = "tokio")))]
#[inline]
pub fn run_stream<
    T: std::marker::Send + 'static,
    S: Stream<Item = T> + 'static + std::marker::Send + std::marker::Unpin,
>(
    s: S,
) -> Pin<Box<dyn Stream<Item = T>>> {
    Box::pin(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::stream::StreamExt;

    #[tokio::test]
    async fn combinations() {
        assert_eq!(
            run_stream(futures::stream::iter(0_u32..3_u32))
                .collect::<Vec<_>>()
                .await,
            vec![0_u32, 1_u32, 2_u32]
        );
    }

    #[tokio::test]
    async fn run_stream_empty() {
        // An empty stream should produce an empty Vec.
        let result: Vec<u32> = run_stream(futures::stream::iter(std::iter::empty::<u32>()))
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, Vec::<u32>::new());
    }

    #[tokio::test]
    async fn run_stream_single_element() {
        // A stream with one element should produce a Vec with that one element.
        let result = run_stream(futures::stream::iter(vec![42_u32]))
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![42_u32]);
    }

    #[tokio::test]
    async fn run_stream_large() {
        // A large stream should preserve all elements in order.
        let n = 1_000_u32;
        let result = run_stream(futures::stream::iter(0..n))
            .collect::<Vec<_>>()
            .await;
        let expected: Vec<u32> = (0..n).collect();
        assert_eq!(result, expected);
    }
}
