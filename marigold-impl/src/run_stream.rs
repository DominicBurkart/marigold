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
    async fn empty_stream() {
        let result: Vec<u32> = run_stream(futures::stream::iter(Vec::<u32>::new()))
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn single_element() {
        let result: Vec<u32> = run_stream(futures::stream::iter(vec![42u32]))
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![42u32]);
    }

    #[tokio::test]
    async fn large_stream_preserves_order() {
        let items: Vec<u64> = (0..500).collect();
        let expected = items.clone();
        let result: Vec<u64> = run_stream(futures::stream::iter(items))
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, expected);
    }
}
