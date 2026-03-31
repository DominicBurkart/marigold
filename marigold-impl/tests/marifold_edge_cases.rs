use futures::stream::StreamExt;
use marigold_impl::Marifold;

#[tokio::test]
async fn fold_empty_stream() {
    let result = futures::stream::iter(Vec::<i32>::new())
        .marifold(0, |acc, x| async move { acc + x })
        .await
        .collect::<Vec<i32>>()
        .await;
    // Empty stream fold returns the initial accumulator
    assert_eq!(result, vec![0]);
}

#[tokio::test]
async fn fold_single_element() {
    let result = futures::stream::iter(vec![42])
        .marifold(0, |acc, x| async move { acc + x })
        .await
        .collect::<Vec<i32>>()
        .await;
    assert_eq!(result, vec![42]);
}

#[tokio::test]
async fn fold_string_concatenation() {
    let result = futures::stream::iter(vec!["hello", " ", "world"])
        .marifold(String::new(), |mut acc, x| async move {
            acc.push_str(x);
            acc
        })
        .await
        .collect::<Vec<String>>()
        .await;
    assert_eq!(result, vec!["hello world".to_string()]);
}

#[tokio::test]
async fn fold_product() {
    let result = futures::stream::iter(vec![1, 2, 3, 4, 5])
        .marifold(1u64, |acc, x| async move { acc * x })
        .await
        .collect::<Vec<u64>>()
        .await;
    assert_eq!(result, vec![120]);
}
