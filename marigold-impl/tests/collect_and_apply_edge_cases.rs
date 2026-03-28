use marigold_impl::CollectAndAppliable;

#[tokio::test]
async fn collect_and_apply_empty_stream() {
    let result = futures::stream::iter(Vec::<i32>::new())
        .collect_and_apply(|v| v.len())
        .await;
    assert_eq!(result, 0);
}

#[tokio::test]
async fn collect_and_apply_single_element() {
    let result = futures::stream::iter(vec![42])
        .collect_and_apply(|v| v[0])
        .await;
    assert_eq!(result, 42);
}

#[tokio::test]
async fn collect_and_apply_sum() {
    let result = futures::stream::iter(1..=100)
        .collect_and_apply(|v| v.iter().sum::<i32>())
        .await;
    assert_eq!(result, 5050);
}

#[tokio::test]
async fn collect_and_apply_transform_type() {
    let result = futures::stream::iter(vec![1, 2, 3])
        .collect_and_apply(|v| format!("{:?}", v))
        .await;
    assert_eq!(result, "[1, 2, 3]");
}
