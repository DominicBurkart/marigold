use marigold::m;
use marigold::marigold_impl::futures::Stream;
use marigold::marigold_impl::StreamExt;
use std::future::Future;
use std::time::Duration;

const GUARD: Duration = Duration::from_secs(20);

async fn collect_sorted<F, S>(label: &str, program: F) -> Vec<S::Item>
where
    F: Future<Output = S>,
    S: Stream + Unpin,
    S::Item: Ord,
{
    let run = async { program.await.collect::<Vec<_>>().await };
    let mut out = tokio::time::timeout(GUARD, run)
        .await
        .unwrap_or_else(|_| panic!("HANG: {label} did not complete within {GUARD:?}"));
    out.sort();
    out
}

#[tokio::test]
async fn named_permutations_n1() {
    let inline = collect_sorted("inline", m!(range(0, 5).permutations(1).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.permutations(1).return
        ),
    )
    .await;
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_permutations_n5() {
    let inline = collect_sorted("inline", m!(range(0, 5).permutations(5).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.permutations(5).return
        ),
    )
    .await;
    assert_eq!(named.len(), 120);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_permutations_with_replacement_n1() {
    let inline = collect_sorted(
        "inline",
        m!(range(0, 5).permutations_with_replacement(1).return),
    )
    .await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.permutations_with_replacement(1).return
        ),
    )
    .await;
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_permutations_with_replacement_n5() {
    let inline = collect_sorted(
        "inline",
        m!(range(0, 5).permutations_with_replacement(5).return),
    )
    .await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.permutations_with_replacement(5).return
        ),
    )
    .await;
    assert_eq!(named.len(), 3125);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_combinations_n1() {
    let inline = collect_sorted("inline", m!(range(0, 5).combinations(1).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.combinations(1).return
        ),
    )
    .await;
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_combinations_n5() {
    let inline = collect_sorted("inline", m!(range(0, 5).combinations(5).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.combinations(5).return
        ),
    )
    .await;
    assert_eq!(named, vec![[0, 1, 2, 3, 4]]);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_keep_first_n_n1() {
    let sorter = |a: &i32, b: &i32| b.cmp(a);
    let inline = collect_sorted("inline", m!(range(0, 5).keep_first_n(1, sorter).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.keep_first_n(1, sorter).return
        ),
    )
    .await;
    assert_eq!(named, vec![0]);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_keep_first_n_n5() {
    let sorter = |a: &i32, b: &i32| b.cmp(a);
    let inline = collect_sorted("inline", m!(range(0, 5).keep_first_n(5, sorter).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.keep_first_n(5, sorter).return
        ),
    )
    .await;
    assert_eq!(named, vec![0, 1, 2, 3, 4]);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn named_fold_completes() {
    fn add(acc: i32, x: i32) -> i32 {
        acc + x
    }
    let inline = collect_sorted("inline", m!(range(0, 5).fold(0, add).return)).await;
    let named = collect_sorted(
        "named",
        m!(
            x = range(0, 5)
            x.fold(0, add).return
        ),
    )
    .await;
    assert_eq!(named, vec![10]);
    assert_eq!(named, inline);
}

#[tokio::test]
async fn variable_from_variable_permutations() {
    let inline = collect_sorted("inline", m!(range(0, 5).permutations(1).return)).await;
    let named = collect_sorted(
        "variable-from-variable",
        m!(
            x = range(0, 5)
            y = x.permutations(1)
            y.return
        ),
    )
    .await;
    assert_eq!(named, inline);
}

#[tokio::test]
async fn two_consumers_of_one_variable_both_collecting() {
    fn first_of_pair(c: [i32; 2]) -> [i32; 1] {
        [c[0]]
    }
    let named = collect_sorted(
        "two consumers",
        m!(
            x = range(0, 3)
            x.permutations(1).return
            x.combinations(2).map(first_of_pair).return
        ),
    )
    .await;
    assert_eq!(named, vec![[0], [0], [0], [1], [1], [2]]);
}

#[tokio::test]
async fn collecting_and_non_collecting_consumers_of_one_variable() {
    fn double(i: i32) -> i32 {
        i * 2
    }
    let named = collect_sorted(
        "mixed consumers",
        m!(
            x = range(0, 5)
            x.map(double).map(wrap_one).return
            x.permutations(1).return
        ),
    )
    .await;
    let mut expected: Vec<[i32; 1]> = (0..5).map(|i| [i * 2]).chain((0..5).map(|i| [i])).collect();
    expected.sort();
    assert_eq!(named, expected);
}

fn wrap_one(i: i32) -> [i32; 1] {
    [i]
}

#[tokio::test]
async fn late_polled_collecting_consumer_receives_every_item() {
    fn double(i: i32) -> i32 {
        i * 2
    }
    let run = async {
        let mut stream = m!(
            x = range(0, 5)
            x.map(double).map(wrap_one).return
            x.permutations(1).return
        )
        .await;
        let mut out = vec![stream.next().await.unwrap()];
        tokio::time::sleep(Duration::from_millis(200)).await;
        while let Some(item) = stream.next().await {
            out.push(item);
        }
        out
    };
    let mut out = tokio::time::timeout(GUARD, run)
        .await
        .unwrap_or_else(|_| panic!("HANG: late polled consumer did not complete within {GUARD:?}"));
    out.sort();
    let mut expected: Vec<[i32; 1]> = (0..5).map(|i| [i * 2]).chain((0..5).map(|i| [i])).collect();
    expected.sort();
    assert_eq!(out, expected);
}
