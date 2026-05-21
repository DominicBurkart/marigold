//! Tests for type-changing folds via `Marifold`.
//!
//! The existing tests only fold same-type streams (i32→i32, &str→String, u64→u64).
//! These tests cover the generic type-change path (SInput::Item != State) and
//! verify that the `marifold` output is itself a chainable `Stream`.

use futures::stream::StreamExt;
use marigold_impl::Marifold;

// --- Type-changing folds -----------------------------------------------------

#[tokio::test]
async fn fold_integers_into_csv_string() {
    let result = futures::stream::iter(vec![1i32, 2, 3, 4, 5])
        .marifold(String::new(), |mut acc, x| async move {
            if !acc.is_empty() {
                acc.push(',');
            }
            acc.push_str(&x.to_string());
            acc
        })
        .await
        .collect::<Vec<String>>()
        .await;

    assert_eq!(result, vec!["1,2,3,4,5".to_string()]);
}

#[tokio::test]
async fn fold_strings_into_total_character_count() {
    let words = vec!["hello", "world", "rust"];
    let result = futures::stream::iter(words)
        .marifold(0usize, |acc, word| async move { acc + word.len() })
        .await
        .collect::<Vec<usize>>()
        .await;

    // "hello"=5 + "world"=5 + "rust"=4 = 14
    assert_eq!(result, vec![14]);
}

#[tokio::test]
async fn fold_i32_stream_into_bool_all_positive() {
    let result = futures::stream::iter(vec![1i32, 2, 3])
        .marifold(true, |acc, x| async move { acc && x > 0 })
        .await
        .collect::<Vec<bool>>()
        .await;
    assert_eq!(result, vec![true]);

    let result_with_neg = futures::stream::iter(vec![1i32, -1, 3])
        .marifold(true, |acc, x| async move { acc && x > 0 })
        .await
        .collect::<Vec<bool>>()
        .await;
    assert_eq!(result_with_neg, vec![false]);
}

// --- marifold output is a chainable Stream -----------------------------------

#[tokio::test]
async fn marifold_output_can_be_polled_with_next() {
    let mut stream = futures::stream::iter(vec![10u32, 20, 30])
        .marifold(0u32, |acc, x| async move { acc + x })
        .await;

    let first = stream.next().await;
    assert_eq!(first, Some(60u32), "sum should be 60");

    let second = stream.next().await;
    assert_eq!(second, None, "stream should be exhausted after one item");
}

#[tokio::test]
async fn marifold_empty_stream_yields_init_for_type_changed_accumulator() {
    // empty stream of i32 folds into a String accumulator
    let result = futures::stream::iter(Vec::<i32>::new())
        .marifold("initial".to_string(), |acc, x| async move {
            format!("{acc}+{x}")
        })
        .await
        .collect::<Vec<String>>()
        .await;

    assert_eq!(
        result,
        vec!["initial".to_string()],
        "empty stream should yield init unchanged"
    );
}
