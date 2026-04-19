//! Empirical cardinality oracle tests.
//!
//! Each test runs a marigold program via the `m!()` macro, counts the items
//! produced at runtime, and verifies the count matches the cardinality
//! predicted by `marigold_analyze()`.

use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::complexity::Cardinality;
use num_bigint::BigUint;

/// Assert that the actual item count from running a marigold program matches
/// the cardinality predicted by the static analyzer.
fn assert_cardinality_matches(actual_len: usize, predicted: &Cardinality) {
    match predicted {
        Cardinality::Exact(n) => {
            assert_eq!(
                BigUint::from(actual_len as u64),
                *n,
                "Exact cardinality mismatch: runtime produced {actual_len} items, analyzer predicted {n}"
            );
        }
        Cardinality::Bounded(symbolic) => {
            if let Some(bound) = symbolic.upper_bound() {
                assert!(
                    BigUint::from(actual_len as u64) <= bound,
                    "Bounded cardinality violated: runtime produced {actual_len} items, \
                     but upper bound is {bound}"
                );
            }
            // If upper_bound() returns None, we cannot assert a numeric bound
        }
        Cardinality::Unknown => {
            // No assertion possible for unknown cardinality
        }
    }
}

#[tokio::test]
async fn oracle_range() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    let items: Vec<i32> = m!(range(0, 100).return).await.collect::<Vec<_>>().await;

    assert_cardinality_matches(items.len(), predicted);
    assert_eq!(items.len(), 100);
}

#[tokio::test]
async fn oracle_combinations() {
    let result = marigold_grammar::marigold_analyze("range(0, 10).combinations(2).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    let items: Vec<[i32; 2]> = m!(range(0, 10).combinations(2).return)
        .await
        .collect::<Vec<_>>()
        .await;

    assert_cardinality_matches(items.len(), predicted);
    // C(10, 2) = 45
    assert_eq!(items.len(), 45);
}

#[tokio::test]
async fn oracle_permutations() {
    let result = marigold_grammar::marigold_analyze("range(0, 10).permutations(2).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    let items: Vec<[i32; 2]> = m!(range(0, 10).permutations(2).return)
        .await
        .collect::<Vec<_>>()
        .await;

    assert_cardinality_matches(items.len(), predicted);
    // P(10, 2) = 90
    assert_eq!(items.len(), 90);
}

#[tokio::test]
async fn oracle_fold() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).fold(0, add).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    fn add(acc: i32, v: i32) -> i32 {
        acc + v
    }

    let items: Vec<i32> = m!(
        range(0, 100)
            .fold(0, add)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // fold always produces exactly 1 item
    assert_eq!(items.len(), 1);
    assert_eq!(items[0], 4950);
}

#[tokio::test]
async fn oracle_map() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).map(double).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    let items: Vec<i32> = m!(
        fn double(v: i32) -> i32 {
            v * 2
        }

        range(0, 100)
            .map(double)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // map preserves cardinality
    assert_eq!(items.len(), 100);
}

#[tokio::test]
async fn oracle_filter() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 100).filter(is_even).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    fn is_even(v: i32) -> bool {
        v % 2 == 0
    }

    let items: Vec<i32> = m!(
        range(0, 100)
            .filter(is_even)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // filter produces bounded cardinality; actual count is 50
    assert_eq!(items.len(), 50);
}

#[tokio::test]
async fn oracle_take_while() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 10).take_while(less_than_4).return").unwrap();
    let predicted = &result.streams[0].cardinality;

    fn less_than_4(i: &i32) -> bool {
        *i < 4
    }

    let items: Vec<i32> = m!(
        range(0, 10)
            .take_while(less_than_4)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // take_while produces bounded cardinality; actual count is 4 (0, 1, 2, 3)
    assert_eq!(items.len(), 4);
}
