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
    // The analyzer string and the `m!()` body must be structurally
    // identical programs — otherwise the oracle is comparing the
    // runtime behaviour of one program to the analyzer's prediction
    // for a different one. In particular, the `fn add` declaration
    // is expressed inside the marigold program (via `TypedExpression::
    // FnDeclaration`) in BOTH places, not as a Rust fn outside `m!()`.
    let result = marigold_grammar::marigold_analyze(
        "fn add(a: i32, v: i32) -> i32 { a + v } range(0, 100).fold(0, add).return",
    )
    .unwrap();
    // `fold` is an `UnnamedReturningStream`; with a `FnDeclaration`
    // ahead of it the stream is still `streams[0]`.
    let predicted = &result.streams[0].cardinality;

    let items: Vec<i32> = m!(
        fn add(a: i32, v: i32) -> i32 {
            a + v
        }

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
    // Same design as `oracle_fold`: the `fn double` declaration is part
    // of the program in BOTH the analyzer input and the `m!()` body so
    // the analyzer sees the same AST shape the runtime executes.
    let result = marigold_grammar::marigold_analyze(
        "fn double(v: i32) -> i32 { v * 2 } range(0, 100).map(double).return",
    )
    .unwrap();
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
async fn oracle_keep_first_n() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 100).keep_first_n(3, compare).return")
            .unwrap();
    let predicted = &result.streams[0].cardinality;

    let sorter = |a: &i32, b: &i32| a.partial_cmp(b).unwrap();

    let items: Vec<i32> = m!(
        range(0, 100)
            .keep_first_n(3, sorter)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // keep_first_n(3) keeps at most 3 items
    assert_eq!(items.len(), 3);
}

#[tokio::test]
async fn oracle_select_all() {
    let result =
        marigold_grammar::marigold_analyze("select_all(range(0, 10), range(0, 20)).return")
            .unwrap();
    let predicted = &result.streams[0].cardinality;

    let items: Vec<i32> = m!(
        select_all(range(0, 10), range(0, 20)).return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_cardinality_matches(items.len(), predicted);
    // select_all combines both streams: 10 + 20 = 30
    assert_eq!(items.len(), 30);
}
