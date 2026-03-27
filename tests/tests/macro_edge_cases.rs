//! Integration tests for the m! macro covering edge cases and
//! compositions that are not exercised by the existing test suite.

use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::test]
async fn empty_range_returns_empty_stream() {
    let r = m!(
        range(0, 0).return
    )
    .await
    .collect::<Vec<i32>>()
    .await;
    assert!(r.is_empty(), "range(0,0) should yield no elements");
}

#[tokio::test]
async fn single_element_range() {
    let r = m!(
        range(0, 1).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![0]);
}

#[tokio::test]
async fn inclusive_range_single_element() {
    let r = m!(
        range(5, =5).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![5]);
}

#[tokio::test]
async fn fold_sum() {
    let r = m!(
        range(1, =5)
            .fold(0, |acc, x| acc + x)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![15], "1+2+3+4+5 = 15");
}

#[tokio::test]
async fn fold_empty_stream_returns_init() {
    let r = m!(
        range(0, 0)
            .fold(42, |acc, _x| acc)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![42], "fold over empty stream should yield the initial value");
}

#[tokio::test]
async fn chained_map_filter() {
    fn triple(v: i32) -> i32 {
        v * 3
    }
    fn is_even(v: i32) -> bool {
        v % 2 == 0
    }

    let r = m!(
        range(0, 5)
            .map(triple)
            .filter(is_even)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    // 0*3=0(even), 1*3=3(odd), 2*3=6(even), 3*3=9(odd), 4*3=12(even)
    assert_eq!(r, vec![0, 6, 12]);
}

#[tokio::test]
async fn filter_rejects_all() {
    fn always_false(_v: i32) -> bool {
        false
    }

    let r = m!(
        range(0, 5)
            .filter(always_false)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert!(r.is_empty(), "filter that rejects everything should yield empty stream");
}

#[tokio::test]
async fn filter_accepts_all() {
    fn always_true(_v: i32) -> bool {
        true
    }

    let r = m!(
        range(0, 3)
            .filter(always_true)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![0, 1, 2]);
}

#[tokio::test]
async fn map_identity() {
    fn identity(v: i32) -> i32 {
        v
    }

    let r = m!(
        range(0, 4)
            .map(identity)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![0, 1, 2, 3]);
}

#[tokio::test]
async fn combinations_of_single_element() {
    let r = m!(
        range(0, 1)
            .combinations(1)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![[0i32]]);
}

#[tokio::test]
async fn permutations_of_single_element() {
    let r = m!(
        range(0, 1)
            .permutations(1)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r, vec![[0i32]]);
}

#[tokio::test]
async fn select_all_single_stream() {
    let r = m!(
        select_all(range(0, 3)).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    let mut sorted = r.clone();
    sorted.sort();
    assert_eq!(sorted, vec![0, 1, 2]);
}

#[tokio::test]
async fn chained_maps() {
    fn double(v: i32) -> i32 {
        v * 2
    }
    fn inc(v: i32) -> i32 {
        v + 1
    }

    let r = m!(
        range(0, 3)
            .map(double)
            .map(inc)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    // 0->0->1, 1->2->3, 2->4->5
    assert_eq!(r, vec![1, 3, 5]);
}

#[tokio::test]
async fn large_range_count() {
    let r = m!(
        range(0, 1000).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(r.len(), 1000);
    assert_eq!(r[0], 0);
    assert_eq!(r[999], 999);
}
