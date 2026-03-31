#[cfg(test)]
mod tests {
    use marigold::m;
    use marigold::marigold_impl::StreamExt;

    #[tokio::test]
    async fn map() {
        let r = m!(
            fn double(v: i32) -> i32 {
                v * 2
            }

            range(0, 3)
                .map(double)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r, vec![0, 2, 4]);
    }

    #[tokio::test]
    async fn map_with_external_rust_function() {
        fn double(i: i32) -> i32 {
            i * 2
        }

        let r = m!(
            range(1, 4)
                .map(double)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r, vec![2, 4, 6]);
    }

    #[tokio::test]
    async fn chained_permutations_and_combinations() {
        let r = m!(
            range(0, 2)
                .permutations(2)
                .combinations(2)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r, vec![[[0i32, 1], [1i32, 0]]]);
    }

    #[tokio::test]
    async fn sort_by_external_function() {
        let sorter = |a: &[i32; 2], b: &[i32; 2]| a[0].partial_cmp(&b[0]).unwrap();
        let r = m!(
            range(0, 2)
                .permutations(2)
                .keep_first_n(1, sorter)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r, vec![[1i32, 0]]);
    }

    #[tokio::test]
    async fn test_filter() {
        fn is_odd_number(i: i32) -> bool {
            i % 2 == 1
        }

        assert_eq!(
            m!(
                range(0, 10)
                .filter(is_odd_number)
                .return
            )
            .await
            .collect::<Vec<_>>()
            .await,
            vec![1, 3, 5, 7, 9]
        );
    }

    #[tokio::test]
    async fn test_permutations_with_replacement() {
        assert_eq!(
            m!(
                range(0, 3)
                    .permutations_with_replacement(2)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await,
            vec![
                [0i32, 0],
                [0, 1],
                [0, 2],
                [1, 0],
                [1, 1],
                [1, 2],
                [2, 0],
                [2, 1],
                [2, 2]
            ]
        );
    }

    #[tokio::test]
    async fn test_select_all_multiple_streams() {
        let result = m!(
            select_all(range(0, 3), range(10, 13)).return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        let mut sorted = result.clone();
        sorted.sort();
        assert_eq!(sorted, vec![0, 1, 2, 10, 11, 12]);
    }

    #[tokio::test]
    async fn test_inclusive_range() {
        let result = m!(
            range(0, =3).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0, 1, 2, 3]);
    }

    // Case 1: basic chain preserves order (second stream follows first)
    #[tokio::test]
    async fn test_chain_basic() {
        let result = m!(
            range(0, 3)
                .chain(range(10, 13))
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0, 1, 2, 10, 11, 12]);
    }

    // Case 3: chain with empty first stream
    #[tokio::test]
    async fn test_chain_empty_first() {
        let result = m!(
            range(0, 0)
                .chain(range(1, 4))
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![1, 2, 3]);
    }

    // Case 4: chain with empty second stream
    #[tokio::test]
    async fn test_chain_empty_second() {
        let result = m!(
            range(1, 4)
                .chain(range(0, 0))
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![1, 2, 3]);
    }

    // Case 6: chain followed by filter
    #[tokio::test]
    async fn test_chain_then_filter() {
        fn is_even(i: i32) -> bool {
            i % 2 == 0
        }

        let result = m!(
            range(0, 3)
                .chain(range(10, 13))
                .filter(is_even)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0, 2, 10, 12]);
    }
}
