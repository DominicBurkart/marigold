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

    // TODO: add a test_read_file_csv integration test once the `io` feature is enabled
    // in tests/Cargo.toml (tracked in the open issue for read_file support).

    #[tokio::test]
    async fn test_enum_range() {
        let r = m!(
            enum Words { Hello, World, }
            range(Words).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r.len(), 2);
        assert_eq!(format!("{:?}", r[0]), "Hello");
        assert_eq!(format!("{:?}", r[1]), "World");
    }

    #[tokio::test]
    async fn test_enum_range_with_filter() {
        let r = m!(
            fn is_hello(w: Words) -> bool {
                matches!(w, Words::Hello)
            }

            enum Words { Hello, World, }
            range(Words)
                .filter(is_hello)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r.len(), 1);
        assert_eq!(format!("{:?}", r[0]), "Hello");
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
}
