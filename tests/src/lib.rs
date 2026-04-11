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

    // Note: This test is disabled until the `io` feature is enabled in tests/Cargo.toml
    // and the Pest parser implementation for read_file is complete.
    // This test documents the expected behavior for the read_file integration test.
    //
    // #[tokio::test]
    // async fn test_read_file_csv() {
    //     use std::io::Write;
    //
    //     #[derive(Debug, serde::Deserialize, PartialEq)]
    //     struct TestData {
    //         id: i32,
    //         name: String,
    //     }
    //
    //     let temp_dir = std::env::temp_dir();
    //     let file_path = temp_dir.join("test_data.csv");
    //     let file_path_str = file_path.to_str().unwrap();
    //
    //     {
    //         let mut file = std::fs::File::create(&file_path).unwrap();
    //         writeln!(file, "id,name").unwrap();
    //         writeln!(file, "1,Alice").unwrap();
    //         writeln!(file, "2,Bob").unwrap();
    //     }
    //
    //     let result = m!(
    //         read_file(file_path_str, csv, struct=TestData).return
    //     )
    //     .await
    //     .collect::<Vec<Result<TestData, _>>>()
    //     .await;
    //
    //     std::fs::remove_file(&file_path).ok();
    //
    //     assert_eq!(result.len(), 2);
    //     assert_eq!(
    //         result[0].as_ref().unwrap(),
    //         &TestData {
    //             id: 1,
    //             name: "Alice".to_string()
    //         }
    //     );
    //     assert_eq!(
    //         result[1].as_ref().unwrap(),
    //         &TestData {
    //             id: 2,
    //             name: "Bob".to_string()
    //         }
    //     );
    // }

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

    // combinations via m!
    // Note: the m! macro generates fixed-size array outputs for combinations;
    // type annotations use array syntax [T; k].

    /// Combinations of range(0, 4) choose 2 should yield C(4,2) = 6 items.
    #[tokio::test]
    async fn test_combinations_count() {
        let result = m!(
            range(0, 4).combinations(2).return
        )
        .await
        .collect::<Vec<[i32; 2]>>()
        .await;
        assert_eq!(result.len(), 6);
    }

    /// Combinations in lexicographic order from range(0, 3) choose 2.
    #[tokio::test]
    async fn test_combinations_values() {
        let result = m!(
            range(0, 3).combinations(2).return
        )
        .await
        .collect::<Vec<[i32; 2]>>()
        .await;
        assert_eq!(result, [[0i32, 1], [0, 2], [1, 2]]);
    }

    // keep_first_n via m!
    // Note: the m! macro grammar requires a named comparator function, not an inline closure.

    /// keep_first_n retains the N largest elements in descending order.
    #[tokio::test]
    async fn test_keep_first_n_largest() {
        let by_value = |a: &i32, b: &i32| a.cmp(b);
        let result = m!(
            range(0, 10).keep_first_n(3, by_value).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![9, 8, 7]);
    }

    /// keep_first_n when n >= stream length returns all items in descending order.
    #[tokio::test]
    async fn test_keep_first_n_larger_than_stream() {
        let by_value = |a: &i32, b: &i32| a.cmp(b);
        let result = m!(
            range(0, 4).keep_first_n(10, by_value).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![3, 2, 1, 0]);
    }

    // chained map + filter via m!

    /// Applying map then filter composes correctly end-to-end.
    #[tokio::test]
    async fn test_map_then_filter() {
        fn double(v: i32) -> i32 {
            v * 2
        }
        fn is_gt_four(v: i32) -> bool {
            v > 4
        }

        let result = m!(
            range(0, 6)
                .map(double)
                .filter(is_gt_four)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        // doubled: 0,2,4,6,8,10  then filtered >4: 6,8,10
        assert_eq!(result, vec![6, 8, 10]);
    }
}
