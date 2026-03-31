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

    // --- New tests below ---

    #[tokio::test]
    async fn test_empty_range() {
        let result = m!(
            range(0, 0).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert!(result.is_empty(), "range(0,0) should produce empty stream");
    }

    #[tokio::test]
    async fn test_single_element_range() {
        let result = m!(
            range(0, 1).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0]);
    }

    #[tokio::test]
    async fn test_negative_range() {
        let result = m!(
            range(-3, 3).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![-3, -2, -1, 0, 1, 2]);
    }

    #[tokio::test]
    async fn test_fold_sum() {
        fn add(acc: i32, x: i32) -> i32 {
            acc + x
        }

        let result = m!(
            range(1, 6)
                .fold(0, add)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![15], "1+2+3+4+5 = 15");
    }

    #[tokio::test]
    async fn test_chained_maps() {
        fn double(v: i32) -> i32 {
            v * 2
        }
        fn inc(v: i32) -> i32 {
            v + 1
        }

        let result = m!(
            range(0, 3)
                .map(double)
                .map(inc)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![1, 3, 5]);
    }

    #[tokio::test]
    async fn test_combinations_standalone() {
        let result = m!(
            range(0, 4)
                .combinations(2)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        // C(4,2) = 6
        assert_eq!(result.len(), 6);
        assert_eq!(result[0], [0i32, 1]);
        assert_eq!(result[5], [2i32, 3]);
    }

    #[tokio::test]
    async fn test_map_identity() {
        fn identity(v: i32) -> i32 {
            v
        }

        let result = m!(
            range(0, 5)
                .map(identity)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0, 1, 2, 3, 4]);
    }

    #[tokio::test]
    async fn test_filter_none_match() {
        fn always_false(_i: i32) -> bool {
            false
        }

        let result = m!(
            range(0, 10)
                .filter(always_false)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert!(result.is_empty(), "Filter that matches nothing should produce empty stream");
    }

    #[tokio::test]
    async fn test_filter_all_match() {
        fn always_true(_i: i32) -> bool {
            true
        }

        let result = m!(
            range(0, 5)
                .filter(always_true)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![0, 1, 2, 3, 4]);
    }

    #[tokio::test]
    async fn test_large_range() {
        let result = m!(
            range(0, 1000).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result.len(), 1000);
        assert_eq!(result[0], 0);
        assert_eq!(result[999], 999);
    }

    #[tokio::test]
    async fn test_fn_declaration_in_macro() {
        let result = m!(
            fn square(x: i32) -> i32 {
                x * x
            }

            range(1, 5)
                .map(square)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![1, 4, 9, 16]);
    }

    #[tokio::test]
    async fn test_inclusive_range_single() {
        let result = m!(
            range(5, =5).return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(result, vec![5]);
    }

    #[tokio::test]
    async fn test_select_all_three_streams() {
        let result = m!(
            select_all(range(0, 2), range(10, 12), range(20, 22)).return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        // select_all ordering is non-deterministic, so sort before comparing.
        let mut sorted = result.clone();
        sorted.sort();
        assert_eq!(sorted, vec![0, 1, 10, 11, 20, 21]);
    }
}
