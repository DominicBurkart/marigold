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
        assert_eq!(r, vec![vec![vec![0, 1], vec![1, 0]]]);
    }

    #[tokio::test]
    async fn sort_by_external_function() {
        let sorter = |a: &Vec<i32>, b: &Vec<i32>| a[0].partial_cmp(&b[0]).unwrap();
        let r = m!(
            range(0, 2)
                .permutations(2)
                .keep_first_n(1, sorter)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        assert_eq!(r, vec![vec![1, 0]]);
    }

    #[tokio::test]
    async fn test_filter() {
        fn is_odd_number(i: &i32) -> bool {
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
                vec![0, 0],
                vec![0, 1],
                vec![0, 2],
                vec![1, 0],
                vec![1, 1],
                vec![1, 2],
                vec![2, 0],
                vec![2, 1],
                vec![2, 2]
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
}
