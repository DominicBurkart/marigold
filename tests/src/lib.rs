#[cfg(test)]
mod oracle;

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

    // ── fold ─────────────────────────────────────────────────────────────────

    /// fold produces exactly one item (the final accumulator) and the value is
    /// the correct sum. This validates the end-to-end marifold → m!() path.
    #[tokio::test]
    async fn fold_sum_produces_single_item_with_correct_value() {
        fn add(acc: i32, v: i32) -> i32 {
            acc + v
        }

        let result = m!(
            range(0, 10)
                .fold(0, add)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        // 0+1+…+9 = 45; fold must yield exactly one element
        assert_eq!(result, vec![45i32]);
    }

    /// fold with an empty range yields the initial accumulator unchanged.
    #[tokio::test]
    async fn fold_empty_range_yields_init() {
        fn add(acc: i32, v: i32) -> i32 {
            acc + v
        }

        let result = m!(
            range(5, 5)
                .fold(99, add)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(result, vec![99i32]);
    }

    // ── filter + map chain ───────────────────────────────────────────────────

    /// filter then map: only even numbers survive the filter, then each is
    /// doubled. Validates that cardinality and values are correct after both
    /// a narrowing and a transforming step.
    #[tokio::test]
    async fn filter_then_map_even_doubled() {
        fn is_even(i: i32) -> bool {
            i % 2 == 0
        }
        fn double(i: i32) -> i32 {
            i * 2
        }

        let result = m!(
            range(0, 6)
                .filter(is_even)
                .map(double)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        // evens in [0,6): 0, 2, 4 → doubled: 0, 4, 8
        assert_eq!(result, vec![0i32, 4, 8]);
    }

    /// filter that rejects everything produces an empty stream (not a panic).
    #[tokio::test]
    async fn filter_rejects_all_yields_empty_stream() {
        fn always_false(_: i32) -> bool {
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

        assert!(result.is_empty());
    }

    // ── keep_first_n via m!() ────────────────────────────────────────────────

    /// keep_first_n selects the N largest values and returns them in
    /// descending order. End-to-end via the m!() macro.
    #[tokio::test]
    async fn keep_first_n_largest_values() {
        let cmp = |a: &i32, b: &i32| a.cmp(b);

        let result = m!(
            range(0, 10)
                .keep_first_n(3, cmp)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(result, vec![9i32, 8, 7]);
    }

    /// keep_first_n with n larger than the stream returns all items in
    /// descending order — no panic, no missing elements.
    #[tokio::test]
    async fn keep_first_n_larger_than_stream_returns_all() {
        let cmp = |a: &i32, b: &i32| a.cmp(b);

        let result = m!(
            range(0, 4)
                .keep_first_n(100, cmp)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(result, vec![3i32, 2, 1, 0]);
    }

    // ── multi-consumer (stream variable reuse) ───────────────────────────────

    /// A named stream variable can be consumed by two independent output
    /// pipelines. Both pipelines see all items from the original source.
    /// This exercises the MultiConsumerStream broadcast path end-to-end.
    #[tokio::test]
    async fn multi_consumer_both_branches_see_all_items() {
        fn is_even(i: i32) -> bool {
            i % 2 == 0
        }
        fn is_odd(i: i32) -> bool {
            i % 2 != 0
        }

        let evens = m!(
            digits = range(0, 10)
            digits.filter(is_even).return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        let odds = m!(
            digits = range(0, 10)
            digits.filter(is_odd).return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(evens, vec![0i32, 2, 4, 6, 8]);
        assert_eq!(odds, vec![1i32, 3, 5, 7, 9]);
    }

    /// A stream variable used with fold produces the correct aggregate.
    /// Validates that the variable definition → fold → single-item output
    /// path works correctly end-to-end.
    #[tokio::test]
    async fn stream_variable_fold_produces_correct_sum() {
        fn add(acc: i32, v: i32) -> i32 {
            acc + v
        }

        let result = m!(
            nums = range(1, 6)
            nums.fold(0, add).return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        // 1+2+3+4+5 = 15
        assert_eq!(result, vec![15i32]);
    }
}
