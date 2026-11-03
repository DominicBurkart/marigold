#![cfg(feature = "io")]

use marigold::m;
use serde::Serialize;
use std::time::Duration;

#[derive(Serialize)]
struct Row {
    a: i32,
    b: i32,
}

fn to_row(p: [i32; 2]) -> Row {
    Row { a: p[0], b: p[1] }
}

fn sorted_lines(path: &str) -> Vec<String> {
    let mut lines: Vec<String> = std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect();
    lines.sort();
    lines
}

#[tokio::test]
async fn named_collecting_step_before_write_file_matches_inline() {
    let inline_path = "/tmp/marigold-436-inline.csv";
    let named_path = "/tmp/marigold-436-named.csv";
    let _ = std::fs::remove_file(inline_path);
    let _ = std::fs::remove_file(named_path);

    tokio::time::timeout(
        Duration::from_secs(20),
        m!(range(0, 4).permutations(2).map(to_row).write_file(
            "/tmp/marigold-436-inline.csv",
            csv,
            compression = none
        )),
    )
    .await
    .expect("HANG: inline write_file did not complete");

    tokio::time::timeout(
        Duration::from_secs(20),
        m!(
            x = range(0, 4)
            x.permutations(2).map(to_row).write_file("/tmp/marigold-436-named.csv", csv, compression=none)
        ),
    )
    .await
    .expect("HANG: named write_file did not complete");

    let inline = sorted_lines(inline_path);
    let named = sorted_lines(named_path);
    let _ = std::fs::remove_file(inline_path);
    let _ = std::fs::remove_file(named_path);
    assert_eq!(inline.len(), 13);
    assert_eq!(named, inline);
}
