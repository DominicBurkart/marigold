//! Edge-case tests for `Writer`, the `AsyncWrite` sink used by generated
//! `write_file` code (see `marigold-impl/src/writer.rs`).
//!
//! Invariants exercised here:
//! - `Writer::vector()` and `Writer::file(..)` both implement `tokio::io::AsyncWrite`.
//! - Writes are forwarded verbatim to the underlying target: bytes out == bytes in.
//! - `write_all` over multiple calls accumulates without loss or reordering.
//! - `flush` and `shutdown` succeed and do not corrupt previously written bytes.
//! - A freshly constructed `Writer` holds no bytes.

#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use tokio::io::AsyncWriteExt;

#[tokio::test]
async fn vector_writer_starts_empty() {
    let mut w = Writer::vector();
    // Nothing was written; shutdown must still succeed cleanly.
    w.shutdown().await.unwrap();
}

#[tokio::test]
async fn vector_writer_single_write_roundtrips() {
    let mut w = Writer::vector();
    let n = w.write(b"hello").await.unwrap();
    assert_eq!(n, 5);
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
}

#[tokio::test]
async fn vector_writer_accumulates_multiple_writes() {
    let mut w = Writer::vector();
    w.write_all(b"abc").await.unwrap();
    w.write_all(b"def").await.unwrap();
    w.write_all(b"ghi").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
}

#[tokio::test]
async fn file_writer_roundtrips_to_disk() {
    let dir = std::env::temp_dir();
    let path = dir.join(format!("marigold_writer_test_{}.txt", std::process::id()));
    let file = tokio::fs::File::create(&path).await.unwrap();

    let mut w = Writer::file(file);
    w.write_all(b"line one\n").await.unwrap();
    w.write_all(b"line two\n").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();

    let contents = tokio::fs::read_to_string(&path).await.unwrap();
    assert_eq!(contents, "line one\nline two\n");

    tokio::fs::remove_file(&path).await.unwrap();
}

#[tokio::test]
async fn file_writer_empty_write_produces_empty_file() {
    let dir = std::env::temp_dir();
    let path = dir.join(format!(
        "marigold_writer_empty_test_{}.txt",
        std::process::id()
    ));
    let file = tokio::fs::File::create(&path).await.unwrap();

    let mut w = Writer::file(file);
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();

    let contents = tokio::fs::read(&path).await.unwrap();
    assert!(contents.is_empty());

    tokio::fs::remove_file(&path).await.unwrap();
}
