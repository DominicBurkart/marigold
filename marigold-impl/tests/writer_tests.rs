//! Tests for `Writer` (writer.rs). Requires the `io` feature.
//!
//! Run with: cargo test --features tokio,io

// Skip compilation when the `io` feature is absent; the file is intentionally
// empty in that configuration.
#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use tokio::io::AsyncWriteExt;

// ---------------------------------------------------------------------------
// Vector backend
// ---------------------------------------------------------------------------

#[tokio::test]
async fn vector_writer_write_and_flush() {
    let mut w = Writer::vector();
    w.write_all(b"hello, marigold").await.unwrap();
    w.flush().await.unwrap();
}

#[tokio::test]
async fn vector_writer_write_and_shutdown() {
    let mut w = Writer::vector();
    w.write_all(b"shutdown test").await.unwrap();
    w.shutdown().await.unwrap();
}

#[tokio::test]
async fn vector_writer_empty_write_flush_shutdown() {
    let mut w = Writer::vector();
    w.write_all(b"").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
}

#[test]
fn vector_writer_debug_format() {
    let w = Writer::vector();
    let s = format!("{:?}", w);
    assert!(!s.is_empty());
}

// ---------------------------------------------------------------------------
// File backend
// ---------------------------------------------------------------------------

#[tokio::test]
async fn file_writer_write_flush_and_verify() {
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let path = tmp.path().to_owned();
    let file = tokio::fs::File::create(&path).await.unwrap();
    let mut w = Writer::file(file);
    w.write_all(b"marigold file writer test").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
    let contents = tokio::fs::read_to_string(&path).await.unwrap();
    assert_eq!(contents, "marigold file writer test");
}

#[test]
fn file_writer_debug_format() {
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let std_file = tmp.reopen().unwrap();
    let tokio_file = tokio::fs::File::from_std(std_file);
    let w = Writer::file(tokio_file);
    let s = format!("{:?}", w);
    assert!(!s.is_empty());
}
