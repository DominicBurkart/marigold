//! Tests for the `writer` module (requires the `io` feature).
//!
//! These tests exercise `Writer::vector()` and `Writer::file()`, covering
//! the `WriteTarget::Vector` and `WriteTarget::File` arms of all three
//! `tokio::io::AsyncWrite` poll methods (`poll_write`, `poll_flush`,
//! `poll_shutdown`).

// Skip this entire file when the `io` feature is not enabled.
#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use tokio::io::AsyncWriteExt;

// ── Vector backend ──────────────────────────────────────────────────────────

/// `Writer::vector()` should accept a write of arbitrary bytes without error.
#[tokio::test]
async fn writer_vector_write_succeeds() {
    let mut w = Writer::vector();
    w.write_all(b"hello, vector writer")
        .await
        .expect("write_all should succeed for a vector writer");
}

/// `flush()` on a vector-backed writer should succeed (no-op in practice but
/// the poll path must be reachable).
#[tokio::test]
async fn writer_vector_flush_succeeds() {
    let mut w = Writer::vector();
    w.flush()
        .await
        .expect("flush should succeed for a vector writer");
}

/// `shutdown()` on a vector-backed writer should succeed.
#[tokio::test]
async fn writer_vector_shutdown_succeeds() {
    let mut w = Writer::vector();
    w.shutdown()
        .await
        .expect("shutdown should succeed for a vector writer");
}

/// Write followed by flush: exercises write then flush poll paths on the
/// Vector arm without duplicating the individual checks above.
#[tokio::test]
async fn writer_vector_write_then_flush() {
    let mut w = Writer::vector();
    w.write_all(b"write first, then flush")
        .await
        .expect("write should succeed");
    w.flush().await.expect("flush after write should succeed");
}

/// Write followed by shutdown: verifies the Vector arm can be cleanly closed
/// after data has been written.
#[tokio::test]
async fn writer_vector_write_then_shutdown() {
    let mut w = Writer::vector();
    w.write_all(b"write first, then shutdown")
        .await
        .expect("write should succeed");
    w.shutdown()
        .await
        .expect("shutdown after write should succeed");
}

// ── File backend ─────────────────────────────────────────────────────────────

/// `Writer::file()` should accept a write without error.
#[tokio::test]
async fn writer_file_write_succeeds() {
    let dir = std::env::temp_dir();
    let path = dir.join("marigold_writer_file_write_test.tmp");
    let f = tokio::fs::File::create(&path)
        .await
        .expect("create temp file");
    let mut w = Writer::file(f);
    w.write_all(b"hello, file writer")
        .await
        .expect("write_all should succeed for a file writer");
    let _ = tokio::fs::remove_file(&path).await;
}

/// `flush()` on a file-backed writer should succeed.
#[tokio::test]
async fn writer_file_flush_succeeds() {
    let dir = std::env::temp_dir();
    let path = dir.join("marigold_writer_file_flush_test.tmp");
    let f = tokio::fs::File::create(&path)
        .await
        .expect("create temp file");
    let mut w = Writer::file(f);
    w.flush()
        .await
        .expect("flush should succeed for a file writer");
    let _ = tokio::fs::remove_file(&path).await;
}

/// `shutdown()` on a file-backed writer should succeed.
#[tokio::test]
async fn writer_file_shutdown_succeeds() {
    let dir = std::env::temp_dir();
    let path = dir.join("marigold_writer_file_shutdown_test.tmp");
    let f = tokio::fs::File::create(&path)
        .await
        .expect("create temp file");
    let mut w = Writer::file(f);
    w.shutdown()
        .await
        .expect("shutdown should succeed for a file writer");
    let _ = tokio::fs::remove_file(&path).await;
}

/// Write followed by flush on a file backend: the most common usage pattern.
#[tokio::test]
async fn writer_file_write_then_flush() {
    let dir = std::env::temp_dir();
    let path = dir.join("marigold_writer_file_write_flush_test.tmp");
    let f = tokio::fs::File::create(&path)
        .await
        .expect("create temp file");
    let mut w = Writer::file(f);
    w.write_all(b"persisted data")
        .await
        .expect("write should succeed");
    w.flush().await.expect("flush should succeed");
    let _ = tokio::fs::remove_file(&path).await;
}

/// Write followed by shutdown on a file backend.
#[tokio::test]
async fn writer_file_write_then_shutdown() {
    let dir = std::env::temp_dir();
    let path = dir.join("marigold_writer_file_write_shutdown_test.tmp");
    let f = tokio::fs::File::create(&path)
        .await
        .expect("create temp file");
    let mut w = Writer::file(f);
    w.write_all(b"final write before shutdown")
        .await
        .expect("write should succeed");
    w.shutdown().await.expect("shutdown should succeed");
    let _ = tokio::fs::remove_file(&path).await;
}
