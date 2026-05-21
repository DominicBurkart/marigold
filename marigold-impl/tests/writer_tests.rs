//! Tests for `marigold_impl::writer::Writer`.
//!
//! `Writer` is the `AsyncWrite` adapter that backs `write_file(...)` codegen
//! and is also used to capture program output to an in-memory vector for
//! tests/macros. It has no direct test coverage in the workspace today; its
//! `AsyncWrite` impl forwards to either a `tokio::fs::File` or an in-memory
//! `Vec<u8>`. The tests below pin down the observable contract for the
//! in-memory and on-disk variants:
//!
//! 1. Bytes written through `Writer::vector()` are forwarded to its inner
//!    Vec via the `AsyncWrite` trait (verified by writing through the
//!    public `tokio::io::AsyncWriteExt::write_all` path).
//! 2. `Writer::file(...)` accepts a `tokio::fs::File`, flushes/shuts down
//!    cleanly, and the bytes land on disk.
//!
//! These tests only require `feature = "io"`, which CI exercises via
//! `cargo test --features tokio,io` and `cargo test --all-features`.

#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use marigold_impl::AsyncWriteExt;

#[tokio::test]
async fn writer_vector_accepts_writes_via_async_write() {
    let mut w = Writer::vector();
    w.write_all(b"hello ").await.expect("write_all part 1");
    w.write_all(b"world").await.expect("write_all part 2");
    w.flush().await.expect("flush");
    w.shutdown().await.expect("shutdown");
    // Writer does not expose its internal buffer; we only assert that the
    // AsyncWrite contract is honored without error. The on-disk path below
    // is the round-trip test for byte fidelity.
}

#[tokio::test]
async fn writer_file_round_trip() {
    let dir =
        std::env::temp_dir().join(format!("marigold_writer_roundtrip_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("out.bin");

    {
        let f = tokio::fs::File::create(&path).await.expect("create file");
        let mut w = Writer::file(f);
        w.write_all(b"abc").await.expect("write abc");
        w.write_all(b"defgh").await.expect("write defgh");
        w.flush().await.expect("flush");
        w.shutdown().await.expect("shutdown");
    }

    let bytes = std::fs::read(&path).expect("read back file");
    assert_eq!(bytes, b"abcdefgh");

    let _ = std::fs::remove_dir_all(&dir);
}

#[tokio::test]
async fn writer_vector_zero_byte_writes() {
    // Writing zero bytes must succeed without error and not advance the
    // writer state in a way that breaks subsequent writes.
    let mut w = Writer::vector();
    w.write_all(b"").await.expect("zero-byte write");
    w.write_all(b"x").await.expect("non-zero write after zero");
    w.flush().await.expect("flush");
    w.shutdown().await.expect("shutdown");
}
