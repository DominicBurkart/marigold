//! Tests for `marigold_impl::writer::Writer`.
//!
//! The `writer` module is only compiled when the `io` feature is enabled.
//! All tests here are guarded with `#[cfg(feature = "io")]`.
//!
//! Coverage:
//! - `Writer::vector()` constructor
//! - `poll_write` via `AsyncWriteExt::write` / `write_all`
//! - `poll_flush` via `AsyncWriteExt::flush`
//! - `poll_shutdown` via `AsyncWriteExt::shutdown`
//! - `Writer::file()` constructor with a real temp file + round-trip read-back

#[cfg(feature = "io")]
mod writer_tests {
    use marigold_impl::writer::Writer;
    use tokio::io::AsyncWriteExt;

    // --- Vector target -------------------------------------------------------

    #[tokio::test]
    async fn writer_vector_write_returns_byte_count() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.expect("write should succeed");
        assert_eq!(n, 5, "should report 5 bytes written");
    }

    #[tokio::test]
    async fn writer_vector_write_all_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello world")
            .await
            .expect("write_all should succeed");
        w.flush().await.expect("flush should succeed");
    }

    #[tokio::test]
    async fn writer_vector_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.expect("write_all should succeed");
        w.shutdown().await.expect("shutdown should succeed");
    }

    #[tokio::test]
    async fn writer_vector_write_empty_slice() {
        let mut w = Writer::vector();
        // Vec::poll_write with empty buf returns Poll::Ready(Ok(0))
        let n = w.write(b"").await.expect("empty write should not error");
        assert_eq!(n, 0, "empty write should report 0 bytes");
    }

    #[tokio::test]
    async fn writer_vector_multiple_sequential_writes() {
        let mut w = Writer::vector();
        w.write_all(b"part1").await.expect("first write");
        w.write_all(b"part2").await.expect("second write");
        w.flush().await.expect("flush after multiple writes");
    }

    // --- File target ---------------------------------------------------------

    #[tokio::test]
    async fn writer_file_write_all_and_shutdown_round_trip() {
        let temp_dir = tempfile::tempdir().expect("tempdir");
        let path = temp_dir.path().join("output.bin");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");

        let mut w = Writer::file(file);
        w.write_all(b"test data")
            .await
            .expect("write_all to file should succeed");
        w.flush().await.expect("flush file should succeed");
        w.shutdown().await.expect("shutdown file should succeed");

        let contents = tokio::fs::read(&path).await.expect("read back temp file");
        assert_eq!(contents, b"test data", "file contents should match written bytes");
    }

    #[tokio::test]
    async fn writer_file_write_then_shutdown_preserves_data() {
        let temp_dir = tempfile::tempdir().expect("tempdir");
        let path = temp_dir.path().join("shutdown.txt");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");

        let mut w = Writer::file(file);
        w.write_all(b"before shutdown")
            .await
            .expect("write before shutdown");
        w.shutdown().await.expect("shutdown should succeed");

        let contents = tokio::fs::read(&path).await.expect("read back");
        assert_eq!(contents, b"before shutdown");
    }
}
