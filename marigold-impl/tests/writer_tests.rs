//! Tests for `Writer` (requires the `io` feature).

#[cfg(feature = "io")]
mod io_tests {
    use marigold_impl::writer::Writer;
    use tokio::io::AsyncWriteExt;

    /// `Writer::vector()`: write bytes, flush, and shutdown all succeed without error.
    #[tokio::test]
    async fn vector_writer_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"hello, world")
            .await
            .expect("write_all failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");
    }

    /// `Writer::vector()`: multiple sequential writes succeed AND the buffered
    /// bytes match the concatenated input. Inspects the inner buffer via
    /// `Writer::into_vec()` so we exercise content correctness, not just the
    /// fact that the AsyncWrite calls returned `Ok`.
    #[tokio::test]
    async fn vector_writer_buffers_concatenated_content() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.expect("first write failed");
        w.write_all(b"bar").await.expect("second write failed");
        w.write_all(b"baz").await.expect("third write failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");

        let buf = w
            .into_vec()
            .expect("Writer::vector should expose its inner Vec");
        assert_eq!(buf, b"foobarbaz");
    }

    /// `Writer::vector()`: an empty writer (no writes) yields an empty buffer.
    #[tokio::test]
    async fn vector_writer_empty_yields_empty_buffer() {
        let w = Writer::vector();
        let buf = w.into_vec().expect("vector variant must yield Some");
        assert!(buf.is_empty());
    }

    /// `Writer::file()`: write bytes, shutdown, then read back to verify contents.
    /// Uses `tempfile::TempDir` so paths are unique per test run and the
    /// directory is removed on drop — no PID-based collision risk across
    /// concurrent test runs.
    #[tokio::test]
    async fn file_writer_write_and_read_back() {
        let dir = tempfile::TempDir::new().expect("tempdir create failed");
        let path = dir.path().join("writer.txt");

        {
            let file = tokio::fs::File::create(&path)
                .await
                .expect("failed to create temp file");
            let mut w = Writer::file(file);
            w.write_all(b"marigold test data")
                .await
                .expect("write_all failed");
            w.shutdown().await.expect("shutdown failed");
        }

        let contents = tokio::fs::read(&path)
            .await
            .expect("failed to read temp file");
        assert_eq!(contents, b"marigold test data");
    }

    /// `Writer::file()`: multiple writes produce the correct concatenated content.
    #[tokio::test]
    async fn file_writer_multiple_writes_concat() {
        let dir = tempfile::TempDir::new().expect("tempdir create failed");
        let path = dir.path().join("writer_multi.txt");

        {
            let file = tokio::fs::File::create(&path)
                .await
                .expect("failed to create temp file");
            let mut w = Writer::file(file);
            w.write_all(b"hello, ").await.expect("first write failed");
            w.write_all(b"world").await.expect("second write failed");
            w.flush().await.expect("flush failed");
            w.shutdown().await.expect("shutdown failed");
        }

        let contents = tokio::fs::read(&path)
            .await
            .expect("failed to read temp file");
        assert_eq!(contents, b"hello, world");
    }

    /// `Writer::file()`: a 64 KB write round-trips cleanly through the file.
    #[tokio::test]
    async fn file_writer_large_write_round_trip() {
        let dir = tempfile::TempDir::new().expect("tempdir create failed");
        let path = dir.path().join("writer_large.bin");

        let payload: Vec<u8> = (0..64 * 1024).map(|i| (i % 251) as u8).collect();

        {
            let file = tokio::fs::File::create(&path)
                .await
                .expect("failed to create temp file");
            let mut w = Writer::file(file);
            w.write_all(&payload).await.expect("write_all failed");
            w.shutdown().await.expect("shutdown failed");
        }

        let contents = tokio::fs::read(&path)
            .await
            .expect("failed to read temp file");
        assert_eq!(contents, payload);
    }

    /// `Writer::file()` does NOT report content via `into_vec()` — that accessor
    /// only applies to the `vector()` variant.
    #[tokio::test]
    async fn file_writer_into_vec_returns_none() {
        let dir = tempfile::TempDir::new().expect("tempdir create failed");
        let path = dir.path().join("writer_none.txt");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("failed to create temp file");
        let w = Writer::file(file);
        assert!(w.into_vec().is_none());
    }
}
