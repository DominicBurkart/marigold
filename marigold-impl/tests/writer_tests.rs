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

    /// `Writer::vector()`: multiple sequential writes succeed.
    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.expect("first write failed");
        w.write_all(b"bar").await.expect("second write failed");
        w.shutdown().await.expect("shutdown failed");
    }

    /// `Writer::file()`: write bytes, shutdown, then read back to verify contents.
    #[tokio::test]
    async fn file_writer_write_and_read_back() {
        let path =
            std::env::temp_dir().join(format!("marigold_writer_test_{}.txt", std::process::id()));

        // Write via Writer.
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

        // Read back and verify.
        let contents = tokio::fs::read(&path)
            .await
            .expect("failed to read temp file");
        assert_eq!(contents, b"marigold test data");

        // Clean up.
        let _ = tokio::fs::remove_file(&path).await;
    }

    /// `Writer::file()`: multiple writes produce the correct concatenated content.
    #[tokio::test]
    async fn file_writer_multiple_writes_concat() {
        let path =
            std::env::temp_dir().join(format!("marigold_writer_multi_{}.txt", std::process::id()));

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

        let _ = tokio::fs::remove_file(&path).await;
    }
}
