//! Integration tests for `marigold_impl::writer::Writer`.
//!
//! Exercises all six code paths in the `AsyncWrite` implementation:
//! `poll_write`, `poll_flush`, and `poll_shutdown` for both the
//! `WriteTarget::Vector` and `WriteTarget::File` backends.
//!
//! These tests are compiled only when the `io` feature is active (which also
//! pulls in `tokio/fs`). The tokio runtime is available unconditionally via
//! the dev-dependency on `tokio` with `features = ["full"]`.

#[cfg(feature = "io")]
mod writer_tests {
    use marigold_impl::writer::Writer;
    use tokio::io::AsyncWriteExt;

    // -- Vector backend -------------------------------------------------------

    #[tokio::test]
    async fn vector_write_flush_shutdown() {
        let mut w = Writer::vector();
        // poll_write
        w.write_all(b"hello world").await.unwrap();
        // poll_flush
        w.flush().await.unwrap();
        // poll_shutdown
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_write_empty_bytes() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"first").await.unwrap();
        w.write_all(b"second").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    // -- File backend ---------------------------------------------------------

    #[tokio::test]
    async fn file_write_flush_shutdown() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("out.txt");

        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        // poll_write
        w.write_all(b"file-content").await.unwrap();
        // poll_flush
        w.flush().await.unwrap();
        // poll_shutdown
        w.shutdown().await.unwrap();

        let contents = std::fs::read_to_string(&path).unwrap();
        assert_eq!(contents, "file-content");
    }

    #[tokio::test]
    async fn file_write_empty() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("empty.txt");

        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();

        let contents = std::fs::read_to_string(&path).unwrap();
        assert_eq!(contents, "");
    }

    #[tokio::test]
    async fn file_multiple_writes_accumulate() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("multi.txt");

        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"hello ").await.unwrap();
        w.write_all(b"world").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();

        let contents = std::fs::read_to_string(&path).unwrap();
        assert_eq!(contents, "hello world");
    }
}
