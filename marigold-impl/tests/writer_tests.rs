#[cfg(feature = "io")]
mod io_tests {
    use marigold_impl::writer::Writer;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn vector_write_all_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"hello, world").await.unwrap();
    }

    #[tokio::test]
    async fn vector_flush_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn file_write_flush_shutdown() {
        let path = std::env::temp_dir().join(format!(
            "marigold_writer_test_{}.tmp",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.subsec_nanos())
                .unwrap_or(42)
        ));
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"test payload").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();

        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"test payload");
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[tokio::test]
    async fn vector_write_empty_bytes() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }
}
