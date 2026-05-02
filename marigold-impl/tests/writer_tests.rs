#[cfg(feature = "io")]
mod tests {
    use marigold_impl::writer::Writer;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn vector_writer_write_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_sequential_writes() {
        let mut w = Writer::vector();
        w.write_all(b"chunk1").await.unwrap();
        w.write_all(b"chunk2").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn file_writer_write_and_flush() {
        let path = std::env::temp_dir().join(format!(
            "marigold_writer_test_{}.bin",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"test data").await.unwrap();
        w.flush().await.unwrap();
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[tokio::test]
    async fn file_writer_shutdown() {
        let path = std::env::temp_dir().join(format!(
            "marigold_writer_shutdown_{}.bin",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"shutdown test").await.unwrap();
        w.shutdown().await.unwrap();
        let _ = tokio::fs::remove_file(&path).await;
    }
}
