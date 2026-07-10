#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use tokio::io::AsyncWriteExt;

#[tokio::test]
async fn test_writer_vector_write() {
    let mut w = Writer::vector();
    w.write_all(b"hello world").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
}

#[tokio::test]
async fn test_writer_file_write() {
    let path = std::env::temp_dir().join("marigold_writer_test_file.tmp");
    let file = tokio::fs::File::create(&path).await.unwrap();
    let mut w = Writer::file(file);
    w.write_all(b"test data for Writer::file").await.unwrap();
    w.flush().await.unwrap();
    w.shutdown().await.unwrap();
    let _ = tokio::fs::remove_file(&path).await;
}
