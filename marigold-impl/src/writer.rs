use std::io::Error;
use std::pin::Pin;

#[derive(Debug)]
enum WriteTarget {
    File(Pin<Box<tokio::fs::File>>),
    #[allow(clippy::box_collection)]
    Vector(Pin<Box<Vec<u8>>>),
}

#[derive(Debug)]
pub struct Writer {
    inner: WriteTarget,
}

impl Writer {
    pub fn file(f: tokio::fs::File) -> Self {
        Writer {
            inner: WriteTarget::File(Box::pin(f)),
        }
    }

    pub fn vector() -> Self {
        Writer {
            inner: WriteTarget::Vector(Box::pin(Vec::new())),
        }
    }
}

impl tokio::io::AsyncWrite for Writer {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
        buf: &[u8],
    ) -> core::task::Poll<Result<usize, Error>> {
        match &mut self.inner {
            WriteTarget::File(f) => f.as_mut().as_mut().poll_write(cx, buf),
            WriteTarget::Vector(v) => v.as_mut().as_mut().poll_write(cx, buf),
        }
    }

    fn poll_flush(
        mut self: Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Result<(), Error>> {
        match &mut self.inner {
            WriteTarget::File(f) => f.as_mut().as_mut().poll_flush(cx),
            WriteTarget::Vector(v) => v.as_mut().as_mut().poll_flush(cx),
        }
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Result<(), Error>> {
        match &mut self.inner {
            WriteTarget::File(f) => f.as_mut().as_mut().poll_shutdown(cx),
            WriteTarget::Vector(v) => v.as_mut().as_mut().poll_shutdown(cx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn test_vector_writer_write_returns_byte_count() {
        let mut writer = Writer::vector();
        let data = b"hello world";
        let n = writer.write(data).await.expect("write failed");
        assert_eq!(n, data.len());
    }

    #[tokio::test]
    async fn test_vector_writer_write_all() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"marigold stream")
            .await
            .expect("write_all failed");
    }

    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        writer.flush().await.expect("flush failed");
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        writer.shutdown().await.expect("shutdown failed");
    }

    #[tokio::test]
    async fn test_vector_writer_empty_write() {
        let mut writer = Writer::vector();
        let n = writer.write(b"").await.expect("empty write failed");
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn test_vector_writer_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"first ").await.unwrap();
        writer.write_all(b"second ").await.unwrap();
        writer.write_all(b"third").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_write() {
        let temp_path = std::env::temp_dir().join("marigold_writer_test.txt");
        let file = tokio::fs::File::create(&temp_path)
            .await
            .expect("could not create temp file");
        let mut writer = Writer::file(file);
        writer
            .write_all(b"marigold test content")
            .await
            .expect("file write failed");
        writer.flush().await.expect("file flush failed");
        writer.shutdown().await.expect("file shutdown failed");
        let _ = tokio::fs::remove_file(&temp_path).await;
    }
}
