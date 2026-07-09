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

#[cfg(all(test, feature = "io"))]
mod tests {
    use super::*;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn test_vector_write_returns_byte_count() {
        let mut writer = Writer::vector();
        let data = b"hello, world";
        let n = writer.write(data).await.unwrap();
        assert_eq!(n, data.len());
    }

    #[tokio::test]
    async fn test_vector_write_all() {
        let mut writer = Writer::vector();
        writer.write_all(b"complete data write").await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_flush() {
        let mut writer = Writer::vector();
        writer.write_all(b"buffered data").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"data before shutdown").await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_empty_write() {
        let mut writer = Writer::vector();
        let n = writer.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn test_vector_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"first ").await.unwrap();
        writer.write_all(b"second ").await.unwrap();
        writer.write_all(b"third").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_write_all() {
        let path = std::env::temp_dir().join("marigold_writer_test.tmp");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"file test data").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_file_write_returns_byte_count() {
        let path = std::env::temp_dir().join("marigold_writer_byte_count_test.tmp");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        let data = b"byte count test";
        let n = writer.write(data).await.unwrap();
        assert_eq!(n, data.len());
        tokio::fs::remove_file(&path).await.ok();
    }
}
