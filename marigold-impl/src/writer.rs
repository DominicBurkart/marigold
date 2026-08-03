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
    use super::Writer;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn vector_write_all() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.unwrap();
    }

    #[tokio::test]
    async fn vector_flush() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"shutdown test").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_write_empty() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_write_large() {
        let mut w = Writer::vector();
        let data = vec![0xABu8; 1024 * 64];
        w.write_all(&data).await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn file_write_all() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test_write.bin");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"file writer test").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"file writer test");
    }

    #[tokio::test]
    async fn file_write_empty() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test_empty.bin");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"");
    }

    #[tokio::test]
    async fn file_write_large() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test_large.bin");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        let data = vec![0x42u8; 1024 * 128];
        w.write_all(&data).await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, data);
    }
}
