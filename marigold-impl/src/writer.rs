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

    // Both WriteTarget arms (Vector and File) must be exercised to satisfy
    // codecov's 100 % patch requirement on this module.

    #[tokio::test]
    async fn vector_writer_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.expect("write_all failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let dir = tempfile::tempdir().expect("tempdir");
        let path = dir.path().join("out.bin");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create file");
        let mut w = Writer::file(file);
        w.write_all(b"world").await.expect("write_all failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");

        let bytes = tokio::fs::read(&path).await.expect("read back");
        assert_eq!(bytes, b"world");
    }

    #[tokio::test]
    async fn file_writer_multiple_writes() {
        let dir = tempfile::tempdir().expect("tempdir");
        let path = dir.path().join("multi.bin");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"abc").await.unwrap();
        w.write_all(b"def").await.unwrap();
        w.flush().await.unwrap();

        let bytes = tokio::fs::read(&path).await.unwrap();
        assert_eq!(bytes, b"abcdef");
    }
}
