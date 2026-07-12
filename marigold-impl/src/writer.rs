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
    async fn vector_write_all_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.unwrap();
    }

    #[tokio::test]
    async fn vector_write_empty_bytes() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
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
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"first").await.unwrap();
        w.write_all(b"second").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn file_write_all_succeeds() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"file content").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let written = std::fs::read(&path).unwrap();
        assert_eq!(written, b"file content");
    }

    #[tokio::test]
    async fn file_shutdown_succeeds() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("shutdown_test.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"x").await.unwrap();
        w.shutdown().await.unwrap();
    }
}
