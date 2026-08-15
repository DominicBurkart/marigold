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

    /// `Writer::vector()` must accept writes and flush without errors.
    #[tokio::test]
    async fn vector_writer_write_and_flush() {
        let mut w = Writer::vector();
        let bytes = b"hello, marigold";
        let n = w.write(bytes).await.expect("write should succeed");
        assert_eq!(n, bytes.len());
        w.flush().await.expect("flush should succeed");
    }

    /// Shutdown (EOF signal) on the vector writer must not error.
    #[tokio::test]
    async fn vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"data")
            .await
            .expect("write_all should succeed");
        w.shutdown().await.expect("shutdown should succeed");
    }

    /// Multiple sequential writes accumulate without error.
    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        for chunk in &[b"foo" as &[u8], b"bar", b"baz"] {
            w.write_all(chunk).await.expect("write_all should succeed");
        }
        w.flush().await.expect("flush should succeed");
    }

    /// Write of an empty slice succeeds and reports zero bytes.
    #[tokio::test]
    async fn vector_writer_empty_write() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.expect("empty write should succeed");
        assert_eq!(n, 0);
    }
}
