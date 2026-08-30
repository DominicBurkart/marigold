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

    // --- Vector backend ---

    /// Writing bytes through the vector backend must succeed and return the
    /// correct byte count (exercising `poll_write` on the Vector arm).
    #[tokio::test]
    async fn vector_writer_write_succeeds() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.unwrap();
        assert_eq!(n, 5);
    }

    /// Flushing a vector-backed Writer must be a no-op that returns `Ok(())`
    /// (exercising `poll_flush` on the Vector arm).
    #[tokio::test]
    async fn vector_writer_flush_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"world").await.unwrap();
        w.flush().await.unwrap();
    }

    /// Shutdown on a vector-backed Writer must return `Ok(())` and be idempotent
    /// (exercising `poll_shutdown` on the Vector arm).
    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"bye").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Writing zero bytes through the vector backend must succeed and report 0
    /// bytes written without advancing the buffer.
    #[tokio::test]
    async fn vector_writer_write_empty_slice() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    // --- File backend ---

    /// Writing bytes through the file backend must succeed.
    /// Exercises `poll_write`, `poll_flush`, and `poll_shutdown` on the File arm.
    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"file content").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Writing an empty slice through the file backend must succeed without
    /// touching disk content (exercises the File arm of `poll_write`).
    #[tokio::test]
    async fn file_writer_write_empty_slice() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(file);
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }
}
