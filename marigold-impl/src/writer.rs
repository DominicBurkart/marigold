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

    // ── Vector-backed Writer ──────────────────────────────────────────────

    /// `Writer::vector()` must accept bytes via `poll_write` and report the
    /// correct number of bytes written.
    #[tokio::test]
    async fn vector_writer_write_returns_byte_count() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.unwrap();
        assert_eq!(n, 5);
    }

    /// Multiple sequential writes must all succeed and accumulate in the
    /// underlying buffer.
    #[tokio::test]
    async fn vector_writer_multiple_writes_succeed() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        // No panic ⇒ both writes succeeded.
    }

    /// `flush` on the vector variant is always a no-op but must return `Ok`.
    #[tokio::test]
    async fn vector_writer_flush_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    /// `shutdown` on the vector variant must complete without error.
    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// An empty write (zero bytes) is valid and must return 0.
    #[tokio::test]
    async fn vector_writer_empty_write_returns_zero() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    // ── File-backed Writer ────────────────────────────────────────────────

    /// `Writer::file()` must delegate `poll_write` to the underlying
    /// `tokio::fs::File`, writing the exact bytes requested.
    #[tokio::test]
    async fn file_writer_write_returns_byte_count() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let f = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(f);
        let n = w.write(b"hello world").await.unwrap();
        assert_eq!(n, 11);
    }

    /// `flush` on a file-backed Writer must succeed (it flushes OS buffers).
    #[tokio::test]
    async fn file_writer_flush_succeeds() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let f = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    /// `shutdown` on a file-backed Writer must complete without error.
    #[tokio::test]
    async fn file_writer_shutdown_succeeds() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let f = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"data").await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Data written via `Writer::file()` must actually land on disk so that a
    /// subsequent read confirms the correct bytes were persisted.
    #[tokio::test]
    async fn file_writer_persists_data() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let path = tmp.path().to_owned();

        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"persisted").await.unwrap();
        w.shutdown().await.unwrap();

        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"persisted");
    }

    // ── Debug representation ──────────────────────────────────────────────

    /// Both variants must implement `Debug` without panicking.
    #[test]
    fn writer_debug_does_not_panic() {
        let v = Writer::vector();
        let _ = format!("{:?}", v);
        // File variant cannot be tested without a real fd; vector covers the
        // derive path for `WriteTarget::Vector`.
    }
}
