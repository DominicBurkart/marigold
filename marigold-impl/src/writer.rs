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

    // ---------------------------------------------------------------------------
    // Vector-backed writer
    // ---------------------------------------------------------------------------

    /// `Writer::vector()` must be constructible without panicking.
    #[tokio::test]
    async fn vector_writer_creates_successfully() {
        let _writer = Writer::vector();
    }

    /// Writing bytes to a vector-backed `Writer` must succeed and report the
    /// correct number of bytes written.
    #[tokio::test]
    async fn vector_writer_write_succeeds() {
        let mut writer = Writer::vector();
        let bytes = b"hello, marigold";
        let n = writer.write(bytes).await.expect("write should not fail");
        assert_eq!(n, bytes.len());
    }

    /// `flush()` on a vector-backed `Writer` is a no-op but must return `Ok`.
    #[tokio::test]
    async fn vector_writer_flush_succeeds() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"data")
            .await
            .expect("write_all should not fail");
        writer.flush().await.expect("flush should not fail");
    }

    /// `shutdown()` on a vector-backed `Writer` must return `Ok`.
    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"shutdown test")
            .await
            .expect("write_all should not fail");
        writer.shutdown().await.expect("shutdown should not fail");
    }

    /// Multiple sequential writes to a vector-backed `Writer` must each succeed.
    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"first ")
            .await
            .expect("first write should not fail");
        writer
            .write_all(b"second")
            .await
            .expect("second write should not fail");
        writer.flush().await.expect("flush should not fail");
    }

    // ---------------------------------------------------------------------------
    // File-backed writer
    // ---------------------------------------------------------------------------

    /// `Writer::file()` wraps a `tokio::fs::File` and must support `write_all`,
    /// `flush`, and `shutdown` without error.
    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("marigold_writer_test_{}.tmp", std::process::id()));
        let file = tokio::fs::File::create(&path)
            .await
            .expect("failed to create temp file");
        let mut writer = Writer::file(file);
        writer
            .write_all(b"file writer test data")
            .await
            .expect("write_all should not fail");
        writer.flush().await.expect("flush should not fail");
        writer.shutdown().await.expect("shutdown should not fail");
        // Clean up
        let _ = tokio::fs::remove_file(&path).await;
    }
}
