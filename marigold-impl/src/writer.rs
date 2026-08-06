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

    // ── vector backend ───────────────────────────────────────────────────────

    #[tokio::test]
    async fn vector_write_all_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello, vector!").await.expect("write_all");
        w.flush().await.expect("flush");
    }

    #[tokio::test]
    async fn vector_write_then_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"shutdown test").await.expect("write_all");
        w.shutdown().await.expect("shutdown");
    }

    #[tokio::test]
    async fn vector_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"part1").await.expect("first write");
        w.write_all(b"part2").await.expect("second write");
        w.flush().await.expect("flush");
    }

    #[tokio::test]
    async fn vector_empty_write() {
        // Writing zero bytes must not error.
        let mut w = Writer::vector();
        w.write_all(b"").await.expect("empty write");
        w.flush().await.expect("flush");
    }

    // ── file backend ─────────────────────────────────────────────────────────

    #[tokio::test]
    async fn file_write_all_and_flush() {
        let path = std::env::temp_dir().join("marigold_writer_test_write.tmp");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");

        let mut w = Writer::file(file);
        w.write_all(b"hello, file!").await.expect("write_all");
        w.flush().await.expect("flush");
        w.shutdown().await.expect("shutdown");

        // Verify round-trip: the bytes we wrote were actually persisted.
        let contents = tokio::fs::read(&path).await.expect("read back file");
        assert_eq!(contents, b"hello, file!");

        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn file_shutdown_without_write() {
        let path = std::env::temp_dir().join("marigold_writer_test_shutdown.tmp");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");

        let mut w = Writer::file(file);
        w.shutdown().await.expect("shutdown on empty file writer");

        tokio::fs::remove_file(&path).await.ok();
    }
}
