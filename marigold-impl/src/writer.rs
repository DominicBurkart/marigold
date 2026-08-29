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

    // Writer::vector() exercises the Vector branch of poll_write, poll_flush,
    // and poll_shutdown through the AsyncWriteExt helper methods.
    #[tokio::test]
    async fn vector_writer_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"hello, world")
            .await
            .expect("write_all failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");
    }

    // Repeated writes should succeed: the Vec grows as needed.
    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        for chunk in &[b"foo" as &[u8], b"bar", b"baz"] {
            w.write_all(chunk).await.expect("write_all failed");
        }
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");
    }

    // An empty write (zero bytes) should be a no-op and not error.
    #[tokio::test]
    async fn vector_writer_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.expect("empty write_all failed");
        w.flush().await.expect("flush failed");
    }

    // Writer::file() exercises the File branch of poll_write, poll_flush,
    // and poll_shutdown.
    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        // Use a unique temp path derived from a random suffix to avoid
        // collisions between parallel test invocations.
        let path = {
            let mut p = std::env::temp_dir();
            p.push(format!(
                "marigold_writer_test_{}.tmp",
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .subsec_nanos()
            ));
            p
        };

        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file failed");
        let mut w = Writer::file(file);

        w.write_all(b"hello from file writer")
            .await
            .expect("write_all failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");

        // Verify the bytes actually landed on disk.
        let contents = tokio::fs::read(&path).await.expect("read back failed");
        assert_eq!(contents, b"hello from file writer");

        let _ = tokio::fs::remove_file(&path).await;
    }

    // Multiple writes to the file branch should be concatenated.
    #[tokio::test]
    async fn file_writer_multiple_writes() {
        let path = {
            let mut p = std::env::temp_dir();
            p.push(format!(
                "marigold_writer_multi_{}.tmp",
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .subsec_nanos()
            ));
            p
        };

        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file failed");
        let mut w = Writer::file(file);

        w.write_all(b"ab").await.expect("write 1 failed");
        w.write_all(b"cd").await.expect("write 2 failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");

        let contents = tokio::fs::read(&path).await.expect("read back failed");
        assert_eq!(contents, b"abcd");

        let _ = tokio::fs::remove_file(&path).await;
    }
}
