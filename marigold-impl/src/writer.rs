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
    async fn vector_writer_write_all_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"hello, world")
            .await
            .expect("write_all to vector should succeed");
    }

    #[tokio::test]
    async fn vector_writer_flush_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.expect("flush on vector writer should succeed");
    }

    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.shutdown()
            .await
            .expect("shutdown on vector writer should succeed");
    }

    #[tokio::test]
    async fn vector_writer_write_empty_bytes() {
        let mut w = Writer::vector();
        w.write_all(b"")
            .await
            .expect("writing empty bytes to vector writer should succeed");
    }

    #[tokio::test]
    async fn file_writer_write_and_flush() {
        let path = std::env::temp_dir().join("marigold_writer_test_file.bin");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");
        let mut w = Writer::file(file);
        w.write_all(b"marigold test bytes")
            .await
            .expect("write to file writer should succeed");
        w.flush().await.expect("flush file writer should succeed");
        w.shutdown()
            .await
            .expect("shutdown file writer should succeed");
        let content = tokio::fs::read(&path).await.expect("read back temp file");
        assert_eq!(content, b"marigold test bytes");
        let _ = tokio::fs::remove_file(&path).await;
    }
}
