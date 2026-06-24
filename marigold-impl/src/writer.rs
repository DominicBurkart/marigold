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
    async fn vector_write_data() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.expect("write failed");
        w.flush().await.expect("flush failed");
    }

    #[tokio::test]
    async fn vector_write_empty() {
        let mut w = Writer::vector();
        w.write_all(b"").await.expect("empty write failed");
        w.flush().await.expect("flush failed");
    }

    #[tokio::test]
    async fn vector_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.expect("write failed");
        w.shutdown().await.expect("shutdown failed");
    }

    #[tokio::test]
    async fn file_write_roundtrip() {
        let path = std::env::temp_dir().join("marigold_writer_test.bin");
        let file = tokio::fs::File::create(&path).await.expect("create failed");
        let mut w = Writer::file(file);
        w.write_all(b"hello file").await.expect("write failed");
        w.flush().await.expect("flush failed");
        w.shutdown().await.expect("shutdown failed");
        drop(w);
        let contents = tokio::fs::read(&path).await.expect("read failed");
        assert_eq!(contents, b"hello file");
        let _ = tokio::fs::remove_file(&path).await;
    }
}
