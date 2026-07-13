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

#[cfg(all(test, feature = "io"))]
mod tests {
    use super::*;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn vector_poll_write_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_poll_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"world").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn file_poll_write_and_flush() {
        let path = std::env::temp_dir().join("marigold_writer_test_write.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[tokio::test]
    async fn file_poll_shutdown() {
        let path = std::env::temp_dir().join("marigold_writer_test_shutdown.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"shutdown").await.unwrap();
        w.shutdown().await.unwrap();
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[test]
    fn writer_debug_impl() {
        let w = Writer::vector();
        let s = format!("{w:?}");
        assert!(s.contains("Writer"));
    }
}
