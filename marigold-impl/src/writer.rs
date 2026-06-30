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

    fn temp_path(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!("marigold_writer_{name}.bin"))
    }

    // --- Writer::vector() covers the Vector arm of all three poll methods ---

    #[tokio::test]
    async fn test_vector_writer_write() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.unwrap();
        assert_eq!(n, 5);
    }

    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut w = Writer::vector();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.shutdown().await.unwrap();
    }

    // --- Writer::file() covers the File arm of all three poll methods ---

    #[tokio::test]
    async fn test_file_writer_write() {
        let path = temp_path("write");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        let n = w.write(b"hello").await.unwrap();
        assert!(n > 0);
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_file_writer_flush() {
        let path = temp_path("flush");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.flush().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_file_writer_shutdown() {
        let path = temp_path("shutdown");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.shutdown().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }
}
