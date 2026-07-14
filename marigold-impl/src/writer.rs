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

    #[tokio::test]
    async fn test_vector_write_flush_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"hello world").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_write_flush_shutdown() {
        let path = std::env::temp_dir().join("marigold_writer_test.txt");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"hello from file writer").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_vector_debug_format() {
        let writer = Writer::vector();
        let s = format!("{:?}", writer);
        assert!(s.contains("Writer"));
    }

    #[tokio::test]
    async fn test_file_debug_format() {
        let path = std::env::temp_dir().join("marigold_writer_debug_test.txt");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let writer = Writer::file(file);
        let s = format!("{:?}", writer);
        assert!(s.contains("Writer"));
        tokio::fs::remove_file(&path).await.ok();
    }
}
