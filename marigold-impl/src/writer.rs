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
    async fn test_vector_writer_write() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_roundtrip() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("writer_test_{}.bin", std::process::id()));
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"roundtrip").await.unwrap();
        w.shutdown().await.unwrap();

        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"roundtrip");
        tokio::fs::remove_file(&path).await.unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_multiple_writes() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("writer_test_multi_{}.bin", std::process::id()));
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.shutdown().await.unwrap();

        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"foobar");
        tokio::fs::remove_file(&path).await.unwrap();
    }

    #[tokio::test]
    async fn test_writer_debug_format() {
        let w = Writer::vector();
        let s = format!("{:?}", w);
        assert!(s.contains("Writer"));
    }
}
