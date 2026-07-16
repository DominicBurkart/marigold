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
    async fn test_vector_writer_write_and_flush() {
        let mut writer = Writer::vector();
        writer.write_all(b"hello world").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"test data").await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"foo").await.unwrap();
        writer.write_all(b"bar").await.unwrap();
        writer.write_all(b"baz").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_empty_write() {
        let mut writer = Writer::vector();
        writer.write_all(b"").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_write_and_flush() {
        let path = std::env::temp_dir().join("marigold_writer_test_write.tmp");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"hello from marigold").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_file_writer_multiple_writes() {
        let path = std::env::temp_dir().join("marigold_writer_test_multi.tmp");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"line 1\n").await.unwrap();
        writer.write_all(b"line 2\n").await.unwrap();
        writer.flush().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    #[tokio::test]
    async fn test_writer_debug_vector() {
        let writer = Writer::vector();
        let debug = format!("{:?}", writer);
        assert!(debug.contains("Vector"));
    }

    #[tokio::test]
    async fn test_writer_debug_file() {
        let path = std::env::temp_dir().join("marigold_writer_test_debug.tmp");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let writer = Writer::file(file);
        let debug = format!("{:?}", writer);
        assert!(debug.contains("File"));
        tokio::fs::remove_file(&path).await.ok();
    }
}
