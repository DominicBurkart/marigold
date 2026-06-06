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
    async fn vector_writer_write_returns_byte_count() {
        let mut writer = Writer::vector();
        let n = writer.write(b"hello world").await.unwrap();
        assert_eq!(n, 11);
    }

    #[tokio::test]
    async fn vector_writer_write_empty_buffer() {
        let mut writer = Writer::vector();
        let n = writer.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn vector_writer_flush_after_write_succeeds() {
        let mut writer = Writer::vector();
        writer.write_all(b"flush test").await.unwrap();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut writer = Writer::vector();
        writer.write_all(b"shutdown test").await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes_accumulate() {
        let mut writer = Writer::vector();
        let n1 = writer.write(b"abc").await.unwrap();
        let n2 = writer.write(b"de").await.unwrap();
        assert_eq!(n1, 3);
        assert_eq!(n2, 2);
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let path = std::env::temp_dir().join("marigold_writer_test.tmp");
        let _ = tokio::fs::remove_file(&path).await;
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"file content").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        let content = tokio::fs::read(&path).await.unwrap();
        assert_eq!(content, b"file content");
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[tokio::test]
    async fn file_writer_empty_write() {
        let path = std::env::temp_dir().join("marigold_writer_empty_test.tmp");
        let _ = tokio::fs::remove_file(&path).await;
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        let content = tokio::fs::read(&path).await.unwrap();
        assert!(content.is_empty());
        let _ = tokio::fs::remove_file(&path).await;
    }

    #[test]
    fn writer_debug_format_contains_struct_name() {
        let writer = Writer::vector();
        let debug = format!("{writer:?}");
        assert!(debug.contains("Writer"));
    }
}
