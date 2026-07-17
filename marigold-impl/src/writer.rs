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
    async fn test_writer_vector_write_flush_shutdown() {
        let mut writer = Writer::vector();
        let bytes = b"hello, world";
        let n = writer.write(bytes).await.unwrap();
        assert_eq!(n, bytes.len());
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_writer_vector_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"first chunk").await.unwrap();
        writer.write_all(b" second chunk").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_writer_vector_empty_write() {
        let mut writer = Writer::vector();
        let n = writer.write(b"").await.unwrap();
        assert_eq!(n, 0);
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_writer_file_write_flush_shutdown() {
        let tmp = std::env::temp_dir().join("marigold_writer_test.bin");
        let file = tokio::fs::File::create(&tmp).await.unwrap();
        let mut writer = Writer::file(file);
        let bytes = b"test file content";
        let n = writer.write(bytes).await.unwrap();
        assert_eq!(n, bytes.len());
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&tmp).await.ok();
    }

    #[tokio::test]
    async fn test_writer_file_multiple_writes() {
        let tmp = std::env::temp_dir().join("marigold_writer_multi_test.bin");
        let file = tokio::fs::File::create(&tmp).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"line one\n").await.unwrap();
        writer.write_all(b"line two\n").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&tmp).await.ok();
    }
}
