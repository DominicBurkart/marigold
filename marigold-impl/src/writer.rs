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

    // --- Vector variant tests ---

    #[tokio::test]
    async fn vector_writer_write_flush_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"hello world").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_write_empty_bytes() {
        let mut writer = Writer::vector();
        writer.write_all(b"").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"foo").await.unwrap();
        writer.write_all(b"bar").await.unwrap();
        writer.write_all(b"baz").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_large_payload() {
        let mut writer = Writer::vector();
        let data = vec![0xABu8; 65536];
        writer.write_all(&data).await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_write_after_shutdown() {
        // Shutdown on a Vec<u8> is a no-op; writes still succeed.
        let mut writer = Writer::vector();
        writer.write_all(b"before").await.unwrap();
        writer.shutdown().await.unwrap();
        writer.write_all(b"after").await.unwrap();
        writer.flush().await.unwrap();
    }

    // --- File variant tests ---

    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"test data").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let contents = std::fs::read(tmp.path()).unwrap();
        assert_eq!(contents, b"test data");
    }

    #[tokio::test]
    async fn file_writer_write_empty() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let contents = std::fs::read(tmp.path()).unwrap();
        assert!(contents.is_empty());
    }

    #[tokio::test]
    async fn file_writer_multiple_writes() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"hello ").await.unwrap();
        writer.write_all(b"world").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let contents = std::fs::read(tmp.path()).unwrap();
        assert_eq!(contents, b"hello world");
    }

    #[tokio::test]
    async fn file_writer_large_payload() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut writer = Writer::file(file);
        let data = vec![0xCDu8; 131072];
        writer.write_all(&data).await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let contents = std::fs::read(tmp.path()).unwrap();
        assert_eq!(contents, data);
    }

    #[tokio::test]
    async fn file_writer_is_debug() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let writer = Writer::file(file);
        // Writer derives Debug — ensure it can be formatted without panicking.
        let _ = format!("{:?}", writer);
    }

    #[tokio::test]
    async fn vector_writer_is_debug() {
        let writer = Writer::vector();
        let _ = format!("{:?}", writer);
    }
}
