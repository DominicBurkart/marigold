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

    /// Writing bytes to a vector-backed Writer succeeds without error.
    #[tokio::test]
    async fn vector_writer_accepts_bytes() {
        let mut writer = Writer::vector();
        let result = writer.write_all(b"hello, marigold").await;
        assert!(result.is_ok(), "write_all failed: {:?}", result.err());
    }

    /// Multiple sequential writes followed by flush complete successfully.
    #[tokio::test]
    async fn vector_writer_flush_succeeds() {
        let mut writer = Writer::vector();
        writer.write_all(b"first").await.unwrap();
        writer.write_all(b"second").await.unwrap();
        let flush_result = writer.flush().await;
        assert!(
            flush_result.is_ok(),
            "flush failed: {:?}",
            flush_result.err()
        );
    }

    /// shutdown (poll_shutdown) completes without error on a vector writer.
    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        let shutdown_result = writer.shutdown().await;
        assert!(
            shutdown_result.is_ok(),
            "shutdown failed: {:?}",
            shutdown_result.err()
        );
    }

    /// Writing to a file-backed Writer uses tokio::fs::File correctly.
    #[tokio::test]
    async fn file_writer_accepts_bytes() {
        let tmp = tempfile::NamedTempFile::new().expect("failed to create temp file");
        let path = tmp.path().to_owned();
        let file = tokio::fs::File::create(&path)
            .await
            .expect("failed to open file");
        let mut writer = Writer::file(file);
        writer
            .write_all(b"file writer test")
            .await
            .expect("write_all to file failed");
        writer.flush().await.expect("flush to file failed");

        let contents = tokio::fs::read(&path)
            .await
            .expect("failed to read back file");
        assert_eq!(contents, b"file writer test");
    }
}
