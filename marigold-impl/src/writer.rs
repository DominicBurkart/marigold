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

    /// Writing bytes to a vector-backed writer should succeed.
    #[tokio::test]
    async fn vector_writer_accepts_bytes() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"hello, marigold!")
            .await
            .expect("write_all should succeed on a vector writer");
        writer
            .flush()
            .await
            .expect("flush should succeed on a vector writer");
    }

    /// An empty write to a vector writer is valid (zero bytes written).
    #[tokio::test]
    async fn vector_writer_accepts_empty_write() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"")
            .await
            .expect("empty write_all should succeed");
        writer
            .flush()
            .await
            .expect("flush after empty write should succeed");
    }

    /// Multiple sequential writes followed by flush and shutdown must all succeed.
    #[tokio::test]
    async fn vector_writer_accepts_multiple_writes() {
        let mut writer = Writer::vector();
        for byte in 0_u8..10_u8 {
            writer
                .write_all(&[byte])
                .await
                .expect("sequential single-byte write should succeed");
        }
        writer
            .flush()
            .await
            .expect("flush after multiple writes should succeed");
        writer.shutdown().await.expect("shutdown should succeed");
    }

    /// Writing to a file-backed writer and reading back should yield the same bytes.
    #[tokio::test]
    async fn file_writer_roundtrip() {
        let path = std::env::temp_dir().join("marigold_writer_test_roundtrip.bin");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("should be able to create a temporary file");

        let expected: &[u8] = b"marigold file writer roundtrip";
        let mut writer = Writer::file(file);
        writer
            .write_all(expected)
            .await
            .expect("write_all to file writer should succeed");
        writer
            .flush()
            .await
            .expect("flush to file writer should succeed");
        writer
            .shutdown()
            .await
            .expect("shutdown of file writer should succeed");

        let content = tokio::fs::read(&path)
            .await
            .expect("reading back written file should succeed");
        let _ = tokio::fs::remove_file(&path).await;
        assert_eq!(
            content,
            expected,
            "file content should exactly match what was written"
        );
    }

    /// Exercises the file-backed poll_write and poll_shutdown paths explicitly.
    #[tokio::test]
    async fn file_writer_poll_write_and_shutdown() {
        let path = std::env::temp_dir().join("marigold_writer_test_poll.bin");
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");

        let mut writer = Writer::file(file);
        writer
            .write_all(b"poll shutdown test")
            .await
            .expect("write should succeed");
        writer.shutdown().await.expect("shutdown should succeed");

        let _ = tokio::fs::remove_file(&path).await;
    }
}
