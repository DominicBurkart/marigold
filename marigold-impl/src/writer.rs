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

    /// Verify that the vector-backed writer accepts writes and reports the
    /// correct number of bytes written.
    #[tokio::test]
    async fn test_vector_writer_write() {
        let mut writer = Writer::vector();
        let n = writer.write(b"hello world").await.unwrap();
        assert_eq!(n, 11);
    }

    /// Verify that flush succeeds on a vector-backed writer (it is a no-op
    /// but must not error).
    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        writer.flush().await.unwrap();
    }

    /// Verify that shutdown completes without error on a vector-backed writer.
    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"goodbye").await.unwrap();
        writer.shutdown().await.unwrap();
    }

    /// Verify that multiple sequential writes to the vector-backed writer
    /// all succeed.
    #[tokio::test]
    async fn test_vector_writer_multiple_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"part one ").await.unwrap();
        writer.write_all(b"part two").await.unwrap();
        writer.flush().await.unwrap();
    }

    /// Verify that Writer derives Debug and formats without panicking.
    #[tokio::test]
    async fn test_vector_writer_debug() {
        let writer = Writer::vector();
        let s = format!("{:?}", writer);
        assert!(s.contains("Writer"));
    }

    /// Verify that the file-backed writer can write, flush, and shut down
    /// against a real temporary file.
    #[tokio::test]
    async fn test_file_writer_write_and_flush() {
        let path = std::env::temp_dir().join("marigold_writer_test_write.txt");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        let n = writer.write(b"hello file").await.unwrap();
        assert_eq!(n, 10);
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        tokio::fs::remove_file(&path).await.ok();
    }

    /// Verify that Writer::file produces a Debug representation that includes
    /// the "Writer" type name.
    #[tokio::test]
    async fn test_file_writer_debug() {
        let path = std::env::temp_dir().join("marigold_writer_test_debug.txt");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let writer = Writer::file(file);
        let s = format!("{:?}", writer);
        assert!(s.contains("Writer"));
        tokio::fs::remove_file(&path).await.ok();
    }
}
