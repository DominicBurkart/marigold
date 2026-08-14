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
        let mut writer = Writer::vector();
        let bytes_written = writer.write(b"hello").await.unwrap();
        assert_eq!(bytes_written, 5);
    }

    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut writer = Writer::vector();
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut writer = Writer::vector();
        writer.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_write_and_read_back() {
        let dir = std::env::temp_dir().join("marigold_writer_test_write");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("output.bin");

        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"hello world").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let content = std::fs::read(&path).unwrap();
        assert_eq!(content, b"hello world");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[tokio::test]
    async fn test_file_writer_flush_and_shutdown() {
        let dir = std::env::temp_dir().join("marigold_writer_test_flush");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("flush_test.bin");

        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn test_writer_debug_vector() {
        let writer = Writer::vector();
        let s = format!("{:?}", writer);
        assert!(s.contains("Writer"));
    }

    #[test]
    fn test_writer_debug_file() {
        // Construct a Writer::file via tokio Handle to open a sync file
        // Use std::fs then unsafely build? No — use a runtime.
        let rt = tokio::runtime::Runtime::new().unwrap();
        let dir = std::env::temp_dir().join("marigold_writer_debug_file");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("debug.bin");

        rt.block_on(async {
            let file = tokio::fs::File::create(&path).await.unwrap();
            let writer = Writer::file(file);
            let s = format!("{:?}", writer);
            assert!(s.contains("Writer"));
        });

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
