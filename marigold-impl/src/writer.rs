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
    async fn writer_vector_write_succeeds() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.unwrap();
        assert_eq!(n, 5);
    }

    #[tokio::test]
    async fn writer_vector_flush_succeeds() {
        let mut w = Writer::vector();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn writer_vector_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn writer_vector_empty_write() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn writer_vector_multiple_sequential_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn writer_vector_debug_contains_variant_name() {
        let w = Writer::vector();
        assert!(format!("{:?}", w).contains("Vector"));
    }

    #[tokio::test]
    async fn writer_file_write_and_read_back() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"hello file").await.unwrap();
        w.shutdown().await.unwrap();
        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"hello file");
    }

    #[tokio::test]
    async fn writer_file_empty_write() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("empty.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn writer_file_multiple_writes() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("multi.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut w = Writer::file(f);
        w.write_all(b"abc").await.unwrap();
        w.write_all(b"def").await.unwrap();
        w.shutdown().await.unwrap();
        let contents = tokio::fs::read(&path).await.unwrap();
        assert_eq!(contents, b"abcdef");
    }

    #[tokio::test]
    async fn writer_file_debug_contains_variant_name() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("debug.bin");
        let f = tokio::fs::File::create(&path).await.unwrap();
        let w = Writer::file(f);
        assert!(format!("{:?}", w).contains("File"));
    }
}
