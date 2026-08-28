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

    /// The vector backend should accept arbitrary bytes and flush/shutdown cleanly.
    #[tokio::test]
    async fn vector_writer_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"hello, world").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Writing nothing to the vector backend should succeed.
    #[tokio::test]
    async fn vector_writer_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Multiple sequential writes to the vector backend should all succeed.
    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"first").await.unwrap();
        w.write_all(b"second").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// The file backend should accept bytes and flush/shutdown cleanly using a
    /// temporary file.
    #[tokio::test]
    async fn file_writer_write_flush_shutdown() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"file content").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Writing nothing to the file backend should succeed.
    #[tokio::test]
    async fn file_writer_empty_write() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let file = tokio::fs::File::create(tmp.path()).await.unwrap();
        let mut w = Writer::file(file);
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }
}
