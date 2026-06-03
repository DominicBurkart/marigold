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

    // Tests for the Vector backend (no filesystem needed).
    // write_all drives poll_write; flush drives poll_flush; shutdown drives poll_shutdown.

    #[tokio::test]
    async fn vector_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
    }

    // Tests for the File backend. These require the `io` feature (tokio/fs).
    #[cfg(feature = "io")]
    mod file_tests {
        use super::*;

        #[tokio::test]
        async fn file_write_flush_shutdown() {
            let path = std::env::temp_dir().join("marigold_writer_file_test.tmp");
            let file = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(file);
            w.write_all(b"hello file").await.unwrap();
            w.flush().await.unwrap();
            w.shutdown().await.unwrap();
            tokio::fs::remove_file(&path).await.ok();
        }

        #[tokio::test]
        async fn file_empty_write() {
            let path = std::env::temp_dir().join("marigold_writer_file_empty_test.tmp");
            let file = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(file);
            w.write_all(b"").await.unwrap();
            w.flush().await.unwrap();
            tokio::fs::remove_file(&path).await.ok();
        }
    }
}
