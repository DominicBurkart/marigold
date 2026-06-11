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

    #[tokio::test]
    async fn vector_writer_write_returns_byte_count() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.unwrap();
        assert_eq!(n, 5);
    }

    #[tokio::test]
    async fn vector_writer_write_all_and_flush_succeed() {
        let mut w = Writer::vector();
        w.write_all(b"hello world").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_sequential_writes_succeed() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_write_empty_slice() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.unwrap();
        assert_eq!(n, 0);
        w.flush().await.unwrap();
    }
}
