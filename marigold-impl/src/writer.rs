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
    async fn vector_writer_write_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.expect("write_all");
        w.flush().await.expect("flush");
    }

    #[tokio::test]
    async fn vector_writer_write_empty_slice() {
        let mut w = Writer::vector();
        w.write_all(b"").await.expect("write empty slice");
        w.flush().await.expect("flush after empty write");
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.expect("first write");
        w.write_all(b"bar").await.expect("second write");
        w.flush().await.expect("flush");
    }

    #[tokio::test]
    async fn vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.expect("write");
        w.shutdown().await.expect("shutdown");
    }
}
