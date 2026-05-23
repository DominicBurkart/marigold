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
    async fn vector_writer_write_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.expect("write failed");
        w.flush().await.expect("flush failed");
        // If we got here without error, the write and flush succeeded.
        // Extract the inner vec to verify content.
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(*v, b"hello");
            }
            _ => panic!("expected vector writer"),
        }
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.expect("first write failed");
        w.write_all(b"bar").await.expect("second write failed");
        w.flush().await.expect("flush failed");
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(*v, b"foobar");
            }
            _ => panic!("expected vector writer"),
        }
    }

    #[tokio::test]
    async fn vector_writer_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.expect("empty write failed");
        w.flush().await.expect("flush failed");
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(*v, b"");
            }
            _ => panic!("expected vector writer"),
        }
    }

    #[tokio::test]
    async fn vector_writer_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"shutdown test").await.expect("write failed");
        w.shutdown().await.expect("shutdown failed");
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(*v, b"shutdown test");
            }
            _ => panic!("expected vector writer"),
        }
    }

    #[tokio::test]
    async fn file_writer_write_and_flush() {
        let path = "/tmp/marigold_writer_test_write.bin";
        let f = tokio::fs::File::create(path)
            .await
            .expect("could not create temp file");
        let mut w = Writer::file(f);
        w.write_all(b"file test data").await.expect("write failed");
        w.flush().await.expect("flush failed");
        // Verify via std::fs read
        let contents = std::fs::read(path).expect("could not read back temp file");
        assert_eq!(contents, b"file test data");
        let _ = std::fs::remove_file(path);
    }

    #[tokio::test]
    async fn file_writer_shutdown() {
        let path = "/tmp/marigold_writer_test_shutdown.bin";
        let f = tokio::fs::File::create(path)
            .await
            .expect("could not create temp file");
        let mut w = Writer::file(f);
        w.write_all(b"shutdown").await.expect("write failed");
        w.shutdown().await.expect("shutdown failed");
        let contents = std::fs::read(path).expect("could not read back temp file");
        assert_eq!(contents, b"shutdown");
        let _ = std::fs::remove_file(path);
    }

    #[tokio::test]
    async fn file_writer_multiple_writes() {
        let path = "/tmp/marigold_writer_test_multi.bin";
        let f = tokio::fs::File::create(path)
            .await
            .expect("could not create temp file");
        let mut w = Writer::file(f);
        w.write_all(b"part1").await.expect("first write failed");
        w.write_all(b"part2").await.expect("second write failed");
        w.flush().await.expect("flush failed");
        let contents = std::fs::read(path).expect("could not read back temp file");
        assert_eq!(contents, b"part1part2");
        let _ = std::fs::remove_file(path);
    }
}
