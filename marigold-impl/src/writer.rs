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
    use std::pin::Pin;
    use tempfile::NamedTempFile;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn vector_writer_write_and_verify() {
        let mut w = Writer::vector();
        w.write_all(b"hello, marigold").await.unwrap();
        w.flush().await.unwrap();
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(Pin::into_inner(v).as_slice(), b"hello, marigold");
            }
            _ => panic!("expected Vector variant"),
        }
    }

    #[tokio::test]
    async fn vector_writer_multiple_writes_accumulate() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
        match w.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(Pin::into_inner(v).as_slice(), b"foobar");
            }
            _ => panic!("expected Vector variant"),
        }
    }

    #[tokio::test]
    async fn vector_writer_empty_write_flush_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        match w.inner {
            WriteTarget::Vector(v) => {
                assert!(Pin::into_inner(v).is_empty());
            }
            _ => panic!("expected Vector variant"),
        }
    }

    #[tokio::test]
    async fn file_writer_write_and_read_back() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path().to_path_buf();
        {
            let f = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(f);
            w.write_all(b"file content").await.unwrap();
            w.flush().await.unwrap();
            w.shutdown().await.unwrap();
        }
        let data = std::fs::read(&path).unwrap();
        assert_eq!(&data, b"file content");
    }

    #[tokio::test]
    async fn file_writer_multiple_chunks() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path().to_path_buf();
        {
            let f = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(f);
            w.write_all(b"chunk1").await.unwrap();
            w.write_all(b"chunk2").await.unwrap();
            w.flush().await.unwrap();
        }
        let data = std::fs::read(&path).unwrap();
        assert_eq!(&data, b"chunk1chunk2");
    }

    #[tokio::test]
    async fn writer_debug_format_contains_variant_name() {
        let v = Writer::vector();
        assert!(format!("{:?}", v).contains("Vector"));
    }
}
