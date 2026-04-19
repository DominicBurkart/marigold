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

    /// Consumes the writer and returns the written bytes if this is a vector writer.
    /// Only used in tests; gated to avoid a dead_code clippy warning in non-test builds.
    #[cfg(test)]
    pub(crate) fn into_bytes(self) -> Option<Vec<u8>> {
        match self.inner {
            WriteTarget::Vector(v) => Some(*Pin::into_inner(v)),
            WriteTarget::File(_) => None,
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
        let mut writer = Writer::vector();
        writer.write_all(b"hello ").await.unwrap();
        writer.write_all(b"world").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        assert_eq!(writer.into_bytes().unwrap(), b"hello world");
    }

    #[tokio::test]
    async fn file_writer_roundtrip() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("marigold_writer_test_{}.txt", std::process::id()));

        let f = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(f);
        writer.write_all(b"marigold").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();

        let contents = tokio::fs::read_to_string(&path).await.unwrap();
        assert_eq!(contents, "marigold");

        tokio::fs::remove_file(&path).await.ok();
    }
}
