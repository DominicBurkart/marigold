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

    // Writer::vector — exercises poll_write, poll_flush, and poll_shutdown for
    // the Vec<u8> backend.  We verify the call succeeds without panicking and
    // that the number of bytes reported equals the input length.
    #[tokio::test]
    async fn vector_write_succeeds() {
        let mut w = Writer::vector();
        let n = w.write(b"hello").await.expect("write should succeed");
        assert_eq!(n, 5);
    }

    #[tokio::test]
    async fn vector_write_empty_slice_succeeds() {
        let mut w = Writer::vector();
        let n = w.write(b"").await.expect("empty write should succeed");
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn vector_flush_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.expect("write_all should succeed");
        w.flush().await.expect("flush should succeed");
    }

    #[tokio::test]
    async fn vector_shutdown_succeeds() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.expect("write_all should succeed");
        w.shutdown().await.expect("shutdown should succeed");
    }

    #[tokio::test]
    async fn vector_write_all_large_payload() {
        let mut w = Writer::vector();
        let payload: Vec<u8> = (0u8..=255).cycle().take(4096).collect();
        w.write_all(&payload)
            .await
            .expect("write_all of 4096 bytes should succeed");
    }

    // Writer::file — exercises the File backend through a real temp file.
    #[tokio::test]
    async fn file_write_succeeds() {
        let tmp = tempfile::NamedTempFile::new().expect("tempfile");
        let f = tokio::fs::File::from_std(tmp.reopen().expect("reopen"));
        let mut w = Writer::file(f);
        let n = w.write(b"world").await.expect("file write should succeed");
        assert_eq!(n, 5);
    }

    #[tokio::test]
    async fn file_flush_succeeds() {
        let tmp = tempfile::NamedTempFile::new().expect("tempfile");
        let f = tokio::fs::File::from_std(tmp.reopen().expect("reopen"));
        let mut w = Writer::file(f);
        w.write_all(b"flush me").await.expect("write should succeed");
        w.flush().await.expect("flush should succeed");
    }

    #[tokio::test]
    async fn file_shutdown_succeeds() {
        let tmp = tempfile::NamedTempFile::new().expect("tempfile");
        let f = tokio::fs::File::from_std(tmp.reopen().expect("reopen"));
        let mut w = Writer::file(f);
        w.write_all(b"shutdown me")
            .await
            .expect("write should succeed");
        w.shutdown().await.expect("shutdown should succeed");
    }

    #[tokio::test]
    async fn file_write_all_roundtrip() {
        let tmp = tempfile::NamedTempFile::new().expect("tempfile");
        let path = tmp.path().to_owned();

        let f = tokio::fs::File::create(&path)
            .await
            .expect("create file for writing");
        let mut w = Writer::file(f);
        w.write_all(b"roundtrip")
            .await
            .expect("write_all should succeed");
        w.shutdown().await.expect("shutdown should succeed");

        let contents = std::fs::read(&path).expect("read back written file");
        assert_eq!(contents, b"roundtrip");
    }
}
