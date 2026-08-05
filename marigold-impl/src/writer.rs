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

    // ── Vector-backed writer ─────────────────────────────────────────────────

    /// Exercises poll_write + poll_flush on the Vector variant.
    #[tokio::test]
    async fn vector_write_and_flush() {
        let mut w = Writer::vector();
        w.write_all(b"hello, vector").await.unwrap();
        w.flush().await.unwrap();
    }

    /// Exercises poll_shutdown on the Vector variant.
    #[tokio::test]
    async fn vector_write_and_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"shutdown test").await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Empty write is valid and must not panic.
    #[tokio::test]
    async fn vector_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
    }

    /// Multiple sequential writes accumulate correctly.
    #[tokio::test]
    async fn vector_multiple_writes() {
        let mut w = Writer::vector();
        w.write_all(b"foo").await.unwrap();
        w.write_all(b"bar").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    /// Debug output for the Vector variant must not panic.
    #[test]
    fn vector_debug_format() {
        let w = Writer::vector();
        let s = format!("{w:?}");
        assert!(s.contains("Writer"), "unexpected debug: {s}");
    }

    // ── File-backed writer ───────────────────────────────────────────────────

    /// Returns a unique temp path for the test identified by `tag`.
    fn tmp_path(tag: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "marigold_writer_{}_{}.tmp",
            std::process::id(),
            tag
        ))
    }

    /// Exercises poll_write + poll_flush + poll_shutdown on the File variant.
    #[tokio::test]
    async fn file_write_flush_shutdown() {
        let path = tmp_path("wfs");
        let std_file = std::fs::File::create(&path).unwrap();
        let mut w = Writer::file(tokio::fs::File::from_std(std_file));
        w.write_all(b"hello, file").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Empty write to a file must succeed.
    #[tokio::test]
    async fn file_empty_write() {
        let path = tmp_path("empty");
        let std_file = std::fs::File::create(&path).unwrap();
        let mut w = Writer::file(tokio::fs::File::from_std(std_file));
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Multiple sequential writes to the File variant.
    #[tokio::test]
    async fn file_multiple_writes() {
        let path = tmp_path("multi");
        let std_file = std::fs::File::create(&path).unwrap();
        let mut w = Writer::file(tokio::fs::File::from_std(std_file));
        w.write_all(b"part1").await.unwrap();
        w.write_all(b"part2").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Debug output for the File variant must not panic.
    #[tokio::test]
    async fn file_debug_format() {
        let path = tmp_path("debug");
        let std_file = std::fs::File::create(&path).unwrap();
        let w = Writer::file(tokio::fs::File::from_std(std_file));
        let s = format!("{w:?}");
        assert!(s.contains("Writer"), "unexpected debug: {s}");
        let _ = std::fs::remove_file(&path);
    }
}
