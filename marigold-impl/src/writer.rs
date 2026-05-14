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

    fn unique_temp_path(prefix: &str) -> std::path::PathBuf {
        use std::sync::atomic::{AtomicU64, Ordering};
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        std::env::temp_dir().join(format!(
            "marigold_{prefix}_{pid}_{n}.tmp",
            pid = std::process::id()
        ))
    }

    #[tokio::test]
    async fn vector_write_all() {
        let mut w = Writer::vector();
        w.write_all(b"hello").await.unwrap();
    }

    #[tokio::test]
    async fn vector_flush() {
        let mut w = Writer::vector();
        w.write_all(b"data").await.unwrap();
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"end").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_empty_write() {
        let mut w = Writer::vector();
        w.write_all(b"").await.unwrap();
        w.flush().await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_multiple_writes() {
        let mut w = Writer::vector();
        for _ in 0..5 {
            w.write_all(b"chunk").await.unwrap();
        }
        w.flush().await.unwrap();
    }

    #[tokio::test]
    async fn vector_retains_written_data() {
        let mut writer = Writer::vector();
        writer.write_all(b"hello world").await.unwrap();
        match &writer.inner {
            WriteTarget::Vector(v) => {
                assert_eq!(v.as_ref().get_ref().as_slice(), b"hello world");
            }
            WriteTarget::File(_) => panic!("expected Vector"),
        }
    }

    #[tokio::test]
    async fn file_write_flush_and_verify() {
        let path = unique_temp_path("writer_file");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"file content").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        drop(writer);
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "file content");
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn file_multiple_writes_and_verify() {
        let path = unique_temp_path("writer_multi");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"part1 ").await.unwrap();
        writer.write_all(b"part2").await.unwrap();
        writer.flush().await.unwrap();
        drop(writer);
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "part1 part2");
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn file_empty_write() {
        let path = unique_temp_path("writer_empty");
        let file = tokio::fs::File::create(&path).await.unwrap();
        let mut writer = Writer::file(file);
        writer.write_all(b"").await.unwrap();
        writer.flush().await.unwrap();
        writer.shutdown().await.unwrap();
        drop(writer);
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "");
        let _ = std::fs::remove_file(&path);
    }
}
