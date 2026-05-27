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

    // ── Vector backend ────────────────────────────────────────────────────────

    /// Basic write + flush + shutdown cycle on the vector backend.
    #[tokio::test]
    async fn vector_write_flush_shutdown() {
        let mut writer = Writer::vector();
        let n = writer.write(b"hello, world").await.expect("write failed");
        assert_eq!(n, 12, "expected 12 bytes written");
        writer.flush().await.expect("flush failed");
        writer.shutdown().await.expect("shutdown failed");
    }

    /// write_all guarantees all bytes are written even if the underlying
    /// poll_write returns fewer bytes on a single call.
    #[tokio::test]
    async fn vector_write_all() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"complete data")
            .await
            .expect("write_all failed");
        writer.flush().await.expect("flush failed");
        writer.shutdown().await.expect("shutdown failed");
    }

    /// Writing multiple chunks in sequence should all succeed.
    #[tokio::test]
    async fn vector_write_multiple_chunks() {
        let mut writer = Writer::vector();
        let chunks: &[&[u8]] = &[b"chunk1", b"chunk2", b"chunk3", b"final"];
        for chunk in chunks {
            let n = writer.write(chunk).await.expect("write failed");
            assert_eq!(n, chunk.len(), "unexpected byte count for chunk");
        }
        writer.flush().await.expect("flush failed");
        writer.shutdown().await.expect("shutdown failed");
    }

    /// Writing an empty byte slice must succeed and report zero bytes written.
    #[tokio::test]
    async fn vector_write_empty_bytes() {
        let mut writer = Writer::vector();
        let n = writer.write(b"").await.expect("write of empty slice failed");
        assert_eq!(n, 0, "empty write should report 0 bytes");
        writer.flush().await.expect("flush failed");
        writer.shutdown().await.expect("shutdown failed");
    }

    /// Interleave empty and non-empty writes to exercise both paths.
    #[tokio::test]
    async fn vector_interleaved_empty_and_nonempty_writes() {
        let mut writer = Writer::vector();
        writer.write_all(b"").await.expect("first empty write failed");
        writer
            .write_all(b"data")
            .await
            .expect("nonempty write failed");
        writer.write_all(b"").await.expect("second empty write failed");
        writer.flush().await.expect("flush failed");
        writer.shutdown().await.expect("shutdown failed");
    }

    // ── File backend ──────────────────────────────────────────────────────────

    /// Create a temporary file, write bytes through `Writer::file`, then read
    /// them back and verify round-trip fidelity.
    #[tokio::test]
    async fn file_write_flush_shutdown() {
        let path = std::env::temp_dir().join("marigold_writer_test_basic.tmp");

        // Write via Writer::file
        {
            let f = tokio::fs::File::create(&path)
                .await
                .expect("could not create temp file");
            let mut writer = Writer::file(f);
            writer
                .write_all(b"hello from file")
                .await
                .expect("write_all failed");
            writer.flush().await.expect("flush failed");
            writer.shutdown().await.expect("shutdown failed");
        }

        // Read back and verify
        let contents = tokio::fs::read(&path)
            .await
            .expect("could not read temp file");
        assert_eq!(contents, b"hello from file");

        // Clean up
        let _ = tokio::fs::remove_file(&path).await;
    }

    /// Write multiple chunks to a file and verify the concatenated content.
    #[tokio::test]
    async fn file_write_multiple_chunks() {
        let path = std::env::temp_dir().join("marigold_writer_test_chunks.tmp");

        {
            let f = tokio::fs::File::create(&path)
                .await
                .expect("could not create temp file");
            let mut writer = Writer::file(f);
            for chunk in &[b"alpha" as &[u8], b"beta", b"gamma"] {
                writer.write_all(chunk).await.expect("write_all failed");
            }
            writer.flush().await.expect("flush failed");
            writer.shutdown().await.expect("shutdown failed");
        }

        let contents = tokio::fs::read(&path)
            .await
            .expect("could not read temp file");
        assert_eq!(contents, b"alphabetagamma");

        let _ = tokio::fs::remove_file(&path).await;
    }

    /// Writing an empty slice to the file backend must succeed.
    #[tokio::test]
    async fn file_write_empty_bytes() {
        let path = std::env::temp_dir().join("marigold_writer_test_empty.tmp");

        {
            let f = tokio::fs::File::create(&path)
                .await
                .expect("could not create temp file");
            let mut writer = Writer::file(f);
            let n = writer
                .write(b"")
                .await
                .expect("empty write to file failed");
            assert_eq!(n, 0, "empty write should report 0 bytes");
            writer.flush().await.expect("flush failed");
            writer.shutdown().await.expect("shutdown failed");
        }

        let contents = tokio::fs::read(&path)
            .await
            .expect("could not read temp file");
        assert!(contents.is_empty(), "file should be empty after zero-byte write");

        let _ = tokio::fs::remove_file(&path).await;
    }

    // ── Debug trait ───────────────────────────────────────────────────────────

    /// `Writer` derives `Debug`; smoke-test that the implementation works.
    #[tokio::test]
    async fn writer_debug_vector() {
        let writer = Writer::vector();
        let dbg = format!("{:?}", writer);
        assert!(dbg.contains("Writer"), "Debug output should mention Writer");
    }

    #[tokio::test]
    async fn writer_debug_file() {
        let path = std::env::temp_dir().join("marigold_writer_test_debug.tmp");
        let f = tokio::fs::File::create(&path)
            .await
            .expect("could not create temp file");
        let writer = Writer::file(f);
        let dbg = format!("{:?}", writer);
        assert!(dbg.contains("Writer"), "Debug output should mention Writer");
        let _ = tokio::fs::remove_file(&path).await;
    }
}
