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
    async fn test_vector_writer_write_all() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"hello, world")
            .await
            .expect("write_all should succeed on vector writer");
    }

    #[tokio::test]
    async fn test_vector_writer_flush() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        writer
            .flush()
            .await
            .expect("flush should succeed on vector writer");
    }

    #[tokio::test]
    async fn test_vector_writer_shutdown() {
        let mut writer = Writer::vector();
        writer.write_all(b"data").await.unwrap();
        writer
            .shutdown()
            .await
            .expect("shutdown should succeed on vector writer");
    }

    #[tokio::test]
    async fn test_vector_writer_multiple_sequential_writes() {
        let mut writer = Writer::vector();
        for i in 0..10u8 {
            writer
                .write_all(&[i])
                .await
                .unwrap_or_else(|e| panic!("write {i} failed: {e}"));
        }
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_empty_write() {
        let mut writer = Writer::vector();
        writer
            .write_all(b"")
            .await
            .expect("empty write_all should succeed");
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_large_write() {
        let data = vec![0u8; 65536];
        let mut writer = Writer::vector();
        writer
            .write_all(&data)
            .await
            .expect("large write_all should succeed");
        writer.flush().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_writer_debug_format() {
        let writer = Writer::vector();
        let debug_output = format!("{writer:?}");
        assert!(
            debug_output.contains("Writer"),
            "Debug output should contain 'Writer', got: {debug_output}"
        );
    }

    #[tokio::test]
    async fn test_file_writer_write_and_flush() {
        let tmp_path = std::env::temp_dir().join(format!(
            "marigold_writer_test_write_{}.tmp",
            std::process::id()
        ));
        let std_file = std::fs::File::create(&tmp_path)
            .expect("failed to create temp file for test_file_writer_write_and_flush");
        let tokio_file = tokio::fs::File::from_std(std_file);
        let mut writer = Writer::file(tokio_file);
        writer
            .write_all(b"hello file")
            .await
            .expect("file write_all should succeed");
        writer.flush().await.expect("file flush should succeed");
        let _ = std::fs::remove_file(&tmp_path);
    }

    #[tokio::test]
    async fn test_file_writer_shutdown() {
        let tmp_path = std::env::temp_dir().join(format!(
            "marigold_writer_test_shutdown_{}.tmp",
            std::process::id()
        ));
        let std_file = std::fs::File::create(&tmp_path)
            .expect("failed to create temp file for test_file_writer_shutdown");
        let tokio_file = tokio::fs::File::from_std(std_file);
        let mut writer = Writer::file(tokio_file);
        writer.write_all(b"shutdown data").await.unwrap();
        writer
            .shutdown()
            .await
            .expect("file shutdown should succeed");
        let _ = std::fs::remove_file(&tmp_path);
    }

    #[tokio::test]
    async fn test_file_writer_debug_format() {
        let tmp_path = std::env::temp_dir().join(format!(
            "marigold_writer_test_debug_{}.tmp",
            std::process::id()
        ));
        let std_file = std::fs::File::create(&tmp_path)
            .expect("failed to create temp file for test_file_writer_debug_format");
        let tokio_file = tokio::fs::File::from_std(std_file);
        let writer = Writer::file(tokio_file);
        let debug_output = format!("{writer:?}");
        assert!(
            debug_output.contains("Writer"),
            "Debug output should contain 'Writer', got: {debug_output}"
        );
        let _ = std::fs::remove_file(&tmp_path);
    }
}
