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
    //! Writer is the AsyncWrite trait object that backs Marigold's generated
    //! `write_file(...)` output, with two concrete targets:
    //!   - `Writer::file` for the real on-disk path.
    //!   - `Writer::vector` as a placeholder used during `mem::replace`
    //!     teardown of the CSV serializer (see
    //!     pest_ast_builder::generate_write_file_no_compression and
    //!     the gzip variant) — the contents are never read but the type
    //!     must still implement `AsyncWrite` correctly.
    //!
    //! These tests pin the polymorphic dispatch: every poll_* method must
    //! delegate to whichever target the Writer was constructed with, so
    //! that adding a new WriteTarget variant or rewriting the match arms
    //! cannot silently regress one of the two paths.
    use super::Writer;
    use tokio::io::AsyncWriteExt;

    #[tokio::test]
    async fn file_writer_round_trips_bytes_to_disk() {
        let dir = std::env::temp_dir().join(format!(
            "marigold_writer_file_{}_{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.bin");

        let payload = b"hello, marigold\n";
        {
            let f = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(f);
            w.write_all(payload).await.unwrap();
            w.flush().await.unwrap();
            w.shutdown().await.unwrap();
        }

        let on_disk = std::fs::read(&path).unwrap();
        assert_eq!(on_disk.as_slice(), payload);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[tokio::test]
    async fn file_writer_handles_multiple_writes_in_order() {
        let dir = std::env::temp_dir().join(format!(
            "marigold_writer_file_multi_{}_{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.bin");

        {
            let f = tokio::fs::File::create(&path).await.unwrap();
            let mut w = Writer::file(f);
            w.write_all(b"a").await.unwrap();
            w.write_all(b"bc").await.unwrap();
            w.write_all(&[]).await.unwrap();
            w.write_all(b"def").await.unwrap();
            w.shutdown().await.unwrap();
        }

        assert_eq!(std::fs::read(&path).unwrap(), b"abcdef");

        let _ = std::fs::remove_dir_all(&dir);
    }

    // The vector target has no public accessor for its buffer, so we cannot
    // observe the bytes directly. What we *can* (and must) verify is that
    // every AsyncWrite method on the vector branch returns Ready/Ok rather
    // than spuriously returning Pending or an error — otherwise the
    // mem::replace teardown path in generated code would deadlock or panic.
    #[tokio::test]
    async fn vector_writer_accepts_writes_flush_and_shutdown() {
        let mut w = Writer::vector();
        w.write_all(b"some-bytes").await.unwrap();
        w.flush().await.unwrap();
        w.write_all(b"more").await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn vector_writer_accepts_empty_write() {
        let mut w = Writer::vector();
        // An empty write must not be misreported as 0-bytes-written-with-error
        // (some AsyncWrite impls treat 0 as EOF); AsyncWriteExt::write_all
        // is the documented invariant we care about.
        w.write_all(&[]).await.unwrap();
        w.shutdown().await.unwrap();
    }

    #[test]
    fn writer_is_debug() {
        // Writer is used inside generic types like csv_async::AsyncSerializer
        // whose Debug derivation requires Writer: Debug. Guarding this
        // prevents an accidental removal of the derive(Debug).
        fn assert_debug<T: std::fmt::Debug>() {}
        assert_debug::<Writer>();
    }
}
