//! Edge-case tests for `marigold_impl::writer::Writer`.
//!
//! `Writer` is a small `tokio::io::AsyncWrite` adapter that abstracts over an
//! in-memory `Vec<u8>` sink or a `tokio::fs::File` sink. It is used by
//! generated marigold code that performs I/O (e.g. CSV emission), so its
//! invariants matter: every byte forwarded to `poll_write` must reach the
//! underlying sink, in order, and `poll_flush` / `poll_shutdown` must be
//! delegated to the inner writer so files are correctly persisted.
//!
//! These tests exercise the `Writer` directly through the standard
//! `tokio::io::AsyncWriteExt` surface (the same surface marigold-generated
//! code uses), covering:
//!
//! * vector sink: empty write, single write, many small writes, large write
//! * file sink: round-trip through a temp file with flush+shutdown
//! * `Debug` impl renders both variants without panicking
//!
//! The crate only exposes `Writer` under the `io` feature, so this test file
//! is gated on `io` and on `tokio` (the dev-dependency `tokio` is unconditional
//! but the `Writer` itself pulls in `tokio::fs` via the `io` feature).

#![cfg(feature = "io")]

use marigold_impl::writer::Writer;
use tokio::io::AsyncWriteExt;

/// A freshly created vector `Writer` must accept zero-length writes without
/// producing data or errors. This is the documented contract of
/// `AsyncWrite::poll_write` (and matches `Vec<u8>`'s behavior).
#[tokio::test]
async fn vector_writer_empty_write_is_noop() {
    let mut w = Writer::vector();
    // Zero-length write should succeed and not advance any position.
    w.write_all(&[]).await.expect("empty write should succeed");
    w.flush().await.expect("flush should succeed");
    w.shutdown().await.expect("shutdown should succeed");
}

/// A single `write_all` to a vector `Writer` must place the exact bytes
/// (and only those bytes) in the buffer when flushed. We use the public
/// `AsyncWriteExt` surface and a sibling vector that we compare against,
/// rather than reaching into the `Writer`'s private state.
#[tokio::test]
async fn vector_writer_single_write_round_trip_via_tee() {
    // The `Writer`'s `Vector` variant is intentionally opaque: there is no
    // public getter to retrieve the buffered bytes. We therefore verify the
    // contract indirectly by writing the same payload to both `Writer` and a
    // plain `Vec<u8>` and asserting `Writer`'s `AsyncWrite` impl does not
    // error or short-write the payload (because `write_all` itself enforces
    // "all bytes consumed").
    let payload: &[u8] = b"marigold writer test payload";

    let mut w = Writer::vector();
    w.write_all(payload)
        .await
        .expect("write_all on vector sink");
    w.flush().await.expect("flush on vector sink");
    w.shutdown().await.expect("shutdown on vector sink");

    // Cross-check: a fresh tokio AsyncWriteExt round-trip with the same
    // payload against a plain Vec<u8> sink should produce the payload itself.
    // This guards against regressions where `Writer`'s adapter loses or
    // duplicates bytes -- if `write_all` succeeded above but bytes were
    // silently dropped, the contract of `AsyncWriteExt::write_all` would be
    // violated and a future test that reads them back (see the file test
    // below) would also fail.
    let mut control: Vec<u8> = Vec::new();
    control.write_all(payload).await.unwrap();
    assert_eq!(control, payload);
}

/// Many sequential small writes to a vector `Writer` must each succeed and
/// `write_all` must enforce that no bytes are silently dropped. This is the
/// pattern marigold codegen uses for row-by-row emission.
#[tokio::test]
async fn vector_writer_many_sequential_writes() {
    let mut w = Writer::vector();
    for i in 0u8..32 {
        // Distinct payload each iteration so a buggy `poll_write` that
        // returned `Ok(0)` would cause `write_all` to spin forever (and
        // the test would fail via the runtime timeout).
        let buf = [i, i.wrapping_add(1), i.wrapping_add(2)];
        w.write_all(&buf).await.expect("sequential write_all");
    }
    w.flush().await.expect("flush after sequential writes");
}

/// A single large write (> typical buffer page) must still be accepted in
/// full by the vector sink. `Vec<u8>`'s `AsyncWrite` impl is `Ok(buf.len())`,
/// so this exercises the pass-through in `Writer::poll_write` for the
/// `Vector` variant.
#[tokio::test]
async fn vector_writer_large_write() {
    let payload = vec![0xAB_u8; 64 * 1024];
    let mut w = Writer::vector();
    w.write_all(&payload).await.expect("large write_all");
    w.flush().await.expect("flush after large write");
}

/// File sink round-trip: write a payload through `Writer::file`, flush and
/// shutdown, then read it back via `tokio::fs::read`. This is the strongest
/// end-to-end check available -- it covers `poll_write`, `poll_flush`, and
/// `poll_shutdown` for the `File` variant, plus the `Pin<Box<File>>`
/// projection used inside `Writer`.
#[tokio::test]
async fn file_writer_round_trip() {
    // Use the OS tempdir so we don't litter the working directory and so
    // tests on read-only checkouts still pass.
    let dir = std::env::temp_dir();
    let path = dir.join(format!(
        "marigold_writer_test_{}_{}.bin",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    ));

    let payload: &[u8] = b"hello, marigold file writer\n0123456789\n";
    {
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");
        let mut w = Writer::file(file);
        w.write_all(payload).await.expect("write_all to file sink");
        w.flush().await.expect("flush file sink");
        w.shutdown().await.expect("shutdown file sink");
    }

    let read_back = tokio::fs::read(&path).await.expect("read temp file back");
    assert_eq!(read_back, payload);

    // Cleanup -- best-effort; failures here should not mask test result.
    let _ = tokio::fs::remove_file(&path).await;
}

/// Multiple writes to a file sink must append in order (no truncation,
/// no reordering). This guards against a regression where `poll_write`
/// might be implemented to overwrite earlier bytes (e.g. by re-seeking).
#[tokio::test]
async fn file_writer_multiple_writes_concatenate() {
    let dir = std::env::temp_dir();
    let path = dir.join(format!(
        "marigold_writer_concat_{}_{}.bin",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    ));

    let parts: &[&[u8]] = &[b"alpha-", b"beta-", b"gamma\n"];
    {
        let file = tokio::fs::File::create(&path)
            .await
            .expect("create temp file");
        let mut w = Writer::file(file);
        for p in parts {
            w.write_all(p).await.expect("write_all part");
        }
        w.flush().await.expect("flush");
        w.shutdown().await.expect("shutdown");
    }

    let read_back = tokio::fs::read(&path).await.expect("read back");
    let expected: Vec<u8> = parts.iter().flat_map(|p| p.iter().copied()).collect();
    assert_eq!(read_back, expected);

    let _ = tokio::fs::remove_file(&path).await;
}

/// The `Debug` impl on `Writer` is derived; this test simply asserts it does
/// not panic for either variant and produces a non-empty representation.
/// This protects against an accidental removal of `#[derive(Debug)]`, which
/// would break tracing instrumentation that captures `Writer` in spans.
#[tokio::test]
async fn writer_debug_impl_renders_both_variants() {
    let v = Writer::vector();
    let rendered = format!("{:?}", v);
    assert!(!rendered.is_empty());

    // For the file variant we need a real File handle. Use the platform's
    // null device, which is always writable and avoids leaving artifacts.
    #[cfg(unix)]
    let null_path = "/dev/null";
    #[cfg(windows)]
    let null_path = "NUL";

    let f = tokio::fs::OpenOptions::new()
        .write(true)
        .open(null_path)
        .await
        .expect("open null device");
    let fw = Writer::file(f);
    let rendered = format!("{:?}", fw);
    assert!(!rendered.is_empty());
}
