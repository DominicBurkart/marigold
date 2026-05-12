//! JSON / JSONL streaming readers and writers.
//!
//! This module provides async streaming adapters over the two JSON-family
//! file formats supported by `read_file(...)` and `write_file(...)` in the
//! Marigold grammar:
//!
//! * `json`  — a single top-level JSON array (`[v1, v2, ...]`).
//! * `jsonl` — newline-delimited JSON; one value per line (aka `ndjson`).
//!
//! Both are implemented on top of `tokio` I/O and parsed eagerly (line- or
//! array-element-at-a-time) using [`serde_json`]. Items are yielded through
//! [`futures::Stream`] so they integrate seamlessly with the rest of the
//! Marigold generated pipeline.
//!
//! The corresponding writers wrap a [`tokio::io::AsyncWrite`] sink and emit
//! either framed JSONL lines or a single `[`/`,`/`]` top-level array. They
//! are used by `write_file(path, json)` / `write_file(path, jsonl)`.

use std::pin::Pin;

use futures::stream::{Stream, StreamExt};
use serde::{de::DeserializeOwned, Serialize};
use serde_json::Value;
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader};

/// Async stream of `Result<T, serde_json::Error>` items, read line-by-line
/// from a JSONL/NDJSON source.
///
/// Blank lines (including trailing newlines) are silently skipped so we
/// tolerate POSIX-friendly files with a final `\n`. Any other malformed
/// line is surfaced as an `Err(serde_json::Error)` in the stream but does
/// not terminate it — downstream `.ok()` / `.ok_or_panic()` can decide.
pub fn read_jsonl<R, T>(reader: R) -> impl Stream<Item = Result<T, serde_json::Error>> + Unpin
where
    R: AsyncRead + Unpin + Send + 'static,
    T: DeserializeOwned + Send + 'static,
{
    let buf = BufReader::new(reader);
    let lines = tokio_stream_lines(buf);
    Box::pin(lines.filter_map(|line_res| async move {
        match line_res {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(serde_json::from_str::<T>(trimmed))
                }
            }
            Err(e) => {
                // Surface I/O errors as a synthetic serde_json error so we
                // stay in a single-error-kind stream shape.
                Some(Err(serde_json::Error::io(e)))
            }
        }
    }))
}

/// Async stream of `Result<T, serde_json::Error>` items read from a
/// top-level JSON array (e.g. `[v1, v2, v3]`).
///
/// The underlying reader is fully buffered before decoding — this keeps the
/// implementation simple and correct for arbitrarily-nested element values.
/// If true streaming over multi-gigabyte arrays is ever required the
/// implementation can be swapped for an event-driven parser without
/// changing the public API.
pub fn read_json_array<R, T>(
    reader: R,
) -> Pin<Box<dyn Stream<Item = Result<T, serde_json::Error>> + Send>>
where
    R: AsyncRead + Unpin + Send + 'static,
    T: DeserializeOwned + Send + 'static,
{
    Box::pin(async_stream_json_array::<R, T>(reader))
}

fn async_stream_json_array<R, T>(
    mut reader: R,
) -> impl Stream<Item = Result<T, serde_json::Error>> + Send
where
    R: AsyncRead + Unpin + Send + 'static,
    T: DeserializeOwned + Send + 'static,
{
    async_stream::stream! {
        let mut buf = Vec::new();
        if let Err(e) = tokio::io::AsyncReadExt::read_to_end(&mut reader, &mut buf).await {
            yield Err(serde_json::Error::io(e));
            return;
        }
        // Parse as Value first so we can validate array shape, then
        // re-deserialize each element into T.
        let parsed: Result<Value, _> = serde_json::from_slice(&buf);
        match parsed {
            Ok(Value::Array(items)) => {
                for item in items {
                    yield serde_json::from_value::<T>(item);
                }
            }
            Ok(other) => {
                // Synthesize a serde error describing the shape violation.
                let err = serde_json::from_value::<Vec<Value>>(other).unwrap_err();
                yield Err(err);
            }
            Err(e) => yield Err(e),
        }
    }
}

/// Stream adapter that reads newline-separated byte chunks from an async
/// buffered reader and yields `io::Result<String>` per line.
///
/// Equivalent to `BufReader::lines()` exposed as a Stream so we can use it
/// with `StreamExt`.
fn tokio_stream_lines<R>(
    mut reader: BufReader<R>,
) -> impl Stream<Item = std::io::Result<String>> + Unpin
where
    R: AsyncRead + Unpin + Send + 'static,
{
    Box::pin(async_stream::stream! {
        loop {
            let mut line = String::new();
            match reader.read_line(&mut line).await {
                Ok(0) => break,
                Ok(_) => {
                    // Strip a single trailing \n (and optional preceding \r)
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    yield Ok(line);
                }
                Err(e) => {
                    yield Err(e);
                    break;
                }
            }
        }
    })
}

/// Async JSONL writer. Serializes each item as one line of JSON followed by
/// `\n`. Flushes the underlying writer on [`JsonlWriter::shutdown`].
pub struct JsonlWriter<W: AsyncWrite + Unpin + Send> {
    inner: W,
}

impl<W: AsyncWrite + Unpin + Send> JsonlWriter<W> {
    pub fn new(inner: W) -> Self {
        Self { inner }
    }

    pub async fn write<T: Serialize>(&mut self, value: &T) -> Result<(), std::io::Error> {
        let mut line = serde_json::to_vec(value)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        line.push(b'\n');
        self.inner.write_all(&line).await
    }

    pub async fn shutdown(mut self) -> Result<(), std::io::Error> {
        self.inner.flush().await?;
        self.inner.shutdown().await
    }
}

/// Async JSON-array writer. Opens with `[`, emits items separated by `,`,
/// closes with `]`. Safe to finish via [`JsonArrayWriter::shutdown`] even
/// when no items were written (produces `[]`).
pub struct JsonArrayWriter<W: AsyncWrite + Unpin + Send> {
    inner: W,
    started: bool,
    opened: bool,
}

impl<W: AsyncWrite + Unpin + Send> JsonArrayWriter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            started: false,
            opened: false,
        }
    }

    async fn ensure_opened(&mut self) -> Result<(), std::io::Error> {
        if !self.opened {
            self.inner.write_all(b"[").await?;
            self.opened = true;
        }
        Ok(())
    }

    pub async fn write<T: Serialize>(&mut self, value: &T) -> Result<(), std::io::Error> {
        self.ensure_opened().await?;
        if self.started {
            self.inner.write_all(b",").await?;
        }
        let bytes = serde_json::to_vec(value)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        self.inner.write_all(&bytes).await?;
        self.started = true;
        Ok(())
    }

    pub async fn shutdown(mut self) -> Result<(), std::io::Error> {
        self.ensure_opened().await?;
        self.inner.write_all(b"]").await?;
        self.inner.flush().await?;
        self.inner.shutdown().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::StreamExt;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct Row {
        id: u32,
        name: String,
    }

    fn sample_rows() -> Vec<Row> {
        vec![
            Row {
                id: 1,
                name: "alice".to_string(),
            },
            Row {
                id: 2,
                name: "bob".to_string(),
            },
            Row {
                id: 3,
                name: "carol".to_string(),
            },
        ]
    }

    #[tokio::test]
    async fn jsonl_round_trip() {
        let rows = sample_rows();
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = JsonlWriter::new(&mut buf);
            for r in &rows {
                w.write(r).await.unwrap();
            }
            w.shutdown().await.unwrap();
        }
        // Each row on its own line, trailing \n.
        let as_str = std::str::from_utf8(&buf).unwrap();
        assert_eq!(as_str.lines().count(), 3);

        let reader = std::io::Cursor::new(buf);
        let items: Vec<Row> = read_jsonl::<_, Row>(reader)
            .map(|r| r.expect("json row"))
            .collect()
            .await;
        assert_eq!(items, rows);
    }

    #[tokio::test]
    async fn jsonl_tolerates_blank_lines() {
        // Manually produce a JSONL body with blank lines mixed in.
        let body = b"{\"id\":1,\"name\":\"alice\"}\n\n{\"id\":2,\"name\":\"bob\"}\n".to_vec();
        let reader = std::io::Cursor::new(body);
        let items: Vec<Row> = read_jsonl::<_, Row>(reader)
            .map(|r| r.expect("json row"))
            .collect()
            .await;
        assert_eq!(
            items,
            vec![
                Row {
                    id: 1,
                    name: "alice".to_string()
                },
                Row {
                    id: 2,
                    name: "bob".to_string()
                },
            ]
        );
    }

    #[tokio::test]
    async fn json_array_round_trip() {
        let rows = sample_rows();
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = JsonArrayWriter::new(&mut buf);
            for r in &rows {
                w.write(r).await.unwrap();
            }
            w.shutdown().await.unwrap();
        }
        // Must be a valid JSON array that serde can round-trip.
        let reparsed: Vec<Row> = serde_json::from_slice(&buf).unwrap();
        assert_eq!(reparsed, rows);

        let reader = std::io::Cursor::new(buf);
        let items: Vec<Row> = read_json_array::<_, Row>(reader)
            .map(|r| r.expect("json row"))
            .collect()
            .await;
        assert_eq!(items, rows);
    }

    #[tokio::test]
    async fn json_array_empty_round_trip() {
        let mut buf: Vec<u8> = Vec::new();
        {
            let w: JsonArrayWriter<&mut Vec<u8>> = JsonArrayWriter::new(&mut buf);
            w.shutdown().await.unwrap();
        }
        assert_eq!(std::str::from_utf8(&buf).unwrap(), "[]");

        let reader = std::io::Cursor::new(buf);
        let items: Vec<Row> = read_json_array::<_, Row>(reader)
            .map(|r| r.expect("json row"))
            .collect()
            .await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn json_array_rejects_non_array_root() {
        // Single top-level object is not a valid json-array input.
        let body = br#"{"id":1,"name":"alice"}"#.to_vec();
        let reader = std::io::Cursor::new(body);
        let errs: Vec<_> = read_json_array::<_, Row>(reader)
            .filter_map(|r| async move { r.err() })
            .collect()
            .await;
        assert_eq!(errs.len(), 1);
    }

    #[tokio::test]
    async fn jsonl_reports_malformed_line() {
        // Middle line is not valid JSON; others are.
        let body =
            b"{\"id\":1,\"name\":\"alice\"}\nnot_json\n{\"id\":2,\"name\":\"bob\"}\n".to_vec();
        let reader = std::io::Cursor::new(body);
        let results: Vec<Result<Row, _>> = read_jsonl::<_, Row>(reader).collect().await;
        assert_eq!(results.len(), 3);
        assert!(results[0].is_ok());
        assert!(results[1].is_err());
        assert!(results[2].is_ok());
    }

    #[tokio::test]
    async fn jsonl_scales_to_10k_items() {
        let rows: Vec<Row> = (0..10_000u32)
            .map(|i| Row {
                id: i,
                name: format!("row_{i}"),
            })
            .collect();

        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = JsonlWriter::new(&mut buf);
            for r in &rows {
                w.write(r).await.unwrap();
            }
            w.shutdown().await.unwrap();
        }
        let reader = std::io::Cursor::new(buf);
        let items: Vec<Row> = read_jsonl::<_, Row>(reader)
            .map(|r| r.expect("json row"))
            .collect()
            .await;
        assert_eq!(items.len(), 10_000);
        assert_eq!(items.first(), rows.first());
        assert_eq!(items.last(), rows.last());
    }
}
