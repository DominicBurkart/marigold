//! A [`TelemetrySource`] backed by the Datadog Metrics Query API.
//!
//! The source reads the counters that `marigold-impl`'s `otel` feature exports and that Datadog
//! ingests over OTLP. Credentials come only from the environment: `DD_API_KEY`, `DD_APP_KEY` and
//! optionally `DD_SITE` (default `datadoghq.com`). They are sent as the `DD-API-KEY` and
//! `DD-APPLICATION-KEY` headers over HTTPS, are never logged, and never appear in error messages
//! or in the `Debug` output of [`DatadogSource`].
//!
//! # Metric mapping
//!
//! `marigold-impl` emits these OTLP monotonic counters, each tagged with `marigold.program` and
//! `marigold.node_id`:
//!
//! - `marigold.node.items_in` becomes [`NodeStats::observed_inputs`]
//! - `marigold.node.runs` becomes [`NodeStats::observed_runs`]
//! - `marigold.node.errors.item` plus `marigold.node.errors.abort` become [`NodeStats::errors`]
//!
//! Datadog's OTLP ingestion keeps metric names and attribute keys verbatim, so the dotted names
//! above are the Datadog metric names and `marigold.program` and `marigold.node_id` are tag keys.
//! Tag values are normalised by Datadog (lowercased, characters outside `a-z0-9_-:./` replaced by
//! `_`), so the program name in queries is normalised the same way. Node ids are already
//! lowercase. Each metric is read with one query of the form
//! `sum:marigold.node.runs{marigold.program:demo} by {marigold.node_id}.as_count()` against
//! `GET https://api.<site>/api/v1/query?from=<unix>&to=<unix>&query=<query>`, and the returned
//! points are summed over the window.
//!
//! This mapping follows Datadog's documented OTLP behaviour as recalled by the authors; it has
//! not been verified against a live Datadog account.
//!
//! # Limits
//!
//! Each request has a 5 second timeout and a 4 MiB response cap, redirects are not followed and
//! plain HTTP is refused.
//!
//! ```
//! use marigold_lsp::datadog::DatadogSource;
//! use std::time::Duration;
//!
//! let source = DatadogSource::new("api-key", "app-key", "datadoghq.eu", Duration::from_secs(3600))
//!     .unwrap();
//! let shown = format!("{source:?}");
//! assert!(shown.contains("api.datadoghq.eu"));
//! assert!(!shown.contains("api-key") && !shown.contains("app-key"));
//! assert!(DatadogSource::new("k", "a", "evil.example/x", Duration::from_secs(1)).is_err());
//! ```

use crate::telemetry::{NodeStats, TelemetryError, TelemetrySource};
use marigold_grammar::NodeId;
use serde_json::Value;
use std::collections::HashMap;
use std::fmt;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub const DEFAULT_SITE: &str = "datadoghq.com";
pub const DEFAULT_WINDOW: Duration = Duration::from_secs(24 * 60 * 60);

pub const METRIC_ITEMS_IN: &str = "marigold.node.items_in";
pub const METRIC_RUNS: &str = "marigold.node.runs";
pub const METRIC_ERRORS_ITEM: &str = "marigold.node.errors.item";
pub const METRIC_ERRORS_ABORT: &str = "marigold.node.errors.abort";

const NODE_ID_TAG: &str = "marigold.node_id";
const PROGRAM_TAG: &str = "marigold.program";
const MAX_RESPONSE_BYTES: u64 = 4 * 1024 * 1024;
const REQUEST_TIMEOUT: Duration = Duration::from_secs(5);

/// One node's totals for one metric over the queried window.
#[derive(Debug, Clone, PartialEq)]
pub struct SeriesTotal {
    pub node_id: String,
    pub total: f64,
    pub last_nonzero: Option<u64>,
}

/// Parses a `/api/v1/query` response grouped by `marigold.node_id`.
///
/// Null points are skipped, and `last_nonzero` is the Unix time in seconds of the last point
/// with a positive value.
///
/// ```
/// use marigold_lsp::datadog::parse_query_response;
///
/// let body = r#"{"status":"ok","series":[{"scope":"marigold.node_id:mg1-0123456789abcdef",
///   "pointlist":[[1700000000000.0, 2.0],[1700000300000.0, null],[1700000600000.0, 3.0]]}]}"#;
/// let totals = parse_query_response(body).unwrap();
/// assert_eq!(totals[0].node_id, "mg1-0123456789abcdef");
/// assert_eq!(totals[0].total, 5.0);
/// assert_eq!(totals[0].last_nonzero, Some(1_700_000_600));
/// ```
pub fn parse_query_response(body: &str) -> Result<Vec<SeriesTotal>, TelemetryError> {
    let malformed = |why: &str| TelemetryError::Malformed(why.to_string());
    let value: Value =
        serde_json::from_str(body).map_err(|_| malformed("response is not valid JSON"))?;
    if value.get("status").and_then(Value::as_str) == Some("error") {
        return Err(TelemetryError::Unavailable(
            "Datadog reported a query error".to_string(),
        ));
    }
    let series = match value.get("series") {
        Some(Value::Array(series)) => series,
        None => return Ok(Vec::new()),
        Some(_) => return Err(malformed("series is not an array")),
    };
    let mut totals = Vec::new();
    for entry in series {
        let Some(node_id) = node_id_of(entry) else {
            continue;
        };
        let points = entry
            .get("pointlist")
            .and_then(Value::as_array)
            .ok_or_else(|| malformed("series without a pointlist"))?;
        let mut total = 0.0;
        let mut last_nonzero = None;
        for point in points {
            let pair = point
                .as_array()
                .filter(|p| p.len() == 2)
                .ok_or_else(|| malformed("point is not a [timestamp, value] pair"))?;
            let Some(v) = pair[1].as_f64().filter(|v| v.is_finite()) else {
                continue;
            };
            total += v;
            if v > 0.0 {
                if let Some(ms) = pair[0].as_f64().filter(|ms| *ms >= 0.0) {
                    last_nonzero = Some((ms / 1000.0) as u64);
                }
            }
        }
        totals.push(SeriesTotal {
            node_id,
            total,
            last_nonzero,
        });
    }
    Ok(totals)
}

fn node_id_of(series: &Value) -> Option<String> {
    let prefix = format!("{NODE_ID_TAG}:");
    let from_tags = series
        .get("tag_set")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str);
    let from_scope = series
        .get("scope")
        .and_then(Value::as_str)
        .into_iter()
        .flat_map(|s| s.split(','));
    from_tags
        .chain(from_scope)
        .find_map(|t| t.trim().strip_prefix(prefix.as_str()))
        .map(str::to_string)
}

/// Normalises a program name the way Datadog normalises tag values.
///
/// ```
/// assert_eq!(marigold_lsp::datadog::normalize_tag_value("My App!"), "my_app");
/// ```
pub fn normalize_tag_value(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for c in value.chars().flat_map(char::to_lowercase) {
        let c = if c.is_ascii_alphanumeric() || "_-:./".contains(c) {
            c
        } else {
            '_'
        };
        if c == '_' && out.ends_with('_') {
            continue;
        }
        out.push(c);
    }
    out.trim_end_matches('_').to_string()
}

/// The metrics query for one counter of one program, grouped by node id.
///
/// ```
/// let q = marigold_lsp::datadog::metric_query("marigold.node.runs", "Demo");
/// assert_eq!(
///     q,
///     "sum:marigold.node.runs{marigold.program:demo} by {marigold.node_id}.as_count()"
/// );
/// ```
pub fn metric_query(metric: &str, program: &str) -> String {
    format!(
        "sum:{metric}{{{PROGRAM_TAG}:{}}} by {{{NODE_ID_TAG}}}.as_count()",
        normalize_tag_value(program)
    )
}

fn valid_site(site: &str) -> bool {
    !site.is_empty()
        && site.len() <= 100
        && site.contains('.')
        && !site.starts_with(['.', '-'])
        && !site.ends_with(['.', '-'])
        && !site.contains("..")
        && site
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '.' || c == '-')
}

fn valid_key(key: &str) -> bool {
    !key.is_empty() && key.len() <= 256 && key.bytes().all(|b| b.is_ascii_graphic())
}

/// Reads counters back from Datadog. See the [module documentation](self).
#[derive(Clone)]
pub struct DatadogSource {
    api_key: String,
    app_key: String,
    base_url: String,
    window: Duration,
    timeout: Duration,
    allow_plain_http: bool,
}

impl fmt::Debug for DatadogSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DatadogSource")
            .field("base_url", &self.base_url)
            .field("window", &self.window)
            .field("api_key", &"<redacted>")
            .field("app_key", &"<redacted>")
            .finish()
    }
}

impl DatadogSource {
    pub fn new(
        api_key: &str,
        app_key: &str,
        site: &str,
        window: Duration,
    ) -> Result<Self, TelemetryError> {
        if !valid_key(api_key) || !valid_key(app_key) {
            return Err(TelemetryError::Config(
                "DD_API_KEY and DD_APP_KEY must be non-empty printable ASCII".to_string(),
            ));
        }
        if !valid_site(site) {
            return Err(TelemetryError::Config(
                "DD_SITE must be a bare lowercase host name such as datadoghq.com".to_string(),
            ));
        }
        if window.is_zero() {
            return Err(TelemetryError::Config(
                "the telemetry window must be positive".to_string(),
            ));
        }
        Ok(Self {
            api_key: api_key.to_string(),
            app_key: app_key.to_string(),
            base_url: format!("https://api.{site}"),
            window,
            timeout: REQUEST_TIMEOUT,
            allow_plain_http: false,
        })
    }

    /// Builds a source from `DD_API_KEY`, `DD_APP_KEY` and optional `DD_SITE`.
    pub fn from_env(window: Duration) -> Result<Self, TelemetryError> {
        Self::from_vars(|name| std::env::var(name).ok(), window)
    }

    fn from_vars(
        get: impl Fn(&str) -> Option<String>,
        window: Duration,
    ) -> Result<Self, TelemetryError> {
        let require = |name: &str| {
            get(name)
                .filter(|v| !v.is_empty())
                .ok_or_else(|| TelemetryError::Config(format!("{name} is not set")))
        };
        let api_key = require("DD_API_KEY")?;
        let app_key = require("DD_APP_KEY")?;
        let site = get("DD_SITE")
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| DEFAULT_SITE.to_string());
        Self::new(&api_key, &app_key, &site, window)
    }

    pub fn window(&self) -> Duration {
        self.window
    }

    fn agent(&self) -> ureq::Agent {
        let mut builder = ureq::Agent::config_builder()
            .timeout_global(Some(self.timeout))
            .max_redirects(0)
            .http_status_as_error(false)
            .https_only(!self.allow_plain_http);
        if self.allow_plain_http {
            builder = builder.proxy(None);
        }
        builder.build().into()
    }

    fn query_totals(
        &self,
        agent: &ureq::Agent,
        metric: &str,
        program: &str,
        from: u64,
        to: u64,
    ) -> Result<Vec<SeriesTotal>, TelemetryError> {
        let mut response = agent
            .get(format!("{}/api/v1/query", self.base_url))
            .header("DD-API-KEY", &self.api_key)
            .header("DD-APPLICATION-KEY", &self.app_key)
            .header("Accept", "application/json")
            .query("from", from.to_string())
            .query("to", to.to_string())
            .query("query", metric_query(metric, program))
            .call()
            .map_err(transport_error)?;
        let status = response.status().as_u16();
        match status {
            200..=299 => {}
            401 | 403 => return Err(TelemetryError::Unauthorized),
            other => {
                return Err(TelemetryError::Unavailable(format!(
                    "Datadog answered HTTP {other}"
                )))
            }
        }
        let body = response
            .body_mut()
            .with_config()
            .limit(MAX_RESPONSE_BYTES)
            .read_to_string()
            .map_err(transport_error)?;
        parse_query_response(&body)
    }
}

fn transport_error(err: ureq::Error) -> TelemetryError {
    let why = match err {
        ureq::Error::Timeout(_) => "request timed out",
        ureq::Error::BodyExceedsLimit(_) => "response exceeds the size limit",
        ureq::Error::StatusCode(_) => "unexpected HTTP status",
        ureq::Error::Io(_) | ureq::Error::ConnectionFailed | ureq::Error::HostNotFound => {
            "could not reach Datadog"
        }
        _ => "request failed",
    };
    TelemetryError::Unavailable(why.to_string())
}

#[derive(Default)]
struct Accumulated {
    inputs: f64,
    runs: f64,
    errors: f64,
    last_seen: Option<u64>,
}

type Adder = fn(&mut Accumulated, f64);

fn to_count(value: f64) -> u64 {
    if value.is_finite() && value > 0.0 {
        value.round() as u64
    } else {
        0
    }
}

impl TelemetrySource for DatadogSource {
    fn node_stats(
        &self,
        program: &str,
        node_ids: &[NodeId],
    ) -> Result<Vec<NodeStats>, TelemetryError> {
        if node_ids.is_empty() {
            return Ok(Vec::new());
        }
        let to = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_or(0, |d| d.as_secs());
        let from = to.saturating_sub(self.window.as_secs());
        let agent = self.agent();
        let wanted: HashMap<&str, &NodeId> = node_ids.iter().map(|id| (id.as_str(), id)).collect();
        let mut acc: HashMap<&str, Accumulated> = HashMap::new();
        let metrics: [(&str, Adder); 4] = [
            (METRIC_ITEMS_IN, |a, v| a.inputs += v),
            (METRIC_RUNS, |a, v| a.runs += v),
            (METRIC_ERRORS_ITEM, |a, v| a.errors += v),
            (METRIC_ERRORS_ABORT, |a, v| a.errors += v),
        ];
        for (metric, add) in metrics {
            for series in self.query_totals(&agent, metric, program, from, to)? {
                let Some((key, _)) = wanted.get_key_value(series.node_id.as_str()) else {
                    continue;
                };
                let entry = acc.entry(key).or_default();
                add(entry, series.total);
                entry.last_seen = entry.last_seen.max(series.last_nonzero);
            }
        }
        Ok(node_ids
            .iter()
            .filter_map(|id| {
                let a = acc.get(id.as_str())?;
                Some(NodeStats {
                    node_id: id.clone(),
                    observed_inputs: to_count(a.inputs),
                    observed_runs: to_count(a.runs),
                    errors: to_count(a.errors),
                    last_seen: a.last_seen,
                    window: self.window,
                    content_hash: None,
                })
            })
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use marigold_grammar::marigold_node_ids;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::sync::{Arc, Mutex};
    use std::thread::JoinHandle;

    const WINDOW: Duration = Duration::from_secs(3600);

    fn series_body(entries: &[(&str, &[(f64, Option<f64>)])]) -> String {
        let series: Vec<Value> = entries
            .iter()
            .map(|(id, points)| {
                serde_json::json!({
                    "scope": format!("{NODE_ID_TAG}:{id}"),
                    "tag_set": [format!("{NODE_ID_TAG}:{id}")],
                    "pointlist": points.iter().map(|(t, v)| serde_json::json!([t, v])).collect::<Vec<_>>(),
                })
            })
            .collect();
        serde_json::json!({"status": "ok", "series": series}).to_string()
    }

    struct Fake {
        port: u16,
        requests: Arc<Mutex<Vec<String>>>,
        handle: JoinHandle<()>,
    }

    fn fake_server(
        connections: usize,
        respond: impl Fn(&str) -> (u16, String) + Send + 'static,
    ) -> Fake {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let requests = Arc::new(Mutex::new(Vec::new()));
        let seen = Arc::clone(&requests);
        let handle = std::thread::spawn(move || {
            for _ in 0..connections {
                let Ok((mut stream, _)) = listener.accept() else {
                    return;
                };
                let mut buf = vec![0u8; 16384];
                let mut len = 0;
                loop {
                    let n = stream.read(&mut buf[len..]).unwrap_or(0);
                    if n == 0 {
                        break;
                    }
                    len += n;
                    if buf[..len].windows(4).any(|w| w == b"\r\n\r\n") {
                        break;
                    }
                }
                let request = String::from_utf8_lossy(&buf[..len]).to_string();
                let (status, body) = respond(&request);
                seen.lock().unwrap().push(request);
                let head = format!(
                    "HTTP/1.1 {status} X\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(head.as_bytes());
                let _ = stream.write_all(body.as_bytes());
            }
        });
        Fake {
            port,
            requests,
            handle,
        }
    }

    fn local_source(port: u16) -> DatadogSource {
        let mut source =
            DatadogSource::new("SECRETAPI", "SECRETAPP", "datadoghq.com", WINDOW).unwrap();
        source.base_url = format!("http://127.0.0.1:{port}");
        source.allow_plain_http = true;
        source
    }

    fn decoded(request: &str) -> String {
        let line = request.lines().next().unwrap_or("");
        let target = line.split(' ').nth(1).unwrap_or("");
        let mut out = String::new();
        let bytes = target.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'%' && i + 2 < bytes.len() {
                if let Ok(b) = u8::from_str_radix(&target[i + 1..i + 3], 16) {
                    out.push(b as char);
                    i += 3;
                    continue;
                }
            }
            out.push(bytes[i] as char);
            i += 1;
        }
        out
    }

    fn routed(ids: Vec<String>) -> impl Fn(&str) -> (u16, String) + Send + 'static {
        move |request| {
            let line = decoded(request);
            let points: &[(f64, Option<f64>)] = &[
                (1_700_000_000_000.0, Some(2.0)),
                (1_700_000_300_000.0, Some(3.0)),
            ];
            let one: &[(f64, Option<f64>)] = &[(1_700_000_600_000.0, Some(1.0))];
            let body = if line.contains(METRIC_ITEMS_IN) {
                series_body(&[(&ids[0], points), ("mg1-unrequested0000", points)])
            } else if line.contains(METRIC_RUNS) {
                series_body(&[(&ids[0], one)])
            } else if line.contains(METRIC_ERRORS_ITEM) {
                series_body(&[(&ids[0], one)])
            } else {
                series_body(&[(&ids[0], one)])
            };
            (200, body)
        }
    }

    #[test]
    fn parses_fixture_series_and_skips_null_points() {
        let body = series_body(&[(
            "mg1-aaaaaaaaaaaaaaaa",
            &[(1_000.0, Some(1.0)), (2_000.0, None), (3_000.0, Some(0.0))],
        )]);
        let totals = parse_query_response(&body).unwrap();
        assert_eq!(totals.len(), 1);
        assert_eq!(totals[0].total, 1.0);
        assert_eq!(totals[0].last_nonzero, Some(1));
    }

    #[test]
    fn parse_reads_node_id_from_scope_alone() {
        let body = r#"{"status":"ok","series":[{"scope":"env:x,marigold.node_id:mg1-bbbbbbbbbbbbbbbb","pointlist":[[1000,4]]}]}"#;
        assert_eq!(
            parse_query_response(body).unwrap()[0].node_id,
            "mg1-bbbbbbbbbbbbbbbb"
        );
    }

    #[test]
    fn parse_handles_empty_and_error_documents() {
        assert_eq!(
            parse_query_response(r#"{"status":"ok","series":[]}"#),
            Ok(vec![])
        );
        assert_eq!(parse_query_response(r#"{"status":"ok"}"#), Ok(vec![]));
        assert!(matches!(
            parse_query_response(r#"{"status":"error","error":"bad key SECRET"}"#),
            Err(TelemetryError::Unavailable(m)) if !m.contains("SECRET")
        ));
    }

    #[test]
    fn parse_rejects_malformed_documents() {
        for body in [
            "not json",
            r#"{"series":"x"}"#,
            r#"{"series":[{"scope":"marigold.node_id:a"}]}"#,
            r#"{"series":[{"scope":"marigold.node_id:a","pointlist":[[1]]}]}"#,
        ] {
            assert!(
                matches!(
                    parse_query_response(body),
                    Err(TelemetryError::Malformed(_))
                ),
                "{body}"
            );
        }
    }

    #[test]
    fn series_without_a_node_id_are_ignored() {
        let body = r#"{"series":[{"scope":"host:a","pointlist":[[1000,4]]}]}"#;
        assert_eq!(parse_query_response(body), Ok(vec![]));
    }

    #[test]
    fn query_normalises_the_program_tag() {
        assert_eq!(normalize_tag_value("Demo  App#1"), "demo_app_1");
        assert_eq!(
            metric_query(METRIC_RUNS, "Demo"),
            "sum:marigold.node.runs{marigold.program:demo} by {marigold.node_id}.as_count()"
        );
        assert!(!metric_query(METRIC_RUNS, "a} or {b").contains("} or"));
    }

    #[test]
    fn rejects_bad_site_keys_and_window() {
        let ok = |site: &str| DatadogSource::new("k", "a", site, WINDOW).is_ok();
        assert!(ok("datadoghq.com") && ok("us5.datadoghq.com") && ok("ddog-gov.com"));
        for bad in [
            "",
            "localhost",
            "a.com/x",
            "a.com:443",
            "u@a.com",
            ".a.com",
            "A.com",
            "a..com",
            "a.com-",
        ] {
            assert!(!ok(bad), "{bad}");
        }
        assert!(DatadogSource::new("", "a", "datadoghq.com", WINDOW).is_err());
        assert!(DatadogSource::new("k\n", "a", "datadoghq.com", WINDOW).is_err());
        assert!(DatadogSource::new("k", "a", "datadoghq.com", Duration::ZERO).is_err());
    }

    #[test]
    fn site_defaults_and_is_read_from_the_environment_only() {
        let vars: HashMap<&str, &str> = HashMap::from([("DD_API_KEY", "k"), ("DD_APP_KEY", "a")]);
        let get = |n: &str| vars.get(n).map(|v| v.to_string());
        let source = DatadogSource::from_vars(get, WINDOW).unwrap();
        assert_eq!(source.base_url, "https://api.datadoghq.com");
        let eu: HashMap<&str, &str> = HashMap::from([
            ("DD_API_KEY", "k"),
            ("DD_APP_KEY", "a"),
            ("DD_SITE", "datadoghq.eu"),
        ]);
        let source =
            DatadogSource::from_vars(|n| eu.get(n).map(|v| v.to_string()), WINDOW).unwrap();
        assert_eq!(source.base_url, "https://api.datadoghq.eu");
    }

    #[test]
    fn missing_credentials_name_the_variable_only() {
        let err = DatadogSource::from_vars(|_| None, WINDOW).unwrap_err();
        assert_eq!(err, TelemetryError::Config("DD_API_KEY is not set".into()));
        let err = DatadogSource::from_vars(
            |n| (n == "DD_API_KEY").then(|| "SECRETAPI".to_string()),
            WINDOW,
        )
        .unwrap_err();
        let shown = format!("{err} {err:?}");
        assert!(shown.contains("DD_APP_KEY") && !shown.contains("SECRETAPI"));
    }

    #[test]
    fn debug_output_is_redacted() {
        let source = DatadogSource::new("SECRETAPI", "SECRETAPP", "datadoghq.com", WINDOW).unwrap();
        for shown in [format!("{source:?}"), format!("{source:#?}")] {
            assert!(!shown.contains("SECRETAPI") && !shown.contains("SECRETAPP"));
            assert!(shown.contains("<redacted>"));
        }
    }

    #[test]
    fn fetches_totals_for_requested_nodes_over_the_fake_server() {
        let nodes = marigold_node_ids("range(0, 3).map(f).return", "Demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let fake = fake_server(4, routed(vec![ids[1].to_string()]));
        let stats = local_source(fake.port).node_stats("Demo", &ids).unwrap();
        fake.handle.join().unwrap();
        assert_eq!(stats.len(), 1);
        assert_eq!(stats[0].node_id, ids[1]);
        assert_eq!(stats[0].observed_inputs, 5);
        assert_eq!(stats[0].observed_runs, 1);
        assert_eq!(stats[0].errors, 2);
        assert_eq!(stats[0].last_seen, Some(1_700_000_600));
        assert_eq!(stats[0].window, WINDOW);
        assert_eq!(stats[0].content_hash, None);
    }

    #[test]
    fn requests_carry_headers_and_never_put_keys_in_the_url() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let fake = fake_server(4, routed(vec![ids[0].to_string()]));
        local_source(fake.port).node_stats("demo", &ids).unwrap();
        fake.handle.join().unwrap();
        let requests = fake.requests.lock().unwrap();
        assert_eq!(requests.len(), 4);
        for request in requests.iter() {
            let line = request.lines().next().unwrap();
            assert!(line.starts_with("GET /api/v1/query?"), "{line}");
            assert!(!line.contains("SECRET"));
            let lower = request.to_ascii_lowercase();
            assert!(lower.contains("dd-api-key: secretapi"));
            assert!(lower.contains("dd-application-key: secretapp"));
            let decoded = decoded(request);
            assert!(decoded.contains("from=") && decoded.contains("to="));
            assert!(decoded.contains("marigold.program:demo"));
        }
        let all: String = requests.iter().map(|r| decoded(r)).collect();
        for metric in [
            METRIC_ITEMS_IN,
            METRIC_RUNS,
            METRIC_ERRORS_ITEM,
            METRIC_ERRORS_ABORT,
        ] {
            assert!(all.contains(metric), "{metric}");
        }
    }

    #[test]
    fn forbidden_maps_to_unauthorized_without_leaking_keys() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let fake = fake_server(1, |_| (403, r#"{"errors":["Forbidden SECRETAPI"]}"#.into()));
        let err = local_source(fake.port)
            .node_stats("demo", &ids)
            .unwrap_err();
        assert_eq!(err, TelemetryError::Unauthorized);
        assert!(!format!("{err} {err:?}").contains("SECRET"));
    }

    #[test]
    fn server_errors_map_to_unavailable() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let fake = fake_server(1, |_| (500, "boom SECRETAPI".into()));
        let err = local_source(fake.port)
            .node_stats("demo", &ids)
            .unwrap_err();
        assert_eq!(
            err,
            TelemetryError::Unavailable("Datadog answered HTTP 500".into())
        );
    }

    #[test]
    fn oversized_responses_are_rejected() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let big = format!(
            r#"{{"series":[],"pad":"{}"}}"#,
            "x".repeat(MAX_RESPONSE_BYTES as usize)
        );
        let fake = fake_server(1, move |_| (200, big.clone()));
        let err = local_source(fake.port)
            .node_stats("demo", &ids)
            .unwrap_err();
        assert_eq!(
            err,
            TelemetryError::Unavailable("response exceeds the size limit".into())
        );
    }

    #[test]
    fn stalled_servers_time_out() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let mut source = local_source(listener.local_addr().unwrap().port());
        source.timeout = Duration::from_millis(200);
        let started = std::time::Instant::now();
        let err = source.node_stats("demo", &ids).unwrap_err();
        assert_eq!(err, TelemetryError::Unavailable("request timed out".into()));
        assert!(started.elapsed() < Duration::from_secs(3));
        drop(listener);
    }

    #[test]
    fn plain_http_is_refused_without_contacting_the_server() {
        let nodes = marigold_node_ids("range(0, 3).return", "demo");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        let fake = fake_server(1, |_| (200, "{}".into()));
        let mut source = local_source(fake.port);
        source.allow_plain_http = false;
        assert!(source.node_stats("demo", &ids).is_err());
        assert!(fake.requests.lock().unwrap().is_empty());
        let _ = std::net::TcpStream::connect(("127.0.0.1", fake.port));
        fake.handle.join().unwrap();
    }

    #[test]
    fn no_node_ids_means_no_requests() {
        let source = local_source(1);
        assert_eq!(source.node_stats("demo", &[]), Ok(vec![]));
    }

    #[test]
    #[ignore = "needs DD_API_KEY and DD_APP_KEY and network access to Datadog"]
    fn live_datadog_query_succeeds() {
        let source =
            DatadogSource::from_env(WINDOW).expect("DD_API_KEY and DD_APP_KEY must be set");
        let nodes = marigold_node_ids("range(0, 3).return", "marigold-live-test");
        let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
        source.node_stats("marigold-live-test", &ids).unwrap();
    }
}
