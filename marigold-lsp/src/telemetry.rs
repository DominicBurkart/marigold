//! Observed telemetry joined to the nodes of a Marigold program.
//!
//! A [`TelemetrySource`] reports per-node counts for the node ids that
//! [`marigold_grammar::marigold_node_ids`] assigns. [`annotate`] joins those counts back onto
//! source ranges and flags annotations whose recorded content hash no longer matches the
//! program text as stale. A failing source never produces an error here: [`annotate`] degrades
//! to no annotations so that diagnostics and hover keep working.
//!
//! ```
//! use marigold_grammar::marigold_node_ids;
//! use marigold_lsp::telemetry::{annotate, NodeStats, StaticSource};
//! use std::time::Duration;
//!
//! let src = "range(0, 3).map(f).return";
//! let nodes = marigold_node_ids(src, "demo");
//! let source = StaticSource::new(vec![NodeStats {
//!     node_id: nodes[1].id.clone(),
//!     observed_inputs: 40,
//!     observed_runs: 2,
//!     errors: 1,
//!     last_seen: Some(1_700_000_000),
//!     window: Duration::from_secs(86_400),
//!     content_hash: None,
//! }]);
//! let annotations = annotate("demo", src, &source);
//! assert_eq!(annotations.len(), 1);
//! assert_eq!(&src[annotations[0].range.start..annotations[0].range.end], "map(f)");
//! assert_eq!(annotations[0].stats.observed_inputs, 40);
//! assert!(!annotations[0].stale);
//! ```

use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::{marigold_node_ids, NodeId, NodeKind};
use std::collections::HashMap;
use std::fmt;
use std::time::Duration;

/// Observed counts for one node over `window`.
///
/// `last_seen` is a Unix timestamp in seconds. `content_hash` is the content hash of the node
/// text that the counts were observed against, when the backend knows it; a mismatch with the
/// current text marks the annotation stale.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeStats {
    pub node_id: NodeId,
    pub observed_inputs: u64,
    pub observed_runs: u64,
    pub errors: u64,
    pub last_seen: Option<u64>,
    pub window: Duration,
    pub content_hash: Option<String>,
}

/// Why a [`TelemetrySource`] could not answer. Messages never contain credentials.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TelemetryError {
    Unavailable(String),
    Unauthorized,
    Malformed(String),
    Config(String),
}

impl fmt::Display for TelemetryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unavailable(why) => write!(f, "telemetry backend unavailable: {why}"),
            Self::Unauthorized => f.write_str("telemetry backend rejected the credentials"),
            Self::Malformed(why) => {
                write!(f, "telemetry backend returned an unusable answer: {why}")
            }
            Self::Config(why) => write!(f, "telemetry is misconfigured: {why}"),
        }
    }
}

impl std::error::Error for TelemetryError {}

/// A backend that can report observed counts for node ids of one program.
///
/// Nodes the backend has never seen are simply absent from the answer.
pub trait TelemetrySource {
    fn node_stats(
        &self,
        program: &str,
        node_ids: &[NodeId],
    ) -> Result<Vec<NodeStats>, TelemetryError>;
}

/// A deterministic in-memory source for tests and examples.
///
/// ```
/// use marigold_lsp::telemetry::{StaticSource, TelemetryError, TelemetrySource};
///
/// let failing = StaticSource::failing(TelemetryError::Unauthorized);
/// assert_eq!(failing.node_stats("demo", &[]), Err(TelemetryError::Unauthorized));
/// ```
#[derive(Debug, Clone, Default)]
pub struct StaticSource {
    stats: Vec<NodeStats>,
    error: Option<TelemetryError>,
}

impl StaticSource {
    pub fn new(stats: Vec<NodeStats>) -> Self {
        Self { stats, error: None }
    }

    pub fn failing(error: TelemetryError) -> Self {
        Self {
            stats: Vec::new(),
            error: Some(error),
        }
    }
}

impl TelemetrySource for StaticSource {
    fn node_stats(
        &self,
        _program: &str,
        node_ids: &[NodeId],
    ) -> Result<Vec<NodeStats>, TelemetryError> {
        if let Some(error) = &self.error {
            return Err(error.clone());
        }
        Ok(node_ids
            .iter()
            .flat_map(|id| self.stats.iter().filter(move |s| &s.node_id == id))
            .cloned()
            .collect())
    }
}

/// Observed counts attached to one node of the current source text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeAnnotation {
    pub node_id: NodeId,
    pub kind: NodeKind,
    pub range: ByteRange,
    pub stats: NodeStats,
    pub stale: bool,
}

/// Like [`annotate`], but reports why the source failed.
///
/// ```
/// use marigold_lsp::telemetry::{try_annotate, StaticSource, TelemetryError};
///
/// let failing = StaticSource::failing(TelemetryError::Unauthorized);
/// assert_eq!(
///     try_annotate("demo", "range(0, 3).return", &failing),
///     Err(TelemetryError::Unauthorized)
/// );
/// ```
pub fn try_annotate(
    program: &str,
    source_text: &str,
    source: &dyn TelemetrySource,
) -> Result<Vec<NodeAnnotation>, TelemetryError> {
    let nodes = marigold_node_ids(source_text, program);
    if nodes.is_empty() {
        return Ok(Vec::new());
    }
    let ids: Vec<NodeId> = nodes.iter().map(|n| n.id.clone()).collect();
    let mut by_id: HashMap<NodeId, NodeStats> = source
        .node_stats(program, &ids)?
        .into_iter()
        .map(|s| (s.node_id.clone(), s))
        .collect();
    Ok(nodes
        .into_iter()
        .filter_map(|node| {
            let stats = by_id.remove(&node.id)?;
            let stale = stats
                .content_hash
                .as_ref()
                .is_some_and(|recorded| *recorded != node.content_hash);
            Some(NodeAnnotation {
                node_id: node.id,
                kind: node.kind,
                range: node.range,
                stats,
                stale,
            })
        })
        .collect())
}

/// Joins the source's stats to the node ranges of `source_text`, in source order.
///
/// Nodes without stats get no annotation, and a failing source yields no annotations.
pub fn annotate(
    program: &str,
    source_text: &str,
    source: &dyn TelemetrySource,
) -> Vec<NodeAnnotation> {
    try_annotate(program, source_text, source).unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::LineIndex;
    use lsp_types::Position;

    const WINDOW: Duration = Duration::from_secs(86_400);

    fn stats(id: &NodeId, inputs: u64, hash: Option<&str>) -> NodeStats {
        NodeStats {
            node_id: id.clone(),
            observed_inputs: inputs,
            observed_runs: 1,
            errors: 0,
            last_seen: Some(10),
            window: WINDOW,
            content_hash: hash.map(str::to_string),
        }
    }

    #[test]
    fn joins_stats_to_nodes_by_id() {
        let src = "range(0, 3).map(f).return";
        let nodes = marigold_node_ids(src, "demo");
        let source = StaticSource::new(vec![
            stats(&nodes[2].id, 7, None),
            stats(&nodes[0].id, 3, None),
        ]);
        let got = annotate("demo", src, &source);
        assert_eq!(got.len(), 2);
        assert_eq!(got[0].node_id, nodes[0].id);
        assert_eq!(got[0].stats.observed_inputs, 3);
        assert_eq!(got[0].kind, NodeKind::Input);
        assert_eq!(got[1].node_id, nodes[2].id);
        assert_eq!(got[1].range, nodes[2].range);
    }

    #[test]
    fn nodes_without_stats_are_omitted() {
        let src = "range(0, 3).map(f).return";
        let nodes = marigold_node_ids(src, "demo");
        let source = StaticSource::new(vec![stats(&nodes[1].id, 1, None)]);
        assert_eq!(annotate("demo", src, &source).len(), 1);
        assert!(annotate("demo", src, &StaticSource::default()).is_empty());
    }

    #[test]
    fn stats_for_unknown_nodes_are_ignored() {
        let other = marigold_node_ids("range(0, 9).return", "other");
        let source = StaticSource::new(vec![stats(&other[0].id, 1, None)]);
        assert!(annotate("demo", "range(0, 3).return", &source).is_empty());
    }

    #[test]
    fn matching_content_hash_is_fresh() {
        let src = "range(0, 3).map(f).return";
        let nodes = marigold_node_ids(src, "demo");
        let source = StaticSource::new(vec![stats(&nodes[1].id, 1, Some(&nodes[1].content_hash))]);
        assert!(!annotate("demo", src, &source)[0].stale);
    }

    #[test]
    fn edited_node_with_the_same_id_is_stale() {
        let old = "x = range(0, 3).map(f)\nx.return";
        let new = "x = range(0, 3).map(g)\nx.return";
        let old_nodes = marigold_node_ids(old, "demo");
        let new_nodes = marigold_node_ids(new, "demo");
        assert_eq!(old_nodes[2].id, new_nodes[2].id);
        let source = StaticSource::new(vec![stats(
            &old_nodes[2].id,
            1,
            Some(&old_nodes[2].content_hash),
        )]);
        let got = annotate("demo", new, &source);
        assert_eq!(got.len(), 1);
        assert!(got[0].stale);
    }

    #[test]
    fn unknown_content_hash_is_not_stale() {
        let src = "range(0, 3).return";
        let nodes = marigold_node_ids(src, "demo");
        let source = StaticSource::new(vec![stats(&nodes[0].id, 1, None)]);
        assert!(!annotate("demo", src, &source)[0].stale);
    }

    #[test]
    fn source_errors_degrade_to_no_annotations() {
        let source = StaticSource::failing(TelemetryError::Unavailable("down".into()));
        assert!(annotate("demo", "range(0, 3).return", &source).is_empty());
        assert_eq!(
            try_annotate("demo", "range(0, 3).return", &source),
            Err(TelemetryError::Unavailable("down".into()))
        );
    }

    #[test]
    fn unparseable_source_yields_nothing_and_skips_the_backend() {
        let source = StaticSource::failing(TelemetryError::Unauthorized);
        assert_eq!(try_annotate("demo", "range(", &source), Ok(Vec::new()));
    }

    #[test]
    fn ranges_cover_multibyte_text() {
        let src = "range(0, 1).write_file(\"\u{e9}\u{1f600}.csv\", csv)\nx = range(0, 2).map(f)\nx.return";
        let nodes = marigold_node_ids(src, "demo");
        let source = StaticSource::new(nodes.iter().map(|n| stats(&n.id, 1, None)).collect());
        let got = annotate("demo", src, &source);
        assert_eq!(got.len(), nodes.len());
        let lines = LineIndex::new(src);
        let write = got
            .iter()
            .find(|a| src[a.range.start..a.range.end].starts_with("write_file"))
            .unwrap();
        assert_eq!(
            &src[write.range.start..write.range.end],
            "write_file(\"\u{e9}\u{1f600}.csv\", csv)"
        );
        assert_eq!(lines.position(write.range.start), Position::new(0, 12));
        assert_eq!(lines.position(write.range.end), Position::new(0, 38));
        let map = got
            .iter()
            .find(|a| src[a.range.start..a.range.end].starts_with("map"))
            .unwrap();
        assert_eq!(lines.position(map.range.start), Position::new(1, 16));
    }

    #[test]
    fn static_source_answers_only_requested_ids() {
        let nodes = marigold_node_ids("range(0, 3).map(f).return", "demo");
        let source = StaticSource::new(nodes.iter().map(|n| stats(&n.id, 1, None)).collect());
        let got = source.node_stats("demo", &[nodes[1].id.clone()]).unwrap();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].node_id, nodes[1].id);
    }
}
