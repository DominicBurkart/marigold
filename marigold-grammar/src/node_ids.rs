//! Stable identifiers and content hashes for the nodes of Marigold stream expressions.
//!
//! Every input, stream function and output of every stream expression, plus every stream
//! variable declaration, gets a [`NodeId`] that survives unrelated edits, and a content hash that
//! changes when the node's own source changes.
//!
//! # Hash algorithm
//!
//! All hashes are 64-bit FNV-1a (offset basis `0xcbf29ce484222325`, prime `0x100000001b3`)
//! rendered as 16 lowercase hex digits. Multi-field inputs are fed as length-prefixed fields: each
//! string is preceded by its byte length as a little-endian `u64`, and each integer is written as
//! a little-endian `u64`. The algorithm does not depend on process state, the Rust version or any
//! map iteration order, and is part of the contract: a change to it changes every id.
//!
//! # Content hash
//!
//! The content hash of a node is FNV-1a of its normalised source. Normalisation removes every
//! ASCII whitespace character outside double-quoted string literals and keeps string literals
//! verbatim, so re-indenting or re-wrapping a chain changes nothing while editing an argument does.
//!
//! # Id format
//!
//! An id is `mg1-` followed by 16 hex digits. The hashed fields are, in order, the version tag
//! `mg1`, the `program` name, and then:
//!
//! - for a stream variable declaration `x = ...`: the tag `named`, the variable name, the
//!   occurrence index of that name among declarations in the program (0 unless the name is
//!   redeclared), the node kind and the ordinal in the chain.
//! - for an unnamed stream expression: the tag `anon`, the content hash of the whole expression,
//!   the occurrence index among identical expressions, the node kind and the ordinal in the chain.
//!
//! The ordinal is the position within the chain: the input is 0 and each following stream
//! function and the output count up by one. The declaration node itself has ordinal 0 and kind
//! `variable_declaration`.
//!
//! # Equality with macro-generated code
//!
//! Ids and content hashes depend on the program text only through the normalised text: every
//! ASCII whitespace character outside double-quoted string literals is dropped and everything
//! else is kept verbatim. Tokenising a program, as the `m!` macro does before it hands the text to
//! the code generator, can change which whitespace separates tokens but never the non-whitespace
//! characters or the contents of string literals, so the normalised text, the occurrence indices
//! and the ordinals are identical. The ids that `marigold_parse_instrumented` embeds in generated
//! code therefore equal the ids [`crate::marigold_node_ids`] returns for the original `.marigold`
//! text with the same program name. The `range` of a node is different: it indexes the exact text
//! that was parsed, which for a macro is the stringified body, so ranges in generated code are
//! approximate and must not be used to locate source.
//!
//! Consequently edits to other expressions, whitespace, or the arguments of a chain element never
//! change the ids of the remaining nodes; inserting or removing a chain element renumbers the
//! elements after it; and editing an unnamed expression gives all of its nodes new ids.

use crate::diagnostics::ByteRange;
use crate::parser::{MarigoldPestParser, Rule};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x100_0000_01b3;

struct Fnv(u64);

impl Fnv {
    fn new() -> Self {
        Self(FNV_OFFSET)
    }

    fn bytes(&mut self, bytes: &[u8]) {
        for b in bytes {
            self.0 ^= u64::from(*b);
            self.0 = self.0.wrapping_mul(FNV_PRIME);
        }
    }

    fn int(&mut self, n: u64) {
        self.bytes(&n.to_le_bytes());
    }

    fn field(&mut self, s: &str) {
        self.int(s.len() as u64);
        self.bytes(s.as_bytes());
    }
}

fn normalise(src: &str) -> String {
    let mut out = String::with_capacity(src.len());
    let mut in_string = false;
    for c in src.chars() {
        if c == '"' {
            in_string = !in_string;
        }
        if in_string || !c.is_ascii_whitespace() {
            out.push(c);
        }
    }
    out
}

fn content_hash_value(src: &str) -> u64 {
    let mut h = Fnv::new();
    h.bytes(normalise(src).as_bytes());
    h.0
}

#[cfg(feature = "otel")]
pub(crate) fn site_hash(site: &str) -> String {
    let mut h = Fnv::new();
    h.field(site);
    hex(h.0)
}

fn hex(n: u64) -> String {
    format!("{n:016x}")
}

/// A stable identifier for a node of a Marigold program, formatted `mg1-<16 hex digits>`.
///
/// ```
/// use marigold_grammar::marigold_node_ids;
///
/// let nodes = marigold_node_ids("x = range(0, 5)", "demo");
/// let id = nodes[0].id.as_str();
/// assert!(id.starts_with("mg1-"));
/// assert_eq!(id.len(), 4 + 16);
/// assert_eq!(nodes[0].id.to_string(), id);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
#[non_exhaustive]
pub struct NodeId(String);

impl NodeId {
    /// The id as a string slice.
    ///
    /// ```
    /// use marigold_grammar::marigold_node_ids;
    ///
    /// let nodes = marigold_node_ids("range(0, 3).return", "demo");
    /// assert!(nodes[0].id.as_str().starts_with("mg1-"));
    /// ```
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// What a [`NodeInfo`] describes.
///
/// ```
/// use marigold_grammar::{marigold_node_ids, node_ids::NodeKind};
///
/// let nodes = marigold_node_ids("x = range(0, 5).map(f)", "demo");
/// let kinds: Vec<_> = nodes.iter().map(|n| n.kind).collect();
/// assert_eq!(
///     kinds,
///     [NodeKind::VariableDeclaration, NodeKind::Input, NodeKind::StreamFunction]
/// );
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum NodeKind {
    Input,
    StreamFunction,
    Output,
    VariableDeclaration,
}

impl NodeKind {
    fn tag(self) -> &'static str {
        match self {
            NodeKind::Input => "input",
            NodeKind::StreamFunction => "stream_function",
            NodeKind::Output => "output",
            NodeKind::VariableDeclaration => "variable_declaration",
        }
    }
}

/// One identified node of a stream expression.
///
/// `range` slices the program source to the node's text, `expr_index` counts top-level
/// expressions from zero, and `ordinal` is the position within the chain.
///
/// ```
/// use marigold_grammar::marigold_node_ids;
///
/// let src = "range(0, 3).map(f).return";
/// let nodes = marigold_node_ids(src, "demo");
/// assert_eq!(nodes.len(), 3);
/// assert_eq!(&src[nodes[1].range.start..nodes[1].range.end], "map(f)");
/// assert_eq!((nodes[1].expr_index, nodes[1].ordinal), (0, 1));
/// assert_eq!(nodes[1].content_hash.len(), 16);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct NodeInfo {
    pub id: NodeId,
    pub kind: NodeKind,
    pub expr_index: usize,
    pub ordinal: usize,
    pub range: ByteRange,
    pub content_hash: String,
}

#[derive(Default)]
struct Builder<'a> {
    program: &'a str,
    named_seen: HashMap<String, u64>,
    anon_seen: HashMap<u64, u64>,
    out: Vec<NodeInfo>,
}

impl Builder<'_> {
    fn id(&self, scope: &[ScopeField], kind: NodeKind, ordinal: usize) -> NodeId {
        let mut h = Fnv::new();
        h.field("mg1");
        h.field(self.program);
        for field in scope {
            match field {
                ScopeField::Str(s) => h.field(s),
                ScopeField::Int(n) => h.int(*n),
            }
        }
        h.field(kind.tag());
        h.int(ordinal as u64);
        NodeId(format!("mg1-{}", hex(h.0)))
    }

    fn push(
        &mut self,
        scope: &[ScopeField],
        kind: NodeKind,
        ordinal: usize,
        pair: &Pair<Rule>,
        shift: usize,
        expr_index: usize,
    ) {
        let span = pair.as_span();
        let id = self.id(scope, kind, ordinal);
        self.out.push(NodeInfo {
            id,
            kind,
            expr_index,
            ordinal,
            range: ByteRange {
                start: span.start() + shift,
                end: span.end() + shift,
            },
            content_hash: hex(content_hash_value(pair.as_str())),
        });
    }

    fn expr(&mut self, expr: Pair<Rule>, shift: usize, expr_index: usize) {
        let Some(item) = expr.into_inner().next() else {
            return;
        };
        match item.as_rule() {
            Rule::stream => {
                let whole = content_hash_value(item.as_str());
                let seen = self.anon_seen.entry(whole).or_insert(0);
                let occurrence = *seen;
                *seen += 1;
                let scope = [
                    ScopeField::Str("anon".to_string()),
                    ScopeField::Int(whole),
                    ScopeField::Int(occurrence),
                ];
                for (ordinal, part) in item.into_inner().enumerate() {
                    let kind = match (ordinal, part.as_rule()) {
                        (0, _) => NodeKind::Input,
                        (_, Rule::output_function) => NodeKind::Output,
                        _ => NodeKind::StreamFunction,
                    };
                    self.push(&scope, kind, ordinal, &part, shift, expr_index);
                }
            }
            Rule::stream_variable_declaration => {
                let mut parts = item.clone().into_inner();
                let Some(name) = parts.next().map(|p| p.as_str().to_string()) else {
                    return;
                };
                let seen = self.named_seen.entry(name.clone()).or_insert(0);
                let occurrence = *seen;
                *seen += 1;
                let scope = [
                    ScopeField::Str("named".to_string()),
                    ScopeField::Str(name),
                    ScopeField::Int(occurrence),
                ];
                self.push(
                    &scope,
                    NodeKind::VariableDeclaration,
                    0,
                    &item,
                    shift,
                    expr_index,
                );
                for (ordinal, part) in parts.enumerate() {
                    let kind = if ordinal == 0 {
                        NodeKind::Input
                    } else {
                        NodeKind::StreamFunction
                    };
                    self.push(&scope, kind, ordinal, &part, shift, expr_index);
                }
            }
            _ => {}
        }
    }

    fn absorb(&mut self, pairs: Pairs<Rule>, shift: usize, base: usize) -> usize {
        let mut count = 0usize;
        for pair in pairs.flatten() {
            if pair.as_rule() == Rule::expr {
                self.expr(pair, shift, base + count);
                count += 1;
            }
        }
        count
    }
}

enum ScopeField {
    Str(String),
    Int(u64),
}

pub(crate) fn node_infos(src: &str, program: &str) -> Vec<NodeInfo> {
    let mut builder = Builder {
        program,
        ..Builder::default()
    };
    if let Ok(pairs) = MarigoldPestParser::parse(Rule::program, src) {
        builder.absorb(pairs, 0, 0);
        return builder.out;
    }
    let mut exprs = 0usize;
    for chunk in crate::recovery::top_level_chunks(src) {
        if let Ok(pairs) = MarigoldPestParser::parse(Rule::program, &src[chunk.clone()]) {
            exprs += builder.absorb(pairs, chunk.start, exprs);
        }
    }
    builder.out
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::{HashMap, HashSet};

    const POOL: &[&str] = &[
        "a = range(0, 5).map(f).filter(g)",
        "b = a.fold(0, add)",
        "range(0, 3).map(double).return",
        "b.filter(p).return",
        "read_file(\"d\u{fc}\u{1f600}.csv\", csv, struct=Row).ok().write_file(\"\u{fc}ut.csv\", csv)",
        "c = select_all(range(0, 1), range(2, 3).map(f)).filter(g)",
        "range(0, 9).return",
    ];

    fn ids(src: &str) -> Vec<NodeInfo> {
        node_ids(src, "p")
    }

    fn node_ids(src: &str, program: &str) -> Vec<NodeInfo> {
        crate::marigold_node_ids(src, program)
    }

    fn id_hash_map(nodes: &[NodeInfo]) -> HashMap<String, String> {
        nodes
            .iter()
            .map(|n| (n.id.to_string(), n.content_hash.clone()))
            .collect()
    }

    fn join(picks: &[usize]) -> String {
        picks
            .iter()
            .map(|i| POOL[*i])
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn rewrap(src: &str) -> String {
        src.replace(").", ")\n    .").replace(" = ", "  =\n\t")
    }

    #[test]
    fn covers_every_chain_element() {
        let src = "x = range(0, 5).map(f)\nx.filter(g).return";
        let kinds: Vec<_> = ids(src)
            .iter()
            .map(|n| (n.kind, n.expr_index, n.ordinal))
            .collect();
        assert_eq!(
            kinds,
            vec![
                (NodeKind::VariableDeclaration, 0, 0),
                (NodeKind::Input, 0, 0),
                (NodeKind::StreamFunction, 0, 1),
                (NodeKind::Input, 1, 0),
                (NodeKind::StreamFunction, 1, 1),
                (NodeKind::Output, 1, 2),
            ]
        );
    }

    #[test]
    fn ranges_slice_to_node_text() {
        let src = "x = range(0, 5).map(f)\nx.filter(g).return";
        let texts: Vec<_> = ids(src)
            .iter()
            .map(|n| src[n.range.start..n.range.end].to_string())
            .collect();
        assert_eq!(
            texts,
            [
                "x = range(0, 5).map(f)",
                "range(0, 5)",
                "map(f)",
                "x",
                "filter(g)",
                "return"
            ]
        );
    }

    #[test]
    fn golden_ids_never_change() {
        let nodes = node_ids("x = range(0, 5).map(f)\nrange(0, 3).return", "golden");
        let got: Vec<_> = nodes
            .iter()
            .map(|n| format!("{} {}", n.id, n.content_hash))
            .collect();
        assert_eq!(got, GOLDEN);
    }

    const GOLDEN: [&str; 5] = [
        "mg1-b5997726efd8a500 22955144b9b85a00",
        "mg1-1cd7cb27650822a2 c0021a5b466d79f4",
        "mg1-04f93a55a926efec d25c173c1ee34f4a",
        "mg1-f9d85c2fea383cf1 bfed765b465bba3e",
        "mg1-9462bfc177abe3bc c5c7b983377cad5f",
    ];

    #[test]
    fn fnv_known_vectors() {
        let mut h = Fnv::new();
        assert_eq!(h.0, 0xcbf2_9ce4_8422_2325);
        h.bytes(b"a");
        assert_eq!(h.0, 0xaf63_dc4c_8601_ec8c);
        let mut h = Fnv::new();
        h.bytes(b"foobar");
        assert_eq!(h.0, 0x8594_4171_f739_67e8);
    }

    #[test]
    fn deterministic_in_process() {
        let src = join(&[0, 1, 2, 3, 4, 5, 6]);
        assert_eq!(ids(&src), ids(&src));
    }

    #[test]
    fn program_name_changes_ids_not_hashes() {
        let a = node_ids("x = range(0, 5)\nrange(0, 1).return", "one");
        let b = node_ids("x = range(0, 5)\nrange(0, 1).return", "two");
        for (x, y) in a.iter().zip(&b) {
            assert_ne!(x.id, y.id);
            assert_eq!(x.content_hash, y.content_hash);
        }
    }

    #[test]
    fn editing_arguments_keeps_other_chain_ids() {
        let before = ids("x = range(0, 5).map(f).filter(g)");
        let after = ids("x = range(0, 5).map(f2).filter(g)");
        assert_eq!(before.len(), after.len());
        for (b, a) in before.iter().zip(&after) {
            assert_eq!(b.id, a.id);
        }
        assert_ne!(before[2].content_hash, after[2].content_hash);
        assert_eq!(before[1].content_hash, after[1].content_hash);
        assert_eq!(before[3].content_hash, after[3].content_hash);
        assert_ne!(before[0].content_hash, after[0].content_hash);
    }

    #[test]
    fn named_ids_ignore_other_expressions() {
        let alone = ids("x = range(0, 5).map(f)");
        let with = ids("range(0, 1).return\nx = range(0, 5).map(f)\ny = range(2, 3)");
        let wanted: HashSet<_> = alone.iter().map(|n| n.id.clone()).collect();
        let found: HashSet<_> = with.iter().map(|n| n.id.clone()).collect();
        assert!(wanted.is_subset(&found));
    }

    #[test]
    fn unnamed_edit_gives_new_ids() {
        let before = ids("range(0, 5).map(f).return");
        let after = ids("range(0, 5).map(h).return");
        for (b, a) in before.iter().zip(&after) {
            assert_ne!(b.id, a.id);
        }
    }

    #[test]
    fn unnamed_ids_survive_neighbour_changes() {
        let alone = ids("range(0, 5).map(f).return");
        let with = ids("z = range(0, 1)\nrange(0, 5).map(f).return\nrange(7, 8).return");
        let wanted: HashSet<_> = alone.iter().map(|n| n.id.clone()).collect();
        let found: HashSet<_> = with.iter().map(|n| n.id.clone()).collect();
        assert!(wanted.is_subset(&found));
    }

    #[test]
    fn identical_unnamed_expressions_get_distinct_stable_ids() {
        let one = ids("range(0, 5).return");
        let two = ids("range(0, 5).return\nrange(0, 5).return");
        assert_eq!(two.len(), 4);
        assert_eq!(two[0].id, one[0].id);
        assert_ne!(two[0].id, two[2].id);
        assert_ne!(two[1].id, two[3].id);
        assert_eq!(two[0].content_hash, two[2].content_hash);
    }

    #[test]
    fn redeclared_variable_gets_distinct_ids() {
        let nodes = ids("x = range(0, 5)\nx = x.map(f)");
        let unique: HashSet<_> = nodes.iter().map(|n| n.id.clone()).collect();
        assert_eq!(unique.len(), nodes.len());
    }

    #[test]
    fn whitespace_inside_strings_matters() {
        let a = ids("range(0, 1).write_file(\"a b.csv\", csv)");
        let b = ids("range(0, 1).write_file(\"ab.csv\", csv)");
        assert_ne!(a[1].content_hash, b[1].content_hash);
    }

    #[test]
    fn declarations_and_declaration_free_programs() {
        assert!(ids("").is_empty());
        assert!(ids("enum E { A }\nstruct S { a: int[0, 3] }").is_empty());
    }

    #[test]
    fn broken_chunks_do_not_hide_good_ones() {
        let clean = ids("x = range(0, 5).map(f)\ny = range(1, 2)");
        let broken = ids("x = range(0, 5).map(f)\nrange(0, 1).retur\ny = range(1, 2)");
        assert_eq!(broken.len(), clean.len());
        for (c, b) in clean.iter().zip(&broken) {
            assert_eq!(c.id, b.id);
        }
        let src = "x = range(0, 5).map(f)\nrange(0, 1).retur\ny = range(1, 2)";
        for n in &broken {
            assert!(n.range.end <= src.len());
            assert!(!src[n.range.start..n.range.end].is_empty());
        }
        assert_eq!(broken.last().unwrap().expr_index, 1);
    }

    proptest! {
        #[test]
        fn whitespace_edits_keep_ids_and_hashes(
            picks in prop::collection::vec(0..POOL.len(), 1..5),
        ) {
            let src = join(&picks);
            let wrapped = rewrap(&src);
            let a = ids(&src);
            let b = ids(&wrapped);
            prop_assert_eq!(a.len(), b.len());
            for (x, y) in a.iter().zip(&b) {
                prop_assert_eq!(&x.id, &y.id);
                prop_assert_eq!(&x.content_hash, &y.content_hash);
            }
        }

        #[test]
        fn inserting_unrelated_expression_keeps_ids(
            picks in prop::sample::subsequence((0..POOL.len()).collect::<Vec<_>>(), 1..POOL.len()),
            extra_seed in 0usize..POOL.len(),
            at in 0usize..8,
        ) {
            let extra = (0..POOL.len()).cycle().skip(extra_seed).find(|i| !picks.contains(i)).unwrap();
            let base = ids(&join(&picks));
            let mut grown = picks.clone();
            grown.insert(at.min(picks.len()), extra);
            let bigger = id_hash_map(&ids(&join(&grown)));
            for (id, hash) in id_hash_map(&base) {
                prop_assert_eq!(bigger.get(&id), Some(&hash));
            }
            let mut shrunk = picks.clone();
            shrunk.remove(at % picks.len());
            let smaller = id_hash_map(&ids(&join(&shrunk)));
            let full = id_hash_map(&base);
            for (id, hash) in smaller {
                prop_assert_eq!(full.get(&id), Some(&hash));
            }
        }

        #[test]
        fn editing_arguments_of_one_expression_keeps_all_other_ids(
            picks in prop::sample::subsequence((0..POOL.len()).collect::<Vec<_>>(), 2..POOL.len()),
            target in 0usize..8,
        ) {
            let k = target % picks.len();
            let lines: Vec<String> = picks.iter().map(|i| POOL[*i].to_string()).collect();
            let mut edited_lines = lines.clone();
            let edits = [
                ("range(0,", "range(1000,"),
                ("fold(0, add)", "fold(0, adder)"),
                ("filter(p)", "filter(pp)"),
                ("\"d\u{fc}\u{1f600}.csv\"", "\"x.csv\""),
            ];
            let (from, to) = edits
                .iter()
                .find(|(from, _)| lines[k].contains(from))
                .unwrap();
            edited_lines[k] = lines[k].replacen(from, to, 1);
            prop_assert_ne!(&lines[k], &edited_lines[k]);
            let before = ids(&lines.join("\n"));
            let after = ids(&edited_lines.join("\n"));
            prop_assert_eq!(before.len(), after.len());
            for (b, a) in before.iter().zip(&after) {
                prop_assert_eq!(b.expr_index, a.expr_index);
                if b.expr_index != k {
                    prop_assert_eq!(&b.id, &a.id);
                    prop_assert_eq!(&b.content_hash, &a.content_hash);
                }
            }
        }

        #[test]
        fn ids_are_unique_and_ranges_valid(
            picks in prop::collection::vec(0..POOL.len(), 1..6),
        ) {
            let src = join(&picks);
            let nodes = ids(&src);
            let unique: HashSet<_> = nodes.iter().map(|n| n.id.clone()).collect();
            prop_assert_eq!(unique.len(), nodes.len());
            for n in &nodes {
                prop_assert!(n.range.start < n.range.end && n.range.end <= src.len());
                prop_assert!(src.is_char_boundary(n.range.start));
                prop_assert!(src.is_char_boundary(n.range.end));
                prop_assert_eq!(
                    hex(content_hash_value(&src[n.range.start..n.range.end])),
                    n.content_hash.clone()
                );
            }
        }
    }
}
