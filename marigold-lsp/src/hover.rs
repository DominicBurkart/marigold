//! Hover text for agents: signature and complexity, as compact Markdown.
//!
//! ```
//! use lsp_types::Position;
//!
//! let text = "x = range(0, 5)\nx.return";
//! let hover = marigold_lsp::hover::hover(text, Position::new(0, 0)).unwrap();
//! let lsp_types::HoverContents::Markup(markup) = hover.contents else { panic!() };
//! assert_eq!(
//!     markup.value,
//!     "```marigold\nx = range(0, 5)\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 5 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: no"
//! );
//! assert!(marigold_lsp::hover::hover(text, Position::new(0, 1)).is_none());
//! ```

use crate::position::LineIndex;
use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position, Range};
use marigold_grammar::complexity::{Cardinality, NodeComplexity};
use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::symbols::SymbolKind;
use marigold_grammar::{marigold_stream_complexities, marigold_symbols};

const MAX_SOURCE_CHARS: usize = 500;

fn fenced(source: &str) -> String {
    let mut shown: String = source.chars().take(MAX_SOURCE_CHARS).collect();
    if shown.len() < source.len() {
        shown.push('\u{2026}');
    }
    format!("```marigold\n{shown}\n```")
}

const MAX_PLAIN_DIGITS: usize = 15;

/// Renders a cardinality, abbreviating exact values over 15 digits as `~d.de<exp>`.
pub fn format_cardinality(cardinality: &Cardinality) -> String {
    let Cardinality::Exact(n) = cardinality else {
        return cardinality.to_string();
    };
    let digits = n.to_string();
    if digits.len() <= MAX_PLAIN_DIGITS {
        return digits;
    }
    let lead: u32 = digits[..3].parse().unwrap_or(0);
    let mut mantissa = (lead + 5) / 10;
    let mut exponent = digits.len() - 1;
    if mantissa >= 100 {
        mantissa = 10;
        exponent += 1;
    }
    format!("~{}.{}e{exponent}", mantissa / 10, mantissa % 10)
}

const MAX_TRUSTED_CARDINALITY: u64 = 1_000_000;

pub fn function_use_ranges(text: &str) -> Vec<ByteRange> {
    marigold_symbols(text)
        .references
        .iter()
        .filter(|r| r.kind == SymbolKind::Function)
        .map(|r| r.range)
        .collect()
}

pub fn time_estimate(
    node: &NodeComplexity,
    function_uses: &[ByteRange],
) -> Result<String, &'static str> {
    if function_uses
        .iter()
        .any(|r| node.range.start <= r.start && r.end <= node.range.end)
    {
        return Err("calls user functions");
    }
    match &node.complexity.cardinality {
        Cardinality::Exact(n)
            if n.to_string()
                .parse::<u64>()
                .is_ok_and(|v| v <= MAX_TRUSTED_CARDINALITY) =>
        {
            Ok(node.complexity.time_class.to_string())
        }
        Cardinality::Exact(_) => Err("cardinality too large"),
        _ => Err("cardinality unknown"),
    }
}

fn time_claim(node: &NodeComplexity, function_uses: &[ByteRange]) -> String {
    match time_estimate(node, function_uses) {
        Ok(class) => format!("{class} per whole stream"),
        Err(reason) => format!("not estimated ({reason})"),
    }
}

pub fn complexity_line(node: &NodeComplexity, function_uses: &[ByteRange]) -> String {
    let c = &node.complexity;
    let space = if c.collects_input {
        "not estimated (collects input)".to_string()
    } else {
        c.exact_space.to_string()
    };
    format!(
        "analyzer estimate for the whole chain \u{b7} cardinality: {} \u{b7} time: {} \u{b7} space: {} \u{b7} collects input: {}",
        format_cardinality(&c.cardinality),
        time_claim(node, function_uses),
        space,
        if c.collects_input { "yes" } else { "no" }
    )
}

fn kind_label(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::Function => "function",
        SymbolKind::Struct => "struct",
        SymbolKind::Enum => "enum",
        _ => "stream variable",
    }
}

fn line_of(text: &str, offset: usize) -> &str {
    let start = text[..offset].rfind('\n').map_or(0, |i| i + 1);
    let end = text[offset..].find('\n').map_or(text.len(), |i| offset + i);
    text[start..end].trim()
}

fn markup(value: String, range: Range) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(range),
    }
}

/// Hover for the symbol or stream expression at `position`; `None` on whitespace or nothing.
///
/// ```
/// use lsp_types::Position;
/// assert!(marigold_lsp::hover::hover("", Position::new(4, 4)).is_none());
/// ```
pub fn hover(text: &str, position: Position) -> Option<Hover> {
    let lines = LineIndex::new(text);
    let offset = lines.offset(position);
    if text[offset..]
        .chars()
        .next()
        .is_none_or(char::is_whitespace)
    {
        return None;
    }
    let symbols = marigold_symbols(text);
    let nodes = marigold_stream_complexities(text).unwrap_or_default();
    let function_uses = function_use_ranges(text);
    let span = |r: ByteRange| Range::new(lines.position(r.start), lines.position(r.end));
    let node_containing = |inner: ByteRange| {
        nodes
            .iter()
            .find(|n| n.range.start <= inner.start && inner.end <= n.range.end)
    };
    if let Some(symbol) = symbols.symbol_at(offset) {
        let name_range = symbol.range();
        let kind = symbol.kind();
        let declaration = symbols.definition_of(offset);
        let value = match (kind, declaration) {
            (SymbolKind::StreamVariable, Some(decl)) => match node_containing(decl.range) {
                Some(node) => format!(
                    "{}\n{}",
                    fenced(&text[node.range.start..node.range.end]),
                    complexity_line(node, &function_uses)
                ),
                None => format!(
                    "**stream variable** `{}`\n{}",
                    decl.name,
                    fenced(line_of(text, decl.range.start))
                ),
            },
            (_, Some(decl)) => format!(
                "**{}** `{}`\n{}",
                kind_label(kind),
                decl.name,
                fenced(line_of(text, decl.range.start))
            ),
            (_, None) => format!(
                "**{}** `{}` (not declared)",
                kind_label(kind),
                symbol.name()
            ),
        };
        return Some(markup(value, span(name_range)));
    }
    let point = ByteRange {
        start: offset,
        end: offset + 1,
    };
    let node = node_containing(point)?;
    Some(markup(
        format!(
            "{}\n{}",
            fenced(&text[node.range.start..node.range.end]),
            complexity_line(node, &function_uses)
        ),
        span(node.range),
    ))
}
