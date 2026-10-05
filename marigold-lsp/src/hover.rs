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

pub fn complexity_line(node: &NodeComplexity) -> String {
    let c = &node.complexity;
    let space = c.exact_space.to_string();
    format!(
        "analyzer estimate for the whole chain \u{b7} cardinality: {} \u{b7} time: {} per whole stream \u{b7} space: {} \u{b7} collects input: {}",
        format_cardinality(&c.cardinality),
        c.time_class,
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
                    complexity_line(node)
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
            complexity_line(node)
        ),
        span(node.range),
    ))
}
