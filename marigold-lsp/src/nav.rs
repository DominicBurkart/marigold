//! Navigation queries over a single document: definition, references and outline.
//!
//! ```
//! use lsp_types::{Position, Uri};
//! use std::str::FromStr;
//!
//! let uri = Uri::from_str("file:///a.marigold").unwrap();
//! let text = "x = range(0, 3)\nx.return";
//! let def = marigold_lsp::nav::definition(&uri, text, Position::new(1, 0)).unwrap();
//! assert_eq!(def.range.start, Position::new(0, 0));
//! let refs = marigold_lsp::nav::references(&uri, text, Position::new(0, 0), false).unwrap();
//! assert_eq!(refs.len(), 1);
//! ```

use crate::position::LineIndex;
use lsp_types::{Location, Position, Range, SymbolKind, Uri};
use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::symbols::{SymbolDeclaration, SymbolKind as Kind};
use marigold_grammar::{marigold_stream_complexities, marigold_symbols};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutlineSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: Range,
    pub selection_range: Range,
}

pub fn lsp_kind(kind: Kind) -> SymbolKind {
    match kind {
        Kind::Function => SymbolKind::FUNCTION,
        Kind::Struct => SymbolKind::STRUCT,
        Kind::Enum => SymbolKind::ENUM,
        _ => SymbolKind::VARIABLE,
    }
}

fn range_of(index: &LineIndex, r: ByteRange) -> Range {
    Range::new(index.position(r.start), index.position(r.end))
}

/// The declaration site of the symbol under `position`, or `None` when the
/// position is not on a resolvable symbol.
///
/// ```
/// use lsp_types::{Position, Uri};
/// use std::str::FromStr;
///
/// let uri = Uri::from_str("file:///a.marigold").unwrap();
/// assert!(marigold_lsp::nav::definition(&uri, "range(0, 3).return", Position::new(0, 2)).is_none());
/// assert!(marigold_lsp::nav::definition(&uri, "", Position::new(9, 9)).is_none());
/// ```
pub fn definition(uri: &Uri, text: &str, position: Position) -> Option<Location> {
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let decl = symbols.definition_of(lines.offset(position))?;
    Some(Location::new(uri.clone(), range_of(&lines, decl.range)))
}

/// All references to the symbol under `position`, optionally with its declaration first.
///
/// ```
/// use lsp_types::{Position, Uri};
/// use std::str::FromStr;
///
/// let uri = Uri::from_str("file:///a.marigold").unwrap();
/// let text = "x = range(0, 3)\nx.return";
/// let with = marigold_lsp::nav::references(&uri, text, Position::new(1, 0), true).unwrap();
/// assert_eq!(with.len(), 2);
/// assert!(marigold_lsp::nav::references(&uri, text, Position::new(0, 5), true).is_none());
/// ```
pub fn references(
    uri: &Uri,
    text: &str,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let offset = lines.offset(position);
    let symbol = symbols.symbol_at(offset)?;
    let mut ranges = Vec::new();
    match symbols.definition_of(offset) {
        Some(decl) => {
            if include_declaration {
                ranges.push(decl.range);
            }
            ranges.extend(symbols.references_of(decl).into_iter().map(|r| r.range));
        }
        None => ranges.extend(
            symbols
                .references_to(symbol.name(), symbol.kind())
                .into_iter()
                .map(|r| r.range),
        ),
    }
    ranges.sort_by_key(|r| r.start);
    Some(
        ranges
            .into_iter()
            .map(|r| Location::new(uri.clone(), range_of(&lines, r)))
            .collect(),
    )
}

fn block_end(text: &str, from: usize) -> usize {
    let line_end = text[from..].find('\n').map_or(text.len(), |i| from + i);
    let bytes = text.as_bytes();
    let Some(open) = text[from..].find('{').map(|i| from + i) else {
        return text[..line_end].trim_end_matches('\r').len();
    };
    let mut depth = 0usize;
    let mut in_string = false;
    let mut i = open;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' if in_string => i += 1,
            b'"' => in_string = !in_string,
            b'{' if !in_string => depth += 1,
            b'}' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    return i + 1;
                }
            }
            _ => {}
        }
        i += 1;
    }
    text.len()
}

fn full_range(
    text: &str,
    decl: &SymbolDeclaration,
    nodes: &[marigold_grammar::complexity::NodeComplexity],
) -> ByteRange {
    let name = decl.range;
    if decl.kind == Kind::StreamVariable {
        if let Some(node) = nodes
            .iter()
            .find(|n| n.range.start <= name.start && name.end <= n.range.end)
        {
            return node.range;
        }
        let end = text[name.end..]
            .find('\n')
            .map_or(text.len(), |i| name.end + i);
        return ByteRange {
            start: name.start,
            end: text[..end].trim_end_matches('\r').len().max(name.end),
        };
    }
    let before = text[..name.start].trim_end();
    let start = ["fn", "struct", "enum"]
        .iter()
        .find_map(|kw| before.strip_suffix(kw))
        .map_or(name.start, str::len);
    ByteRange {
        start,
        end: block_end(text, name.end).max(name.end),
    }
}

/// Declarations of the document in source order, with full and name ranges.
///
/// ```
/// let text = "enum E { A }\nx = range(0, 1)";
/// let outline = marigold_lsp::nav::outline(text);
/// assert_eq!(outline.len(), 2);
/// assert_eq!(outline[0].name, "E");
/// assert_eq!(outline[1].kind, lsp_types::SymbolKind::VARIABLE);
/// ```
pub fn outline(text: &str) -> Vec<OutlineSymbol> {
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let nodes = marigold_stream_complexities(text).unwrap_or_default();
    let mut decls: Vec<&SymbolDeclaration> = symbols.declarations.iter().collect();
    decls.sort_by_key(|d| d.range.start);
    decls
        .into_iter()
        .map(|d| OutlineSymbol {
            name: d.name.clone(),
            kind: lsp_kind(d.kind),
            range: range_of(&lines, full_range(text, d, &nodes)),
            selection_range: range_of(&lines, d.range),
        })
        .collect()
}
