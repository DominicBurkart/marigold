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
use lsp_types::{Location, Position, Range, SymbolKind, TextEdit, Uri};
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

/// Declaration names with their name ranges, without computing full extents.
///
/// ```
/// let names = marigold_lsp::nav::declaration_names("enum E { A }");
/// assert_eq!(names[0].name, "E");
/// assert_eq!(names[0].range, names[0].selection_range);
/// ```
pub fn declaration_names(text: &str) -> Vec<OutlineSymbol> {
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let mut decls: Vec<&SymbolDeclaration> = symbols.declarations.iter().collect();
    decls.sort_by_key(|d| d.range.start);
    decls
        .into_iter()
        .map(|d| {
            let range = range_of(&lines, d.range);
            OutlineSymbol {
                name: d.name.clone(),
                kind: lsp_kind(d.kind),
                range,
                selection_range: range,
            }
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameError {
    NoSymbol,
    NoDeclaration,
    InvalidName(String),
    Clash(String),
    Breaks(String),
}

impl std::fmt::Display for RenameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RenameError::NoSymbol => write!(f, "no symbol at this position"),
            RenameError::NoDeclaration => {
                write!(f, "cannot rename: the symbol has no declaration in this document")
            }
            RenameError::InvalidName(name) => write!(
                f,
                "invalid identifier {name:?}: use ASCII letters, digits and underscores, not starting with a digit"
            ),
            RenameError::Clash(name) => {
                write!(f, "{name:?} is already declared with the same kind")
            }
            RenameError::Breaks(why) => write!(f, "rename would introduce errors: {why}"),
        }
    }
}

impl std::error::Error for RenameError {}

/// Whether `name` is an ASCII identifier: a letter or underscore, then letters, digits or underscores.
///
/// ```
/// assert!(marigold_lsp::nav::is_valid_identifier("_row2"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("2row"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("caf\u{e9}"));
/// ```
pub fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// The name range and current name of the symbol under `position`, if it can be renamed.
///
/// ```
/// use lsp_types::Position;
/// let (range, name) = marigold_lsp::nav::prepare_rename("x = range(0, 1)", Position::new(0, 0)).unwrap();
/// assert_eq!(name, "x");
/// assert_eq!(range.end, Position::new(0, 1));
/// ```
pub fn prepare_rename(text: &str, position: Position) -> Option<(Range, String)> {
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let symbol = symbols.symbol_at(lines.offset(position))?;
    Some((range_of(&lines, symbol.range()), symbol.name().to_string()))
}

/// Edits renaming the declaration under `position` and every reference that resolves to it.
///
/// ```
/// use lsp_types::Position;
/// let edits = marigold_lsp::nav::rename_edits("x = range(0, 1)\nx.return", Position::new(0, 0), "y").unwrap();
/// assert_eq!(edits.len(), 2);
/// assert!(marigold_lsp::nav::rename_edits("x = range(0, 1)", Position::new(0, 0), "1y").is_err());
/// ```
pub fn rename_edits(
    text: &str,
    position: Position,
    new_name: &str,
) -> Result<Vec<TextEdit>, RenameError> {
    if !is_valid_identifier(new_name) {
        return Err(RenameError::InvalidName(new_name.to_string()));
    }
    let lines = LineIndex::new(text);
    let symbols = marigold_symbols(text);
    let offset = lines.offset(position);
    if symbols.symbol_at(offset).is_none() {
        return Err(RenameError::NoSymbol);
    }
    let decl = symbols
        .definition_of(offset)
        .ok_or(RenameError::NoDeclaration)?;
    if decl.name == new_name {
        return Ok(Vec::new());
    }
    if symbols
        .declarations
        .iter()
        .any(|d| d.kind == decl.kind && d.name == new_name)
    {
        return Err(RenameError::Clash(new_name.to_string()));
    }
    let mut ranges: Vec<ByteRange> = std::iter::once(decl.range)
        .chain(symbols.references_of(decl).into_iter().map(|r| r.range))
        .collect();
    ranges.sort_by_key(|r| r.start);
    ranges.dedup();
    Ok(ranges
        .into_iter()
        .map(|r| TextEdit::new(range_of(&lines, r), new_name.to_string()))
        .collect())
}

/// Applies non-overlapping `edits` to `text`.
///
/// ```
/// use lsp_types::{Position, Range, TextEdit};
/// let edit = TextEdit::new(Range::new(Position::new(0, 0), Position::new(0, 1)), "y".into());
/// assert_eq!(marigold_lsp::nav::apply_edits("x = 1", &[edit]), "y = 1");
/// ```
pub fn apply_edits(text: &str, edits: &[TextEdit]) -> String {
    let lines = LineIndex::new(text);
    let mut spans: Vec<(usize, usize, &str)> = edits
        .iter()
        .map(|e| {
            (
                lines.offset(e.range.start),
                lines.offset(e.range.end),
                e.new_text.as_str(),
            )
        })
        .collect();
    spans.sort_by_key(|s| std::cmp::Reverse(s.0));
    let mut out = text.to_string();
    for (start, end, new) in spans {
        out.replace_range(start..end.max(start), new);
    }
    out
}

fn error_count(text: &str) -> usize {
    marigold_grammar::marigold_check(text)
        .iter()
        .filter(|d| d.severity == marigold_grammar::diagnostics::Severity::Error)
        .count()
}

/// Like [`rename_edits`], but refuses a rename whose result has more errors than the original.
///
/// ```
/// use lsp_types::Position;
/// let ok = marigold_lsp::nav::rename("x = range(0, 1)\nx.return", Position::new(0, 0), "y");
/// assert_eq!(ok.unwrap().len(), 2);
/// ```
pub fn rename(
    text: &str,
    position: Position,
    new_name: &str,
) -> Result<Vec<TextEdit>, RenameError> {
    let edits = rename_edits(text, position, new_name)?;
    if edits.is_empty() {
        return Ok(edits);
    }
    let renamed = apply_edits(text, &edits);
    let (before, after) = (error_count(text), error_count(&renamed));
    if after > before {
        return Err(RenameError::Breaks(format!(
            "{after} errors after rename, {before} before"
        )));
    }
    Ok(edits)
}
