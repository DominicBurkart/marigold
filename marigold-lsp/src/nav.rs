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
#[non_exhaustive]
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

fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn skip_literal_or_comment(bytes: &[u8], i: usize) -> Option<usize> {
    let n = bytes.len();
    match bytes[i] {
        b'/' if bytes.get(i + 1) == Some(&b'/') => Some(
            bytes[i..]
                .iter()
                .position(|&b| b == b'\n')
                .map_or(n, |p| i + p),
        ),
        b'/' if bytes.get(i + 1) == Some(&b'*') => {
            let mut j = i + 2;
            while j < n && !(bytes[j] == b'*' && bytes.get(j + 1) == Some(&b'/')) {
                j += 1;
            }
            Some((j + 2).min(n))
        }
        b'"' => {
            let mut j = i + 1;
            while j < n {
                match bytes[j] {
                    b'\\' => j += 2,
                    b'"' => return Some(j + 1),
                    _ => j += 1,
                }
            }
            Some(n)
        }
        b'r' if i == 0
            || !is_ident_byte(bytes[i - 1])
            || (bytes[i - 1] == b'b' && (i < 2 || !is_ident_byte(bytes[i - 2]))) =>
        {
            let mut j = i + 1;
            while bytes.get(j) == Some(&b'#') {
                j += 1;
            }
            if bytes.get(j) != Some(&b'"') {
                return None;
            }
            let hashes = j - i - 1;
            j += 1;
            while j < n {
                if bytes[j] == b'"'
                    && bytes[j + 1..]
                        .iter()
                        .take(hashes)
                        .filter(|&&b| b == b'#')
                        .count()
                        == hashes
                {
                    return Some(j + 1 + hashes);
                }
                j += 1;
            }
            Some(n)
        }
        b'\'' => {
            if bytes.get(i + 1) == Some(&b'\\') {
                let mut j = i + 2;
                while j < n && bytes[j] != b'\'' && bytes[j] != b'\n' {
                    j += 1;
                }
                return Some((j + 1).min(n));
            }
            let rest = std::str::from_utf8(&bytes[i + 1..]).ok().or_else(|| {
                let end = (i + 1..=n.min(i + 5))
                    .rev()
                    .find(|&e| std::str::from_utf8(&bytes[i + 1..e]).is_ok())?;
                std::str::from_utf8(&bytes[i + 1..end]).ok()
            })?;
            let c = rest.chars().next()?;
            let close = i + 1 + c.len_utf8();
            (bytes.get(close) == Some(&b'\'')).then_some(close + 1)
        }
        _ => None,
    }
}

fn brace_regions(text: &str) -> Vec<(usize, usize)> {
    let bytes = text.as_bytes();
    let mut regions = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        if let Some(next) = skip_literal_or_comment(bytes, i) {
            i = next.max(i + 1);
            continue;
        }
        match bytes[i] {
            b'{' => {
                if depth == 0 {
                    start = i;
                }
                depth += 1;
            }
            b'}' if depth > 0 => {
                depth -= 1;
                if depth == 0 {
                    regions.push((start, i + 1));
                }
            }
            _ => {}
        }
        i += 1;
    }
    if depth > 0 {
        regions.push((start, text.len()));
    }
    regions
}

fn block_end(text: &str, from: usize) -> usize {
    let line_end = text[from..].find('\n').map_or(text.len(), |i| from + i);
    let open = brace_regions(text).into_iter().find(|&(_, end)| end > from);
    match open {
        Some((_, end)) => end,
        None => text[..line_end].trim_end_matches('\r').len(),
    }
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
#[non_exhaustive]
pub enum RenameError {
    NoSymbol,
    NoDeclaration,
    InvalidName(String),
    Clash(String),
    Breaks(String),
    RustBody { name: String, line: usize },
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
            RenameError::RustBody { name, line } => write!(
                f,
                "cannot rename {name:?}: it is also used in Rust code on line {line} (a {{ }} body or signature), which the server cannot rewrite; rename it there by hand first"
            ),
            RenameError::Breaks(why) => write!(f, "rename would introduce errors: {why}"),
        }
    }
}

impl std::error::Error for RenameError {}

const RESERVED_NAMES: &[&str] = &[
    "as",
    "break",
    "const",
    "continue",
    "crate",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
    "async",
    "await",
    "dyn",
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "macro",
    "override",
    "priv",
    "typeof",
    "unsized",
    "virtual",
    "yield",
    "try",
    "gen",
    "union",
    "range",
    "read_file",
    "select_all",
    "map",
    "filter",
    "filter_map",
    "permutations",
    "permutations_with_replacement",
    "combinations",
    "keep_first_n",
    "fold",
    "ok",
    "ok_or_panic",
    "write_file",
    "csv",
    "Vec",
    "Option",
    "Result",
    "String",
    "Box",
    "Some",
    "None",
    "Ok",
    "Err",
    "main",
];

/// Whether `name` is an ASCII identifier that is not a Rust keyword, a bare `_`, a Marigold
/// builtin or a std prelude name: a letter or underscore, then letters, digits or underscores.
///
/// ```
/// assert!(marigold_lsp::nav::is_valid_identifier("_row2"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("match"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("_"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("2row"));
/// assert!(!marigold_lsp::nav::is_valid_identifier("caf\u{e9}"));
/// ```
pub fn is_valid_identifier(name: &str) -> bool {
    if name == "_" || RESERVED_NAMES.contains(&name) {
        return false;
    }
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

fn untracked_use(text: &str, name: &str, covered: &[ByteRange]) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if let Some(next) = skip_literal_or_comment(bytes, i) {
            i = next.max(i + 1);
            continue;
        }
        if is_ident_byte(bytes[i]) {
            let mut j = i;
            while j < bytes.len() && is_ident_byte(bytes[j]) {
                j += 1;
            }
            if &text[i..j] == name && !covered.iter().any(|r| r.start == i) {
                return Some(text[..i].matches('\n').count() + 1);
            }
            i = j;
        } else {
            i += 1;
        }
    }
    None
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
    if decl.kind != Kind::StreamVariable {
        if let Some(line) = untracked_use(text, &decl.name, &ranges) {
            return Err(RenameError::RustBody {
                name: decl.name.clone(),
                line,
            });
        }
    }
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
