use crate::diagnostics::ByteRange;
use crate::parser::{MarigoldPestParser, Rule};
use crate::span_index::{ReferenceSource, SpanIndex};
use pest::Parser;
use serde::Serialize;

/// What a symbol names.
///
/// ```
/// use marigold_grammar::{marigold_symbols, symbols::SymbolKind};
///
/// let index = marigold_symbols("enum E { A }\nrange(E).return");
/// assert_eq!(index.declarations[0].kind, SymbolKind::Enum);
/// assert_eq!(index.references[0].kind, SymbolKind::Enum);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum SymbolKind {
    StreamVariable,
    Function,
    Struct,
    Enum,
}

/// A place where a name is declared.
///
/// ```
/// use marigold_grammar::marigold_symbols;
///
/// let src = "fn double(x: i32) -> i32 { x * 2 }";
/// let decl = &marigold_symbols(src).declarations[0];
/// assert_eq!(&src[decl.range.start..decl.range.end], "double");
/// assert_eq!(decl.expr_index, 0);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SymbolDeclaration {
    pub name: String,
    pub range: ByteRange,
    pub kind: SymbolKind,
    pub expr_index: usize,
}

/// A place where a declared name is used.
///
/// ```
/// use marigold_grammar::marigold_symbols;
///
/// let src = "range(0, 3).map(double).return";
/// let reference = &marigold_symbols(src).references[0];
/// assert_eq!(&src[reference.range.start..reference.range.end], "double");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SymbolReference {
    pub name: String,
    pub range: ByteRange,
    pub kind: SymbolKind,
    pub expr_index: usize,
}

/// A declaration or reference found at a byte offset.
///
/// ```
/// use marigold_grammar::{marigold_symbols, symbols::Symbol};
///
/// let index = marigold_symbols("x = range(0, 5)\nx.return");
/// assert!(matches!(index.symbol_at(0), Some(Symbol::Declaration(_))));
/// assert!(matches!(index.symbol_at(16), Some(Symbol::Reference(_))));
/// assert!(index.symbol_at(3).is_none());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Symbol<'a> {
    Declaration(&'a SymbolDeclaration),
    Reference(&'a SymbolReference),
}

impl<'a> Symbol<'a> {
    /// The symbol's name.
    ///
    /// ```
    /// use marigold_grammar::marigold_symbols;
    ///
    /// let index = marigold_symbols("x = range(0, 5)");
    /// assert_eq!(index.symbol_at(0).unwrap().name(), "x");
    /// ```
    pub fn name(&self) -> &'a str {
        match self {
            Symbol::Declaration(d) => &d.name,
            Symbol::Reference(r) => &r.name,
        }
    }

    /// The symbol's byte range in the source.
    ///
    /// ```
    /// use marigold_grammar::marigold_symbols;
    ///
    /// let index = marigold_symbols("x = range(0, 5)");
    /// let range = index.symbol_at(0).unwrap().range();
    /// assert_eq!((range.start, range.end), (0, 1));
    /// ```
    pub fn range(&self) -> ByteRange {
        match self {
            Symbol::Declaration(d) => d.range,
            Symbol::Reference(r) => r.range,
        }
    }

    /// What the symbol names.
    ///
    /// ```
    /// use marigold_grammar::{marigold_symbols, symbols::SymbolKind};
    ///
    /// let index = marigold_symbols("x = range(0, 5)");
    /// assert_eq!(index.symbol_at(0).unwrap().kind(), SymbolKind::StreamVariable);
    /// ```
    pub fn kind(&self) -> SymbolKind {
        match self {
            Symbol::Declaration(d) => d.kind,
            Symbol::Reference(r) => r.kind,
        }
    }
}

/// Every declaration and reference in a Marigold program, in source order.
///
/// ```
/// use marigold_grammar::marigold_symbols;
///
/// let src = "x = range(0, 5).map(f)\nx.return";
/// let index = marigold_symbols(src);
/// let names: Vec<_> = index.references.iter().map(|r| r.name.as_str()).collect();
/// assert_eq!(names, ["f", "x"]);
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SymbolIndex {
    pub declarations: Vec<SymbolDeclaration>,
    pub references: Vec<SymbolReference>,
}

fn kind_of(source: ReferenceSource) -> SymbolKind {
    match source {
        ReferenceSource::MapFn
        | ReferenceSource::FilterFn
        | ReferenceSource::FilterMapFn
        | ReferenceSource::FoldFn
        | ReferenceSource::KeepFirstNFn => SymbolKind::Function,
        ReferenceSource::ReadFileStruct => SymbolKind::Struct,
        ReferenceSource::StreamStart | ReferenceSource::VariableSource => {
            SymbolKind::StreamVariable
        }
        ReferenceSource::RangeEnum => SymbolKind::Enum,
    }
}

impl SymbolIndex {
    pub(crate) fn from_source(src: &str) -> Self {
        if let Ok(pairs) = MarigoldPestParser::parse(Rule::program, src) {
            let mut out = Self::default();
            out.absorb(pairs, 0, 0);
            return out;
        }
        let mut out = Self::default();
        let mut exprs = 0usize;
        for chunk in crate::recovery::top_level_chunks(src) {
            if let Ok(pairs) = MarigoldPestParser::parse(Rule::program, &src[chunk.clone()]) {
                exprs += out.absorb(pairs, chunk.start, exprs);
            }
        }
        out
    }

    fn absorb(&mut self, pairs: pest::iterators::Pairs<Rule>, shift: usize, base: usize) -> usize {
        let count = pairs
            .clone()
            .flatten()
            .filter(|p| p.as_rule() == Rule::expr)
            .count();
        let index = SpanIndex::build(pairs);
        let moved = |r: ByteRange| ByteRange {
            start: r.start + shift,
            end: r.end + shift,
        };
        self.declarations
            .extend(index.declarations.into_iter().map(|d| SymbolDeclaration {
                name: d.name,
                range: moved(d.range),
                kind: d.kind,
                expr_index: d.expr_index + base,
            }));
        self.references
            .extend(index.references.into_iter().map(|r| SymbolReference {
                name: r.name,
                range: moved(r.range),
                kind: kind_of(r.source),
                expr_index: r.expr_index + base,
            }));
        count
    }

    /// The symbol whose range contains `offset`, treating the end of a name as part of it.
    ///
    /// ```
    /// use marigold_grammar::marigold_symbols;
    ///
    /// let index = marigold_symbols("x = range(0, 5)\nx.return");
    /// assert_eq!(index.symbol_at(1).unwrap().name(), "x");
    /// assert!(index.symbol_at(5).is_none());
    /// ```
    pub fn symbol_at(&self, offset: usize) -> Option<Symbol<'_>> {
        let inside = |r: ByteRange| r.start <= offset && offset < r.end;
        let touching = |r: ByteRange| r.start <= offset && offset <= r.end;
        let found = |pred: &dyn Fn(ByteRange) -> bool| {
            self.declarations
                .iter()
                .find(|d| pred(d.range))
                .map(Symbol::Declaration)
                .or_else(|| {
                    self.references
                        .iter()
                        .find(|r| pred(r.range))
                        .map(Symbol::Reference)
                })
        };
        found(&inside).or_else(|| found(&touching))
    }

    /// The declaration the symbol at `offset` resolves to.
    ///
    /// A reference resolves to the latest declaration of the same name and kind in an
    /// earlier expression, or else the first declaration of that name and kind.
    ///
    /// ```
    /// use marigold_grammar::marigold_symbols;
    ///
    /// let src = "x = range(0, 5)\nx = x.map(f)\nx.return";
    /// let index = marigold_symbols(src);
    /// let use_in_second = src.find("x.map").unwrap();
    /// assert_eq!(index.definition_of(use_in_second).unwrap().expr_index, 0);
    /// let use_in_return = src.rfind("x.return").unwrap();
    /// assert_eq!(index.definition_of(use_in_return).unwrap().expr_index, 1);
    /// assert!(index.definition_of(src.find("f)").unwrap()).is_none());
    /// ```
    pub fn definition_of(&self, offset: usize) -> Option<&SymbolDeclaration> {
        match self.symbol_at(offset)? {
            Symbol::Declaration(d) => Some(d),
            Symbol::Reference(r) => {
                let mut candidates = self
                    .declarations
                    .iter()
                    .filter(|d| d.kind == r.kind && d.name == r.name);
                let first = candidates.clone().next();
                candidates.rfind(|d| d.expr_index < r.expr_index).or(first)
            }
        }
    }

    /// Every reference to `name` with the given kind, in source order.
    ///
    /// ```
    /// use marigold_grammar::{marigold_symbols, symbols::SymbolKind};
    ///
    /// let index = marigold_symbols("x = range(0, 5)\nx.return\nx.return");
    /// assert_eq!(index.references_to("x", SymbolKind::StreamVariable).len(), 2);
    /// assert!(index.references_to("x", SymbolKind::Function).is_empty());
    /// ```
    pub fn references_to(&self, name: &str, kind: SymbolKind) -> Vec<&SymbolReference> {
        self.references
            .iter()
            .filter(|r| r.kind == kind && r.name == name)
            .collect()
    }

    /// Every reference that resolves to `declaration` under the rules of [`SymbolIndex::definition_of`].
    ///
    /// ```
    /// use marigold_grammar::marigold_symbols;
    ///
    /// let src = "x = range(0, 5)\nx.return\nx = range(1, 2)\nx.return";
    /// let index = marigold_symbols(src);
    /// let first = &index.declarations[0];
    /// let refs = index.references_of(first);
    /// assert_eq!(refs.len(), 1);
    /// assert_eq!(refs[0].expr_index, 1);
    /// ```
    pub fn references_of(&self, declaration: &SymbolDeclaration) -> Vec<&SymbolReference> {
        self.references_to(&declaration.name, declaration.kind)
            .into_iter()
            .filter(|r| self.definition_of(r.range.start) == Some(declaration))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marigold_symbols;
    use proptest::prelude::*;

    const PROGRAMS: &[&str] = &[
        "range(0, 10).return",
        "x = range(0, 5).map(f)\nx.filter(g).return",
        "enum Color { Red, Green }\nrange(Color).return",
        "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Row).return",
        "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
        "y = range(0, 3).filter_map(fm)\nz = y.fold(0, add)\nz.keep_first_n(3, cmp).return",
        "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)\nq = range(0, 2)\nq.return",
    ];

    fn summary(src: &str) -> Vec<(String, SymbolKind, usize, bool)> {
        let index = marigold_symbols(src);
        let mut out: Vec<_> = index
            .declarations
            .iter()
            .map(|d| (d.name.clone(), d.kind, d.range.start, true))
            .chain(
                index
                    .references
                    .iter()
                    .map(|r| (r.name.clone(), r.kind, r.range.start, false)),
            )
            .collect();
        out.sort_by_key(|t| t.2);
        out
    }

    fn at(
        src: &str,
        needle: &str,
        kind: SymbolKind,
        decl: bool,
    ) -> (String, SymbolKind, usize, bool) {
        (needle.to_string(), kind, src.find(needle).unwrap(), decl)
    }

    #[test]
    fn golden_stream_variable() {
        let src = "x = range(0, 5)\nx.return";
        assert_eq!(
            summary(src),
            vec![
                (String::from("x"), SymbolKind::StreamVariable, 0, true),
                (String::from("x"), SymbolKind::StreamVariable, 16, false),
            ]
        );
    }

    #[test]
    fn golden_function() {
        let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return";
        assert_eq!(
            summary(src),
            vec![
                at(src, "double", SymbolKind::Function, true),
                (
                    String::from("double"),
                    SymbolKind::Function,
                    src.rfind("double").unwrap(),
                    false
                ),
            ]
        );
    }

    #[test]
    fn golden_struct() {
        let src = "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Row).return";
        assert_eq!(
            summary(src),
            vec![
                at(src, "Row", SymbolKind::Struct, true),
                (
                    String::from("Row"),
                    SymbolKind::Struct,
                    src.rfind("Row").unwrap(),
                    false
                ),
            ]
        );
    }

    #[test]
    fn golden_enum() {
        let src = "enum Color { Red, Green }\nrange(Color).return";
        assert_eq!(
            summary(src),
            vec![
                at(src, "Color", SymbolKind::Enum, true),
                (
                    String::from("Color"),
                    SymbolKind::Enum,
                    src.rfind("Color").unwrap(),
                    false
                ),
            ]
        );
    }

    #[test]
    fn every_range_slices_to_its_name() {
        for src in PROGRAMS {
            let index = marigold_symbols(src);
            for d in &index.declarations {
                assert_eq!(&src[d.range.start..d.range.end], d.name, "{src}");
            }
            for r in &index.references {
                assert_eq!(&src[r.range.start..r.range.end], r.name, "{src}");
            }
        }
    }

    #[test]
    fn definition_and_references_round_trip() {
        let src = "x = range(0, 5)\ny = x.map(f)\ny.return\nx.return";
        let index = marigold_symbols(src);
        for d in &index.declarations {
            assert_eq!(index.definition_of(d.range.start), Some(d));
            for r in index.references_of(d) {
                assert_eq!(index.definition_of(r.range.start), Some(d));
            }
        }
        let x = &index.declarations[0];
        assert_eq!(index.references_of(x).len(), 2);
    }

    #[test]
    fn undefined_reference_has_no_definition() {
        let src = "range(0, 3).map(f).return";
        let index = marigold_symbols(src);
        assert!(index.definition_of(src.find('f').unwrap()).is_none());
    }

    #[test]
    fn redeclaration_resolves_to_latest_earlier_declaration() {
        let src = "x = range(0, 5)\nx.return\nx = range(1, 2)\nx.return";
        let index = marigold_symbols(src);
        let first_use = src.find("x.return").unwrap();
        let last_use = src.rfind("x.return").unwrap();
        assert_eq!(index.definition_of(first_use).unwrap().expr_index, 0);
        assert_eq!(index.definition_of(last_use).unwrap().expr_index, 2);
    }

    #[test]
    fn symbol_at_prefers_strict_containment_and_accepts_name_end() {
        let src = "x = range(0, 5)\nx.return";
        let index = marigold_symbols(src);
        assert_eq!(index.symbol_at(1).unwrap().name(), "x");
        assert_eq!(index.symbol_at(17).unwrap().range().start, 16);
        assert!(index.symbol_at(src.len() + 10).is_none());
        assert!(index.symbol_at(2).is_none());
    }

    #[test]
    fn empty_source_has_no_symbols() {
        assert_eq!(marigold_symbols(""), SymbolIndex::default());
    }

    #[test]
    fn broken_chunk_does_not_hide_good_chunk() {
        let src = "x = range(0, 5)\nrange(0, 1).retur\nx.return\n";
        let index = marigold_symbols(src);
        assert_eq!(index.declarations.len(), 1);
        assert_eq!(index.declarations[0].name, "x");
        let ret = src.find("x.return").unwrap();
        assert_eq!(index.references.len(), 1);
        assert_eq!(index.references[0].range.start, ret);
        assert_eq!(index.definition_of(ret).unwrap().range.start, 0);
    }

    #[test]
    fn good_chunk_after_bad_chunk_is_rebased() {
        let src =
            "range(0, 1).retur\nfn double(x: i32) -> i32 { x * 2 }\ny = range(0, 3).map(double)\ny.return\n";
        let index = marigold_symbols(src);
        assert_eq!(index.declarations.len(), 2);
        for d in &index.declarations {
            assert_eq!(&src[d.range.start..d.range.end], d.name);
        }
        for r in &index.references {
            assert_eq!(&src[r.range.start..r.range.end], r.name);
        }
        let def = index.definition_of(src.rfind("double").unwrap()).unwrap();
        assert_eq!(def.name, "double");
        assert_eq!(def.range.start, src.find("double").unwrap());
        assert_eq!(index.references.len(), 2);
    }

    #[test]
    fn one_good_and_one_bad_chunk() {
        let src = "a = range(0, 5)\nb = range(0, 1).filt\na.return";
        let index = marigold_symbols(src);
        assert_eq!(index.declarations.len(), 1);
        assert_eq!(index.declarations[0].name, "a");
    }

    #[test]
    fn unicode_offsets_are_byte_offsets() {
        let src = "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)\nq = range(0, 2)\nq.return";
        let index = marigold_symbols(src);
        let decl = &index.declarations[0];
        assert_eq!(decl.range.start, src.find("q =").unwrap());
        assert_eq!(&src[decl.range.start..decl.range.end], "q");
        assert_eq!(index.definition_of(src.rfind('q').unwrap()), Some(decl));

        let broken = format!("\u{fc}\u{fc}\u{fc}.bad\n{src}");
        let index = marigold_symbols(&broken);
        let decl = &index.declarations[0];
        assert_eq!(&broken[decl.range.start..decl.range.end], "q");
        assert_eq!(decl.range.start, broken.find("q =").unwrap());
    }

    #[test]
    fn kind_serializes_snake_case() {
        let json = serde_json::to_string(&SymbolKind::StreamVariable).unwrap();
        assert_eq!(json, "\"stream_variable\"");
    }

    proptest! {
        #[test]
        fn ranges_slice_to_names_under_truncation(
            idx in 0..PROGRAMS.len(),
            cut in 0usize..200,
        ) {
            let full = PROGRAMS[idx];
            let mut end = cut.min(full.len());
            while !full.is_char_boundary(end) {
                end -= 1;
            }
            let src = format!("{}\n{}", &full[..end], PROGRAMS[(idx + 1) % PROGRAMS.len()]);
            let index = marigold_symbols(&src);
            for d in &index.declarations {
                prop_assert_eq!(&src[d.range.start..d.range.end], d.name.as_str());
            }
            for r in &index.references {
                prop_assert_eq!(&src[r.range.start..r.range.end], r.name.as_str());
            }
            for d in &index.declarations {
                prop_assert_eq!(index.definition_of(d.range.start), Some(d));
            }
        }
    }
}
