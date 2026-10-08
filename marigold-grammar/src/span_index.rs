use crate::diagnostics::ByteRange;
use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

pub(crate) use crate::symbols::SymbolKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReferenceSource {
    MapFn,
    FilterFn,
    FilterMapFn,
    FoldFn,
    KeepFirstNFn,
    ReadFileStruct,
    StreamStart,
    VariableSource,
    RangeEnum,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Declaration {
    pub name: String,
    pub range: ByteRange,
    pub kind: SymbolKind,
    pub expr_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Reference {
    pub name: String,
    pub range: ByteRange,
    pub source: ReferenceSource,
    pub expr_index: usize,
}

#[derive(Debug, Default)]
pub(crate) struct SpanIndex {
    pub declarations: Vec<Declaration>,
    pub references: Vec<Reference>,
    pub enum_range_inputs: Vec<(String, ByteRange)>,
    pub bounded_field_types: Vec<(String, ByteRange)>,
    pub bound_type_refs: Vec<(String, ByteRange)>,
}

fn range_of(pair: &Pair<Rule>) -> ByteRange {
    let span = pair.as_span();
    ByteRange {
        start: span.start(),
        end: span.end(),
    }
}

impl SpanIndex {
    pub fn build(pairs: Pairs<Rule>) -> Self {
        let mut index = Self::default();
        let mut expr_count = 0usize;
        let mut expr_index = 0usize;
        for pair in pairs.flatten() {
            match pair.as_rule() {
                Rule::expr => {
                    expr_index = expr_count;
                    expr_count += 1;
                }
                Rule::stream_variable_declaration => index.add_variable(&pair, expr_index),
                Rule::stream => {
                    if let Some(first) = pair.into_inner().next() {
                        index.add_reference(
                            &first,
                            Rule::identifier,
                            ReferenceSource::StreamStart,
                            expr_index,
                        );
                    }
                }
                Rule::fn_decl => index.add_declaration(&pair, SymbolKind::Function, expr_index),
                Rule::enum_decl => index.add_declaration(&pair, SymbolKind::Enum, expr_index),
                Rule::map_fn => {
                    index.add_function_ref(&pair, 0, ReferenceSource::MapFn, expr_index)
                }
                Rule::filter_fn => {
                    index.add_function_ref(&pair, 0, ReferenceSource::FilterFn, expr_index)
                }
                Rule::filter_map_fn => {
                    index.add_function_ref(&pair, 0, ReferenceSource::FilterMapFn, expr_index)
                }
                Rule::fold_fn => {
                    index.add_function_ref(&pair, 1, ReferenceSource::FoldFn, expr_index)
                }
                Rule::keep_first_n_fn => {
                    index.add_function_ref(&pair, 1, ReferenceSource::KeepFirstNFn, expr_index)
                }
                Rule::read_file_csv_input => {
                    let target = pair
                        .into_inner()
                        .find(|p| p.as_rule() == Rule::free_text_identifier);
                    if let Some(target) = target {
                        index.push_reference(&target, ReferenceSource::ReadFileStruct, expr_index);
                    }
                }
                Rule::range_input => {
                    let mut inner = pair.into_inner();
                    if let (Some(name), None) = (inner.next(), inner.next()) {
                        if name.as_rule() == Rule::free_text_identifier {
                            index.push_reference(&name, ReferenceSource::RangeEnum, expr_index);
                            index
                                .enum_range_inputs
                                .push((name.as_str().to_string(), range_of(&name)));
                        }
                    }
                }
                Rule::struct_decl => {
                    index.add_declaration(&pair, SymbolKind::Struct, expr_index);
                    index.add_struct(pair);
                }
                Rule::bound_type_ref => {
                    if let Some(name) = pair.into_inner().next() {
                        index
                            .bound_type_refs
                            .push((name.as_str().to_string(), range_of(&name)));
                    }
                }
                _ => {}
            }
        }
        index
    }

    fn push_reference(&mut self, name: &Pair<Rule>, source: ReferenceSource, expr_index: usize) {
        self.references.push(Reference {
            name: name.as_str().to_string(),
            range: range_of(name),
            source,
            expr_index,
        });
    }

    fn add_reference(
        &mut self,
        name: &Pair<Rule>,
        expected: Rule,
        source: ReferenceSource,
        expr_index: usize,
    ) {
        if name.as_rule() == expected {
            self.push_reference(name, source, expr_index);
        }
    }

    fn add_function_ref(
        &mut self,
        pair: &Pair<Rule>,
        arg: usize,
        source: ReferenceSource,
        expr_index: usize,
    ) {
        if let Some(name) = pair.clone().into_inner().nth(arg) {
            self.add_reference(&name, Rule::free_text_identifier, source, expr_index);
        }
    }

    fn add_declaration(&mut self, pair: &Pair<Rule>, kind: SymbolKind, expr_index: usize) {
        if let Some(name) = pair.clone().into_inner().next() {
            self.declarations.push(Declaration {
                name: name.as_str().to_string(),
                range: range_of(&name),
                kind,
                expr_index,
            });
        }
    }

    fn add_variable(&mut self, pair: &Pair<Rule>, expr_index: usize) {
        self.add_declaration(pair, SymbolKind::StreamVariable, expr_index);
        if let Some(source) = pair.clone().into_inner().nth(1) {
            self.add_reference(
                &source,
                Rule::identifier,
                ReferenceSource::VariableSource,
                expr_index,
            );
        }
    }

    fn add_struct(&mut self, pair: Pair<Rule>) {
        let mut inner = pair.into_inner();
        let Some(struct_name) = inner.next().map(|p| p.as_str().to_string()) else {
            return;
        };
        let fields = inner
            .flatten()
            .filter(|p| p.as_rule() == Rule::struct_field);
        for field in fields {
            let mut parts = field.into_inner();
            let (Some(name), Some(ty)) = (parts.next(), parts.next()) else {
                continue;
            };
            let bounded = ty.clone().into_inner().flatten().find(|p| {
                matches!(
                    p.as_rule(),
                    Rule::bounded_int_type | Rule::bounded_uint_type
                )
            });
            if let Some(bounded) = bounded {
                self.bounded_field_types.push((
                    format!("{struct_name}.{}", name.as_str()),
                    range_of(&bounded),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::MarigoldPestParser;
    use pest::Parser;
    use proptest::prelude::*;

    const PROGRAMS: &[&str] = &[
        "range(0, 10).return",
        "x = range(0, 5).map(f)\nx.filter(g).return",
        "enum Color { Red, Green }\nrange(Color).return",
        "struct Test { value: int[0, 100] }\nrange(0, 1).return",
        "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
        "y = range(0, 3).filter_map(fm)\nz = y.fold(0, add)\nz.keep_first_n(3, cmp).return",
        "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Row).return",
        "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)",
    ];

    fn index(src: &str) -> SpanIndex {
        SpanIndex::build(MarigoldPestParser::parse(Rule::program, src).unwrap())
    }

    fn decls(src: &str) -> Vec<(String, SymbolKind, usize)> {
        index(src)
            .declarations
            .into_iter()
            .map(|d| (d.name, d.kind, d.expr_index))
            .collect()
    }

    fn refs(src: &str) -> Vec<(String, ReferenceSource, usize)> {
        index(src)
            .references
            .into_iter()
            .map(|r| (r.name, r.source, r.expr_index))
            .collect()
    }

    fn r(name: &str, source: ReferenceSource, expr: usize) -> (String, ReferenceSource, usize) {
        (name.to_string(), source, expr)
    }

    fn d(name: &str, kind: SymbolKind, expr: usize) -> (String, SymbolKind, usize) {
        (name.to_string(), kind, expr)
    }

    #[test]
    fn declares_stream_variable_and_references_fn() {
        let src = "x = range(0, 5).map(f)\nx.return";
        assert_eq!(decls(src), vec![d("x", SymbolKind::StreamVariable, 0)]);
        assert_eq!(
            refs(src),
            vec![
                r("f", ReferenceSource::MapFn, 0),
                r("x", ReferenceSource::StreamStart, 1)
            ]
        );
    }

    #[test]
    fn variable_from_variable_references_source() {
        let src = "a = range(0, 5)\nb = a.filter(p)";
        assert_eq!(
            decls(src),
            vec![
                d("a", SymbolKind::StreamVariable, 0),
                d("b", SymbolKind::StreamVariable, 1)
            ]
        );
        assert_eq!(
            refs(src),
            vec![
                r("a", ReferenceSource::VariableSource, 1),
                r("p", ReferenceSource::FilterFn, 1)
            ]
        );
    }

    #[test]
    fn declares_fn() {
        let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return";
        assert_eq!(decls(src), vec![d("double", SymbolKind::Function, 0)]);
        assert_eq!(refs(src), vec![r("double", ReferenceSource::MapFn, 1)]);
    }

    #[test]
    fn declares_struct_and_enum() {
        let src = "struct S { a: int[0, 3] }\nenum E { A, B }\nrange(E).return";
        assert_eq!(
            decls(src),
            vec![d("S", SymbolKind::Struct, 0), d("E", SymbolKind::Enum, 1)]
        );
        assert_eq!(refs(src), vec![r("E", ReferenceSource::RangeEnum, 2)]);
    }

    #[test]
    fn filter_map_fold_and_keep_first_n_references() {
        let src = "range(0, 3).filter_map(fm).fold(0, add).return\nrange(0, 3).keep_first_n(2, cmp).return";
        assert_eq!(
            refs(src),
            vec![
                r("fm", ReferenceSource::FilterMapFn, 0),
                r("add", ReferenceSource::FoldFn, 0),
                r("cmp", ReferenceSource::KeepFirstNFn, 1)
            ]
        );
    }

    #[test]
    fn read_file_struct_reference() {
        let src = "read_file(\"d.csv\", csv, struct=Row).return";
        assert_eq!(
            refs(src),
            vec![r("Row", ReferenceSource::ReadFileStruct, 0)]
        );
    }

    #[test]
    fn literal_arguments_are_not_references() {
        assert!(refs("range(0, 3).fold(0, add).return")
            .iter()
            .all(|(n, _, _)| n == "add"));
        assert!(refs("range(0, 10).return").is_empty());
    }

    #[test]
    fn legacy_collections_still_populated() {
        let idx = index("enum E { A }\nrange(E).return");
        assert_eq!(idx.enum_range_inputs.len(), 1);
    }

    fn assert_slices(src: &str) {
        let idx = index(src);
        let named = idx
            .declarations
            .iter()
            .map(|x| (&x.name, x.range))
            .chain(idx.references.iter().map(|x| (&x.name, x.range)));
        for (name, range) in named {
            assert_eq!(&src[range.start..range.end], name.as_str(), "{src}");
        }
    }

    #[test]
    fn ranges_slice_to_names_for_samples() {
        for src in PROGRAMS {
            assert_slices(src);
        }
    }

    proptest! {
        #[test]
        fn ranges_slice_to_names_for_sampled_programs(
            picks in prop::collection::vec(0..PROGRAMS.len(), 1..4),
        ) {
            let src = picks.iter().map(|i| PROGRAMS[*i]).collect::<Vec<_>>().join("\n");
            if MarigoldPestParser::parse(Rule::program, &src).is_ok() {
                assert_slices(&src);
                let idx = index(&src);
                let exprs = src.lines().count();
                for e in idx.declarations.iter().map(|x| x.expr_index).chain(idx.references.iter().map(|x| x.expr_index)) {
                    prop_assert!(e < exprs);
                }
            }
        }
    }
}
