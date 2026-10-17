//! Shared helpers for integration tests that need the repository's Marigold program corpus.
//!
//! Not a test target itself (cargo ignores `tests/<dir>/mod.rs`); include with `mod common;`.
#![allow(dead_code)]

use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::parser::{MarigoldPestParser, Rule};
use pest::Parser;
use std::path::PathBuf;

/// Every `.marigold` / `.mg` file in `tests/programs`, `../examples` and the repo root
/// (same search path as `check_diagnostics::corpus`), sorted for determinism.
pub fn corpus() -> Vec<PathBuf> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in [
        root.join("tests/programs"),
        root.join("../examples"),
        root.join(".."),
    ] {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let ext = path.extension().and_then(|e| e.to_str());
            if path.is_file() && matches!(ext, Some("marigold") | Some("mg")) {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

/// Independent oracle for "the range of top-level expression N": re-derives the
/// convention used by `marigold_stream_complexities` straight from the pest tree
/// (start of the `expr` pair .. start + `as_str().trim_end().len()`).
/// Returns `None` if `src` does not parse.
pub fn expr_ranges(src: &str) -> Option<Vec<ByteRange>> {
    let pairs = MarigoldPestParser::parse(Rule::program, src).ok()?;
    Some(
        pairs
            .flatten()
            .filter(|p| p.as_rule() == Rule::expr)
            .map(|p| ByteRange {
                start: p.as_span().start(),
                end: p.as_span().start() + p.as_str().trim_end().len(),
            })
            .collect(),
    )
}

// ===========================================================================================
// T2: independent oracle for the sub-expression segments
// ===========================================================================================

use marigold_grammar::source_map::SegmentKind as K;
use pest::iterators::Pair;

/// Rule name -> segment kind, for every alternative of `input_function`, `stream_function` and
/// `output_function` in `marigold.pest`. `None` = the construct has NO generated region of its own
/// (`return`), so it must not produce a segment. The test `grammar_alternatives_are_all_covered`
/// reads the grammar file and fails when an alternative is missing from this table.
pub const RULE_KINDS: &[(&str, Option<K>)] = &[
    ("range_input", Some(K::InputRange)),
    ("read_file_csv_input", Some(K::InputReadFile)),
    ("select_all_input", Some(K::InputSelectAll)),
    ("permutations_fn", Some(K::StreamPermutations)),
    (
        "permutations_with_replacement_fn",
        Some(K::StreamPermutationsWithReplacement),
    ),
    ("combinations_fn", Some(K::StreamCombinations)),
    ("keep_first_n_fn", Some(K::StreamKeepFirstN)),
    ("filter_fn", Some(K::StreamFilter)),
    ("filter_map_fn", Some(K::StreamFilterMap)),
    ("map_fn", Some(K::StreamMap)),
    ("fold_fn", Some(K::StreamFold)),
    ("ok_fn", Some(K::StreamOk)),
    ("ok_or_panic_fn", Some(K::StreamOkOrPanic)),
    ("return_fn", None),
    ("write_file_fn", Some(K::OutputWriteFile)),
];

fn rule_kind(rule: Rule) -> Option<K> {
    let name = format!("{rule:?}");
    RULE_KINDS
        .iter()
        .find(|(n, _)| *n == name)
        .unwrap_or_else(|| panic!("rule {name} missing in RULE_KINDS"))
        .1
}

/// pest's implicit WHITESPACE set; NOT `str::trim_end` (which also strips U+00A0 and friends and
/// would cut a legal trailing NBSP off a fn return type).
const PEST_WS: [char; 4] = [' ', '\t', '\r', '\n'];

fn sub_range(p: &Pair<Rule>) -> ByteRange {
    ByteRange {
        start: p.as_span().start(),
        end: p.as_span().start() + p.as_str().trim_end_matches(PEST_WS).len(),
    }
}

/// One expected nested segment: where it must sit in the tree and which source it covers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exp {
    pub expr_index: usize,
    pub kind: K,
    pub source: ByteRange,
    /// kinds of the ancestors, outermost (the statement-level kind) first.
    pub path: Vec<K>,
}

impl Exp {
    /// Total order that does not depend on the implementation's segment order.
    pub fn key(&self) -> (usize, usize, usize, String, String) {
        (
            self.expr_index,
            self.source.start,
            self.source.end,
            format!("{:?}", self.kind),
            format!("{:?}", self.path),
        )
    }
}

/// Re-derives every expected NESTED segment (not the statement-level ones) straight from the pest
/// parse tree. `None` if `src` does not parse. Written independently of the production code.
pub fn expected_nested(src: &str) -> Option<Vec<Exp>> {
    let pairs = MarigoldPestParser::parse(Rule::program, src).ok()?;
    let mut out = Vec::new();
    let exprs = pairs.flatten().filter(|p| p.as_rule() == Rule::expr);
    for (i, expr) in exprs.enumerate() {
        let inner = expr.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::stream => {
                let mut writes_file = false;
                for p in inner.clone().into_inner() {
                    if p.as_rule() == Rule::output_function {
                        writes_file =
                            p.into_inner().next().unwrap().as_rule() == Rule::write_file_fn;
                    }
                }
                let top = if writes_file {
                    K::NonReturningStream
                } else {
                    K::ReturningStream
                };
                chain(inner, i, &[top], &mut out);
            }
            Rule::stream_variable_declaration => chain(inner, i, &[K::StreamVariable], &mut out),
            Rule::struct_decl => {
                for p in inner
                    .into_inner()
                    .flat_map(|b| b.into_inner())
                    .flat_map(|l| l.into_inner())
                    .filter(|p| p.as_rule() == Rule::struct_field)
                {
                    out.push(Exp {
                        expr_index: i,
                        kind: K::StructField,
                        source: sub_range(&p),
                        path: vec![K::Struct],
                    });
                }
            }
            Rule::enum_decl => {
                for body in inner
                    .into_inner()
                    .filter(|p| p.as_rule() == Rule::enum_body)
                {
                    for p in body.into_inner() {
                        let variants: Vec<Pair<Rule>> = match p.as_rule() {
                            Rule::enum_variant_list => p.into_inner().collect(),
                            Rule::default_variant => vec![p],
                            _ => vec![],
                        };
                        for v in variants {
                            out.push(Exp {
                                expr_index: i,
                                kind: K::EnumVariant,
                                source: sub_range(&v),
                                path: vec![K::Enum],
                            });
                        }
                    }
                }
            }
            Rule::fn_decl => {
                let mut name_start = None;
                for p in inner.into_inner() {
                    match p.as_rule() {
                        Rule::free_text_identifier if name_start.is_none() => {
                            name_start = Some(p.as_span().start())
                        }
                        Rule::fn_return_type => out.push(Exp {
                            expr_index: i,
                            kind: K::FunctionSignature,
                            source: ByteRange {
                                start: name_start.unwrap(),
                                end: sub_range(&p).end,
                            },
                            path: vec![K::Function],
                        }),
                        Rule::fn_body => out.push(Exp {
                            expr_index: i,
                            kind: K::FunctionBody,
                            source: sub_range(&p),
                            path: vec![K::Function],
                        }),
                        _ => {}
                    }
                }
            }
            r => panic!("unexpected expr child {r:?}"),
        }
    }
    Some(out)
}

/// `input (. fn)* [. output]` of a `stream`, `stream_variable_declaration` or
/// `input_and_maybe_stream_functions` pair.
fn chain(pair: Pair<Rule>, expr_index: usize, path: &[K], out: &mut Vec<Exp>) {
    let mut seen_decl_name = pair.as_rule() != Rule::stream_variable_declaration;
    for p in pair.into_inner() {
        match p.as_rule() {
            // the first identifier of `name = ...` is the declared name, not an input.
            Rule::identifier if !seen_decl_name => seen_decl_name = true,
            Rule::identifier => out.push(Exp {
                expr_index,
                kind: K::InputVariable,
                source: sub_range(&p),
                path: path.to_vec(),
            }),
            Rule::input_function => {
                let source = sub_range(&p);
                let kind = rule_kind(p.clone().into_inner().next().unwrap().as_rule()).unwrap();
                out.push(Exp {
                    expr_index,
                    kind,
                    source,
                    path: path.to_vec(),
                });
                if kind == K::InputSelectAll {
                    let sel = p.into_inner().next().unwrap();
                    let mut inner_path = path.to_vec();
                    inner_path.push(K::InputSelectAll);
                    for b in sel
                        .into_inner()
                        .filter(|b| b.as_rule() == Rule::input_and_maybe_stream_functions)
                    {
                        out.push(Exp {
                            expr_index,
                            kind: K::SelectAllBranch,
                            source: sub_range(&b),
                            path: inner_path.clone(),
                        });
                        let mut branch_path = inner_path.clone();
                        branch_path.push(K::SelectAllBranch);
                        chain(b, expr_index, &branch_path, out);
                    }
                }
            }
            Rule::stream_function => {
                let kind = rule_kind(p.clone().into_inner().next().unwrap().as_rule()).unwrap();
                out.push(Exp {
                    expr_index,
                    kind,
                    source: sub_range(&p),
                    path: path.to_vec(),
                });
            }
            Rule::output_function => {
                let rule = p.clone().into_inner().next().unwrap().as_rule();
                if let Some(kind) = rule_kind(rule) {
                    // generated as two regions around the stream: setup before it, sink after it.
                    for _ in 0..2 {
                        out.push(Exp {
                            expr_index,
                            kind,
                            source: sub_range(&p),
                            path: path.to_vec(),
                        });
                    }
                }
            }
            _ => {}
        }
    }
}
