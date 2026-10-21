//! The source side of the nested segments of a [`crate::source_map::SourceMap`].
//!
//! The AST does not remember where its parts came from, and adding span fields to the public
//! node types would break every struct literal of them. Instead the parts of each top-level
//! expression are read off the pest parse tree here, in a small tree that is parallel to the
//! generated side: [`crate::source_map::Emit`] records the layout of the generated code of a
//! node (which region is its input, which is its third stream function, ...) and the two
//! are paired by [`Role`].
//!
//! Nothing in this module may panic, whatever the tree looks like: the pest tree is walked
//! before the AST builder has had its chance to reject the program, and a shape this module
//! does not know produces fewer parts, never an error.

use crate::diagnostics::ByteRange;
use crate::parser::Rule;
use crate::source_map::{Role, SegmentKind};
use pest::iterators::{Pair, Pairs};

/// One part of a top-level expression: where it is in the source and what it is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SubSpan {
    pub(crate) role: Role,
    pub(crate) kind: SegmentKind,
    pub(crate) source: ByteRange,
    pub(crate) name: Option<String>,
    pub(crate) children: Vec<SubSpan>,
}

/// The parts of every top-level expression, in source order. Parallel to
/// `PestParser::expr_ranges` and to the builder's `Vec<TypedExpression>`: element `i` holds
/// the parts of expression `i`.
pub(crate) fn skeletons(pairs: &Pairs<'_, Rule>) -> Vec<Vec<SubSpan>> {
    pairs
        .clone()
        .flatten()
        .filter(|pair| pair.as_rule() == Rule::expr)
        .map(parts_of_expr)
        .collect()
}

/// The kind of segment a grammar alternative of `input_function` or `stream_function` (or
/// the `write_file` alternative of `output_function`) is. `None` for rules that have no
/// generated code of their own, like `return_fn`.
fn kind_of(rule: Rule) -> Option<SegmentKind> {
    Some(match rule {
        Rule::range_input => SegmentKind::InputRange,
        Rule::read_file_csv_input => SegmentKind::InputReadFile,
        Rule::select_all_input => SegmentKind::InputSelectAll,
        Rule::permutations_fn => SegmentKind::StreamPermutations,
        Rule::permutations_with_replacement_fn => SegmentKind::StreamPermutationsWithReplacement,
        Rule::combinations_fn => SegmentKind::StreamCombinations,
        Rule::keep_first_n_fn => SegmentKind::StreamKeepFirstN,
        Rule::filter_fn => SegmentKind::StreamFilter,
        Rule::filter_map_fn => SegmentKind::StreamFilterMap,
        Rule::map_fn => SegmentKind::StreamMap,
        Rule::fold_fn => SegmentKind::StreamFold,
        Rule::ok_fn => SegmentKind::StreamOk,
        Rule::ok_or_panic_fn => SegmentKind::StreamOkOrPanic,
        Rule::write_file_fn => SegmentKind::OutputWriteFile,
        _ => return None,
    })
}

/// The range of `pair` without trailing ASCII whitespace (the whitespace of the grammar:
/// space, tab, `\r`, `\n`). Several rules include the blanks that follow them in their span.
/// `str::trim_end` would also cut a trailing U+00A0, which is legal at the end of a
/// function's return type.
fn trimmed(pair: &Pair<'_, Rule>) -> ByteRange {
    let span = pair.as_span();
    let text = pair.as_str().trim_end_matches([' ', '\t', '\r', '\n']);
    ByteRange {
        start: span.start(),
        end: span.start() + text.len(),
    }
}

fn leaf(role: Role, kind: SegmentKind, source: ByteRange, name: Option<String>) -> SubSpan {
    #[cfg(test)]
    crate::source_map::count_subspan_step();
    SubSpan {
        role,
        kind,
        source,
        name,
        children: Vec::new(),
    }
}

fn parts_of_expr(expr: Pair<'_, Rule>) -> Vec<SubSpan> {
    #[cfg(test)]
    crate::source_map::count_subspan_step();
    let Some(inner) = expr.into_inner().next() else {
        return Vec::new();
    };
    match inner.as_rule() {
        Rule::stream => chain(inner.into_inner()),
        // the first identifier is the declared name, which is not a part
        Rule::stream_variable_declaration => chain(inner.into_inner().skip(1)),
        Rule::struct_decl => struct_fields(inner),
        Rule::enum_decl => enum_variants(inner),
        Rule::fn_decl => fn_parts(inner),
        _ => Vec::new(),
    }
}

/// The input, the stream functions and the output of a stream or a stream variable
/// declaration, given the children of its pair after the declared name.
fn chain<'i>(pairs: impl Iterator<Item = Pair<'i, Rule>>) -> Vec<SubSpan> {
    let mut parts = Vec::new();
    let mut functions = 0;
    for pair in pairs {
        match pair.as_rule() {
            Rule::input_function => parts.extend(input_part(pair)),
            Rule::identifier => parts.push(leaf(
                Role::Input,
                SegmentKind::InputVariable,
                trimmed(&pair),
                None,
            )),
            Rule::stream_function => {
                parts.extend(function_part(pair, functions));
                functions += 1;
            }
            Rule::output_function => {
                // `return` generates no code of its own; `write_file` wraps the stream, so
                // it is two parts with one source.
                if let Some(call) = pair.into_inner().next() {
                    if let Some(kind) = kind_of(call.as_rule()) {
                        let source = trimmed(&call);
                        parts.push(leaf(Role::OutPrefix, kind, source, None));
                        parts.push(leaf(Role::OutPostfix, kind, source, None));
                    }
                }
            }
            _ => {}
        }
    }
    parts
}

fn function_part(pair: Pair<'_, Rule>, index: usize) -> Option<SubSpan> {
    let source = trimmed(&pair);
    let kind = kind_of(pair.into_inner().next()?.as_rule())?;
    Some(leaf(Role::Fn(index), kind, source, None))
}

fn input_part(pair: Pair<'_, Rule>) -> Option<SubSpan> {
    let call = pair.into_inner().next()?;
    let kind = kind_of(call.as_rule())?;
    let source = trimmed(&call);
    let mut part = leaf(Role::Input, kind, source, None);
    if call.as_rule() == Rule::select_all_input {
        part.children = call
            .into_inner()
            .filter(|branch| branch.as_rule() == Rule::input_and_maybe_stream_functions)
            .enumerate()
            .map(|(index, branch)| {
                let source = trimmed(&branch);
                let mut part = leaf(
                    Role::Branch(index),
                    SegmentKind::SelectAllBranch,
                    source,
                    None,
                );
                part.children = chain(branch.into_inner());
                part
            })
            .collect();
    }
    Some(part)
}

fn struct_fields(decl: Pair<'_, Rule>) -> Vec<SubSpan> {
    decl.into_inner()
        .filter(|pair| pair.as_rule() == Rule::struct_body)
        .flat_map(|body| body.into_inner())
        .filter(|pair| pair.as_rule() == Rule::struct_field_list)
        .flat_map(|list| list.into_inner())
        .filter(|pair| pair.as_rule() == Rule::struct_field)
        .enumerate()
        .map(|(index, field)| {
            let name = first_text(&field);
            leaf(
                Role::Field(index),
                SegmentKind::StructField,
                trimmed(&field),
                name,
            )
        })
        .collect()
}

fn enum_variants(decl: Pair<'_, Rule>) -> Vec<SubSpan> {
    let mut variants = Vec::new();
    for body in decl
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::enum_body)
    {
        for part in body.into_inner() {
            match part.as_rule() {
                Rule::enum_variant_list => {
                    for variant in part
                        .into_inner()
                        .filter(|pair| pair.as_rule() == Rule::enum_variant)
                    {
                        let index = variants.len();
                        variants.push(variant_part(index, &variant));
                    }
                }
                // the `default` variant comes after the plain ones
                Rule::default_variant => {
                    let index = variants.len();
                    variants.push(variant_part(index, &part));
                }
                _ => {}
            }
        }
    }
    variants
}

fn variant_part(index: usize, variant: &Pair<'_, Rule>) -> SubSpan {
    leaf(
        Role::Variant(index),
        SegmentKind::EnumVariant,
        trimmed(variant),
        first_text(variant),
    )
}

/// The text of the first child of `pair`: the name of a field, variant or function.
fn first_text(pair: &Pair<'_, Rule>) -> Option<String> {
    pair.clone()
        .into_inner()
        .next()
        .map(|name| name.as_str().to_string())
}

/// The signature (from the name to the end of the return type) and the body of a function.
fn fn_parts(decl: Pair<'_, Rule>) -> Vec<SubSpan> {
    let mut start = None;
    let mut signature_end = None;
    let mut body = None;
    for (position, pair) in decl.into_inner().enumerate() {
        match pair.as_rule() {
            Rule::free_text_identifier if position == 0 => {
                start = Some(pair.as_span().start());
            }
            Rule::fn_return_type => signature_end = Some(trimmed(&pair).end),
            Rule::fn_body => body = Some(trimmed(&pair)),
            _ => {}
        }
    }
    let mut parts = Vec::new();
    if let (Some(start), Some(end)) = (start, signature_end) {
        parts.push(leaf(
            Role::Signature,
            SegmentKind::FunctionSignature,
            ByteRange { start, end },
            None,
        ));
    }
    if let Some(source) = body {
        parts.push(leaf(Role::Body, SegmentKind::FunctionBody, source, None));
    }
    parts
}
