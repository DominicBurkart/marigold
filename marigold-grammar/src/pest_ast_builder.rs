#![forbid(unsafe_code)]

use crate::nodes::*;
use pest::iterators::{Pair, Pairs};

use crate::parser::Rule;

/// Advance a `Pairs` iterator by one, returning an error with `msg` if exhausted.
fn next_pair<'a>(pairs: &mut Pairs<'a, Rule>, msg: &str) -> Result<Pair<'a, Rule>, String> {
    pairs.next().ok_or_else(|| msg.to_string())
}

/// AST builder for converting Pest parse trees to Marigold AST nodes
pub struct PestAstBuilder;

impl PestAstBuilder {
    /// Build a complete program from the top-level program rule
    pub fn build_program(pairs: Pairs<Rule>) -> Result<Vec<TypedExpression>, String> {
        let mut expressions = Vec::new();

        for pair in pairs {
            match pair.as_rule() {
                Rule::program => {
                    // Process each expression within the program
                    for inner in pair.into_inner() {
                        match inner.as_rule() {
                            Rule::expr => {
                                let expr = Self::build_expression(inner)?;
                                expressions.push(expr);
                            }
                            Rule::EOI => {} // End of input, ignore
                            _ => {
                                return Err(format!(
                                    "Unexpected rule in program: {:?}",
                                    inner.as_rule()
                                ))
                            }
                        }
                    }
                }
                _ => return Err(format!("Expected program rule, got: {:?}", pair.as_rule())),
            }
        }

        Ok(expressions)
    }

    /// Dispatch expression building to specific handlers
    fn build_expression(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty expression".to_string())?;

        match inner.as_rule() {
            Rule::stream => Self::build_stream(inner),
            Rule::stream_variable_declaration => Self::build_stream_variable(inner),
            Rule::struct_decl => Self::build_struct_decl(inner),
            Rule::enum_decl => Self::build_enum_decl(inner),
            Rule::fn_decl => Self::build_fn_decl(inner),
            _ => Err(format!("Unknown expression type: {:?}", inner.as_rule())),
        }
    }

    /// Build a stream from the stream rule
    fn build_stream(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut pairs = pair.into_inner();

        let first = next_pair(&mut pairs, "Empty stream")?;

        let (input_fn, stream_type) = match first.as_rule() {
            Rule::input_function => {
                let (code, stream_type) = Self::build_input_function(first)?;
                (code, stream_type)
            }
            Rule::identifier => {
                // Stream variable reference
                let name = first.as_str().to_string();
                let code = format!("marigold_stream_var_{name}");
                (code, StreamType::StreamVariable(name))
            }
            _ => {
                return Err(format!(
                    "Expected input function or identifier, got: {:?}",
                    first.as_rule()
                ))
            }
        };

        let mut stream_code = input_fn;
        let mut current_stream_type = stream_type;
        let mut stream_fns: Vec<(StreamFunctionKind, String)> = Vec::new();

        // Process stream functions and output function
        while let Some(next) = pairs.next() {
            match next.as_rule() {
                Rule::stream_function => {
                    let (kind, fn_code) = Self::build_stream_function(next)?;
                    stream_fns.push((kind.clone(), fn_code.clone()));
                    stream_code = format!("{stream_code}.{fn_code}");
                    current_stream_type = Self::update_stream_type(current_stream_type, &kind);
                }
                Rule::output_function => {
                    let output = Self::build_output_function(next, &stream_code, &current_stream_type)?;
                    return Ok(TypedExpression::Stream {
                        code: output,
                        stream_functions: stream_fns,
                    });
                }
                _ => {
                    return Err(format!(
                        "Expected stream function or output function, got: {:?}",
                        next.as_rule()
                    ))
                }
            }
        }

        Err("Stream missing output function".to_string())
    }

    fn update_stream_type(current: StreamType, kind: &StreamFunctionKind) -> StreamType {
        match kind {
            StreamFunctionKind::Filter
            | StreamFunctionKind::TakeWhile
            | StreamFunctionKind::Ok
            | StreamFunctionKind::OkOrPanic
            | StreamFunctionKind::KeepFirstN(_)
            | StreamFunctionKind::Fold => StreamType::Regular,
            StreamFunctionKind::FilterMap | StreamFunctionKind::Map => StreamType::Regular,
            StreamFunctionKind::Permutations(k) => StreamType::Array(*k),
            StreamFunctionKind::PermutationsWithReplacement(k) => StreamType::Array(*k),
            StreamFunctionKind::Combinations(k) => StreamType::Array(*k),
        }
        .or_keep_variable_name(current)
    }

    /// Build a stream variable declaration
    fn build_stream_variable(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut pairs = pair.into_inner();

        let name_pair = next_pair(&mut pairs, "Missing variable name")?;
        let name = name_pair.as_str().to_string();

        let source_pair = next_pair(&mut pairs, "Missing variable source")?;

        let (input_code, stream_type) = match source_pair.as_rule() {
            Rule::input_function => Self::build_input_function(source_pair)?,
            Rule::identifier => {
                let source_name = source_pair.as_str().to_string();
                (
                    format!("marigold_stream_var_{source_name}"),
                    StreamType::StreamVariable(source_name),
                )
            }
            _ => {
                return Err(format!(
                    "Expected input function or identifier in stream variable, got: {:?}",
                    source_pair.as_rule()
                ))
            }
        };

        let mut current_code = input_code;
        let mut current_stream_type = stream_type;
        let mut stream_fns: Vec<(StreamFunctionKind, String)> = Vec::new();

        for next in pairs {
            match next.as_rule() {
                Rule::stream_function => {
                    let (kind, fn_code) = Self::build_stream_function(next)?;
                    stream_fns.push((kind.clone(), fn_code.clone()));
                    current_code = format!("{current_code}.{fn_code}");
                    current_stream_type = Self::update_stream_type(current_stream_type, &kind);
                }
                _ => {
                    return Err(format!(
                        "Expected stream function in variable declaration, got: {:?}",
                        next.as_rule()
                    ))
                }
            }
        }

        Ok(TypedExpression::StreamVariable {
            name,
            code: current_code,
            stream_type: current_stream_type,
            stream_functions: stream_fns,
        })
    }

    /// Build an input function and return the code and stream type
    fn build_input_function(pair: Pair<Rule>) -> Result<(String, StreamType), String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty input function".to_string())?;

        match inner.as_rule() {
            Rule::range_input => Self::build_range_input(inner),
            Rule::read_file_csv_input => Self::build_read_file_csv_input(inner),
            Rule::select_all_input => Self::build_select_all_input(inner),
            _ => Err(format!(
                "Unknown input function: {:?}",
                inner.as_rule()
            )),
        }
    }

    /// Build a range input function
    fn build_range_input(pair: Pair<Rule>) -> Result<(String, StreamType), String> {
        let mut pairs = pair.into_inner();
        let first = next_pair(&mut pairs, "Empty range input")?;

        match first.as_rule() {
            Rule::free_text_literal => {
                let start = first.as_str();
                let maybe_inclusive = pairs
                    .next()
                    .ok_or_else(|| "Missing range end".to_string())?;

                if maybe_inclusive.as_rule() == Rule::inclusive_marker {
                    // Inclusive range: range(start, =end)
                    let end_pair = next_pair(&mut pairs, "Missing inclusive range end")?;
                    let end = end_pair.as_str();
                    Ok((
                        format!("::marigold::marigold_impl::futures::stream::iter({start}..={end})"),
                        StreamType::Range {
                            start: start.parse::<i64>().ok(),
                            end: end.parse::<i64>().ok(),
                            inclusive: true,
                        },
                    ))
                } else {
                    // Exclusive range: range(start, end)
                    let end = maybe_inclusive.as_str();
                    Ok((
                        format!("::marigold::marigold_impl::futures::stream::iter({start}..{end})"),
                        StreamType::Range {
                            start: start.parse::<i64>().ok(),
                            end: end.parse::<i64>().ok(),
                            inclusive: false,
                        },
                    ))
                }
            }
            Rule::free_text_identifier => {
                // Enum range: range(EnumName)
                let enum_name = first.as_str();
                Ok((
                    format!("::marigold::marigold_impl::futures::stream::iter({enum_name}::iter())"),
                    StreamType::EnumRange(enum_name.to_string()),
                ))
            }
            _ => Err(format!("Unexpected range input rule: {:?}", first.as_rule())),
        }
    }

    /// Build a read_file CSV input function
    fn build_read_file_csv_input(pair: Pair<Rule>) -> Result<(String, StreamType), String> {
        let mut pairs = pair.into_inner();

        let path = next_pair(&mut pairs, "Missing file path")?.as_str();
        let struct_type = next_pair(&mut pairs, "Missing struct type")?.as_str();

        // Check for optional infer_compression parameter
        let infer_compression = pairs
            .next()
            .map(|p| p.as_str() == "true")
            .unwrap_or(false);

        // Strip quotes from path
        let path = &path[1..path.len() - 1];

        // Auto-detect gzip based on file extension if infer_compression is true
        let is_gzip = infer_compression && path.ends_with(".gz");

        let code = if is_gzip {
            format!(
                "::marigold::read_csv_gz::<{struct_type}>(\"{}\")",
                path
            )
        } else {
            format!(
                "::marigold::read_csv::<{struct_type}>(\"{}\")",
                path
            )
        };

        Ok((code, StreamType::Regular))
    }

    /// Build a select_all input function
    fn build_select_all_input(pair: Pair<Rule>) -> Result<(String, StreamType), String> {
        let mut stream_codes = Vec::new();
        let mut total_count: Option<i64> = Some(0);

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::input_and_maybe_stream_functions => {
                    let (code, stream_type) = Self::build_input_and_maybe_stream_functions(inner)?;
                    stream_codes.push(code);
                    // Track total element count for cardinality
                    if let Some(count) = total_count {
                        if let Some(n) = stream_type.exact_count() {
                            total_count = Some(count + n);
                        } else {
                            total_count = None;
                        }
                    }
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in select_all: {:?}",
                        inner.as_rule()
                    ))
                }
            }
        }

        let n = stream_codes.len();
        let code = if n == 1 {
            stream_codes.into_iter().next().unwrap()
        } else if n == 2 {
            let mut iter = stream_codes.into_iter();
            let a = iter.next().unwrap();
            let b = iter.next().unwrap();
            format!("::marigold::marigold_impl::futures::stream::select({a}, {b})")
        } else {
            let joined = stream_codes.join(",\n    ");
            format!("::marigold::marigold_impl::futures::stream::select_all(vec![\n    {joined}\n])")
        };

        let stream_type = match total_count {
            Some(n) => StreamType::SelectAll(n),
            None => StreamType::Regular,
        };

        Ok((code, stream_type))
    }

    /// Build an input function possibly followed by stream functions (for select_all)
    fn build_input_and_maybe_stream_functions(
        pair: Pair<Rule>,
    ) -> Result<(String, StreamType), String> {
        let mut pairs = pair.into_inner();

        let input_pair = next_pair(&mut pairs, "Empty input_and_maybe_stream_functions")?;
        let (mut code, mut stream_type) = Self::build_input_function(input_pair)?;

        for next in pairs {
            match next.as_rule() {
                Rule::stream_function => {
                    let (kind, fn_code) = Self::build_stream_function(next)?;
                    code = format!("{code}.{fn_code}");
                    stream_type = Self::update_stream_type(stream_type, &kind);
                }
                _ => {
                    return Err(format!(
                        "Expected stream function, got: {:?}",
                        next.as_rule()
                    ))
                }
            }
        }

        Ok((code, stream_type))
    }

    /// Build a stream function and return (kind, code)
    fn build_stream_function(pair: Pair<Rule>) -> Result<(StreamFunctionKind, String), String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty stream function".to_string())?;

        match inner.as_rule() {
            Rule::map_fn => (StreamFunctionKind::Map, Self::build_map_fn(inner)?),
            Rule::take_while_fn => (
                StreamFunctionKind::TakeWhile,
                Self::build_take_while_fn(inner)?,
            ),
            Rule::filter_fn => (StreamFunctionKind::Filter, Self::build_filter_fn(inner)?),
            Rule::filter_map_fn => (
                StreamFunctionKind::FilterMap,
                Self::build_filter_map_fn(inner)?,
            ),
            Rule::permutations_fn => {
                let n = Self::peek_numeric_arg(&inner)?;
                (
                    StreamFunctionKind::Permutations(n),
                    Self::build_permutations_fn(inner)?,
                )
            }
            Rule::permutations_with_replacement_fn => {
                let n = Self::peek_numeric_arg(&inner)?;
                (
                    StreamFunctionKind::PermutationsWithReplacement(n),
                    Self::build_permutations_with_replacement_fn(inner)?,
                )
            }
            Rule::combinations_fn => {
                let n = Self::peek_numeric_arg(&inner)?;
                (
                    StreamFunctionKind::Combinations(n),
                    Self::build_combinations_fn(inner)?,
                )
            }
            Rule::keep_first_n_fn => {
                let n = Self::peek_numeric_arg(&inner)?;
                (
                    StreamFunctionKind::KeepFirstN(n),
                    Self::build_keep_first_n_fn(inner)?,
                )
            }
            Rule::fold_fn => (StreamFunctionKind::Fold, Self::build_fold_fn(inner)?),
            Rule::ok_fn => (StreamFunctionKind::Ok, "ok()".to_string()),
            Rule::ok_or_panic_fn => (
                StreamFunctionKind::OkOrPanic,
                "ok_or_panic()".to_string(),
            ),
            _ => {
                return Err(format!(
                    "Unknown stream function: {:?}",
                    inner.as_rule()
                ))
            }
        }
        .pipe_ok()
    }

    /// Extract the numeric argument from a function rule (used for permutations/combinations)
    fn peek_numeric_arg(pair: &Pair<Rule>) -> Result<u64, String> {
        pair.clone()
            .into_inner()
            .next()
            .ok_or_else(|| "Missing numeric argument".to_string())?
            .as_str()
            .parse::<u64>()
            .map_err(|e| format!("Invalid numeric argument: {e}"))
    }

    fn build_map_fn(pair: Pair<Rule>) -> Result<String, String> {
        let map_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing map function".to_string())?
            .as_str();

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!(
            "map(|v| ::marigold::marigold_impl::futures::future::ready({map_fn}(v)))"
        ));

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!("map(|v| async move {{ {map_fn}(v) }})"));
    }

    fn build_take_while_fn(pair: Pair<Rule>) -> Result<String, String> {
        // Single codegen path: take_while is inherently sequential (reordering would break
        // prefix semantics), so no buffered/parallel variant is generated under tokio/async-std,
        // unlike build_filter_fn / build_map_fn which split on
        // #[cfg(any(feature = "tokio", feature = "async-std"))] for parallelism.
        let take_while_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing take_while function".to_string())?
            .as_str();

        Ok(format!(
            "take_while(|v| ::marigold::marigold_impl::futures::future::ready({take_while_fn}(v)))"
        ))
    }

    fn build_filter_map_fn(pair: Pair<Rule>) -> Result<String, String> {
        let filter_map_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing filter_map function".to_string())?
            .as_str();

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!(
            "filter_map(|v| ::marigold::marigold_impl::futures::future::ready({filter_map_fn}(v)))"
        ));

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!(
            "filter_map(|v| async move {{ {filter_map_fn}(v) }})"
        ));
    }

    fn build_filter_fn(pair: Pair<Rule>) -> Result<String, String> {
        let filter_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing filter function".to_string())?
            .as_str();

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!(
            "filter(|v| ::marigold::marigold_impl::futures::future::ready({filter_fn}(*v)))"
        ));

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!(
            "map(|v| async move {{ ({filter_fn}(v), v) }}).buffered(::std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).filter_map(|(keep, v)| ::marigold::marigold_impl::futures::future::ready(if keep {{ Some(v) }} else {{ None }}))"
        ));
    }

    fn build_permutations_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations n".to_string())?
            .as_str();

        Ok(format!("collect::<Vec<_>>().await.into_iter().permutations({n}).map(|p| {{ let arr: [_; {n}] = p.try_into().unwrap(); arr }}).pipe(::marigold::marigold_impl::futures::stream::iter)"))
    }

    fn build_permutations_with_replacement_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations_with_replacement n".to_string())?
            .as_str();

        Ok(format!("collect::<Vec<_>>().await.into_iter().permutations_with_replacement({n}).map(|p| {{ let arr: [_; {n}] = p.try_into().unwrap(); arr }}).pipe(::marigold::marigold_impl::futures::stream::iter)"))
    }

    fn build_combinations_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing combinations n".to_string())?
            .as_str();

        Ok(format!("collect::<Vec<_>>().await.into_iter().combinations({n}).map(|p| {{ let arr: [_; {n}] = p.try_into().unwrap(); arr }}).pipe(::marigold::marigold_impl::futures::stream::iter)"))
    }

    fn build_keep_first_n_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut pairs = pair.into_inner();
        let n = next_pair(&mut pairs, "Missing keep_first_n n")?.as_str();
        let comparator = next_pair(&mut pairs, "Missing keep_first_n comparator")?.as_str();

        Ok(format!("collect::<Vec<_>>().await.into_iter().sorted_by({comparator}).take({n}).pipe(::marigold::marigold_impl::futures::stream::iter)"))
    }

    fn build_fold_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut pairs = pair.into_inner();
        let init = next_pair(&mut pairs, "Missing fold init")?.as_str();
        let fold_fn = next_pair(&mut pairs, "Missing fold function")?.as_str();

        Ok(format!("fold({init}, |acc, v| ::marigold::marigold_impl::futures::future::ready({fold_fn}(acc, v))).into_stream()"))
    }

    fn build_output_function(
        pair: Pair<Rule>,
        stream_code: &str,
        stream_type: &StreamType,
    ) -> Result<String, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty output function".to_string())?;

        match inner.as_rule() {
            Rule::return_fn => Ok(Self::build_return_fn(stream_code, stream_type)),
            Rule::write_file_fn => Self::build_write_file_fn(inner, stream_code),
            _ => Err(format!("Unknown output function: {:?}", inner.as_rule())),
        }
    }

    fn build_return_fn(stream_code: &str, stream_type: &StreamType) -> String {
        match stream_type {
            StreamType::StreamVariable(name) => {
                format!("Box::pin(marigold_stream_var_{name})")
            }
            _ => format!("Box::pin({stream_code})"),
        }
    }

    fn build_write_file_fn(pair: Pair<Rule>, stream_code: &str) -> Result<String, String> {
        let mut pairs = pair.into_inner();

        let path = next_pair(&mut pairs, "Missing write_file path")?.as_str();
        let format_pair = next_pair(&mut pairs, "Missing write_file format")?;
        let format = format_pair.as_str();

        // Check for optional compression
        let compression = pairs.next().and_then(|p| {
            p.into_inner()
                .next()
                .map(|c| c.as_str().to_string())
        });

        // Strip quotes from path
        let path = &path[1..path.len() - 1];

        let is_gzip = match compression.as_deref() {
            Some("gz") => true,
            Some("none") => false,
            None => path.ends_with(".gz"),
            Some(other) => return Err(format!("Unknown compression type: {other}")),
        };

        match format {
            "csv" => {
                let write_fn = if is_gzip {
                    "write_csv_gz"
                } else {
                    "write_csv"
                };
                Ok(format!(
                    "{{ ::marigold::{write_fn}({stream_code}, \"{path}\").await }}"
                ))
            }
            _ => Err(format!("Unsupported write_file format: {format}")),
        }
    }

    fn build_struct_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut pairs = pair.into_inner();

        let name = next_pair(&mut pairs, "Missing struct name")?.as_str().to_string();
        let body_pair = next_pair(&mut pairs, "Missing struct body")?;

        let mut fields = Vec::new();
        for field_pair in body_pair.into_inner() {
            match field_pair.as_rule() {
                Rule::struct_field_list => {
                    for inner in field_pair.into_inner() {
                        if inner.as_rule() == Rule::struct_field {
                            let field = Self::build_struct_field(inner)?;
                            fields.push(field);
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(TypedExpression::Struct { name, fields })
    }

    fn build_struct_field(pair: Pair<Rule>) -> Result<StructField, String> {
        let mut pairs = pair.into_inner();

        let name = next_pair(&mut pairs, "Missing field name")?.as_str().to_string();
        let field_type_pair = next_pair(&mut pairs, "Missing field type")?;
        let field_type = Self::build_field_type(field_type_pair)?;

        Ok(StructField { name, field_type })
    }

    fn build_field_type(pair: Pair<Rule>) -> Result<FieldType, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty field type".to_string())?;

        match inner.as_rule() {
            Rule::optional_type => {
                let inner_type_pair = inner
                    .into_inner()
                    .next()
                    .ok_or_else(|| "Empty optional type".to_string())?;
                let inner_type = Self::build_field_type(inner_type_pair)?;
                Ok(FieldType::Optional(Box::new(inner_type)))
            }
            Rule::bounded_int_type => {
                let text = inner.as_str();
                Ok(FieldType::BoundedInt(text.to_string()))
            }
            Rule::bounded_uint_type => {
                let text = inner.as_str();
                Ok(FieldType::BoundedUint(text.to_string()))
            }
            Rule::string_type => {
                let text = inner.as_str();
                // Parse the N from string_N
                let n: usize = text[7..].parse().map_err(|e| format!("Invalid string type: {e}"))?;
                Ok(FieldType::String(n))
            }
            Rule::primitive_type => {
                let text = inner.as_str();
                Ok(FieldType::Primitive(text.to_string()))
            }
            Rule::type_expr => {
                let text = inner.as_str();
                Ok(FieldType::Custom(text.to_string()))
            }
            _ => Err(format!("Unknown field type: {:?}", inner.as_rule())),
        }
    }

    fn build_enum_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut pairs = pair.into_inner();

        let name = next_pair(&mut pairs, "Missing enum name")?.as_str().to_string();
        let body_pair = next_pair(&mut pairs, "Missing enum body")?;

        let mut variants = Vec::new();
        let mut default_variant = None;

        for inner in body_pair.into_inner() {
            match inner.as_rule() {
                Rule::enum_variant_list => {
                    for variant_pair in inner.into_inner() {
                        if variant_pair.as_rule() == Rule::enum_variant {
                            let variant = Self::build_enum_variant(variant_pair)?;
                            variants.push(variant);
                        }
                    }
                }
                Rule::default_variant => {
                    default_variant = Some(Self::build_default_variant(inner)?);
                }
                _ => {}
            }
        }

        Ok(TypedExpression::Enum {
            name,
            variants,
            default_variant,
        })
    }

    fn build_enum_variant(pair: Pair<Rule>) -> Result<EnumVariant, String> {
        let mut pairs = pair.into_inner();

        let name = next_pair(&mut pairs, "Missing variant name")?.as_str().to_string();
        let value = pairs.next().map(|p| {
            // Skip the '=' sign, get the quoted string
            p.into_inner()
                .next()
                .map(|s| {
                    let raw = s.as_str();
                    // Strip quotes
                    raw[1..raw.len() - 1].to_string()
                })
        }).flatten();

        Ok(EnumVariant { name, value })
    }

    fn build_default_variant(pair: Pair<Rule>) -> Result<DefaultVariant, String> {
        let mut pairs = pair.into_inner();

        let name = next_pair(&mut pairs, "Missing default variant name")?.as_str().to_string();

        let rest = pairs.next();
        let variant_type = match rest {
            Some(p) if p.as_rule() == Rule::default_variant_type => {
                let type_str = p
                    .into_inner()
                    .next()
                    .map(|t| t.as_str().to_string())
                    .unwrap_or_default();
                DefaultVariantType::Typed(type_str)
            }
            Some(p) if p.as_rule() == Rule::enum_value => {
                let value_str = p
                    .into_inner()
                    .next()
                    .map(|s| {
                        let raw = s.as_str();
                        raw[1..raw.len() - 1].to_string()
                    })
                    .unwrap_or_default();
                DefaultVariantType::Value(value_str)
            }
            _ => DefaultVariantType::Unit,
        };

        Ok(DefaultVariant { name, variant_type })
    }

    fn build_fn_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        // The entire fn declaration is captured as source text
        let source = pair.as_str().to_string();
        Ok(TypedExpression::Function { source })
    }
}

trait PipeOk<T, E> {
    fn pipe_ok(self) -> Result<T, E>;
}

impl<T, E> PipeOk<T, E> for (T, String)
where
    E: From<String>,
{
    fn pipe_ok(self) -> Result<T, E> {
        Ok(self.0)
    }
}

impl<T, E> PipeOk<(T, String), E> for Result<(T, String), E> {
    fn pipe_ok(self) -> Result<(T, String), E> {
        self
    }
}
