#![forbid(unsafe_code)]

use crate::nodes::*;
use pest::iterators::{Pair, Pairs};

use crate::parser::Rule;

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
            Rule::stream_expr => Ok(Self::build_stream_expr(inner)?.into()),
            Rule::struct_declaration => Ok(Self::build_struct_declaration(inner)?.into()),
            Rule::enum_declaration => Ok(crate::nodes::parse_enum(
                Self::get_identifier(&inner)?,
                Self::get_enum_body(&inner)?,
            )),
            Rule::fn_declaration => Ok(Self::build_fn_declaration(inner)?.into()),
            _ => Err(format!("Unknown expression type: {:?}", inner.as_rule())),
        }
    }

    fn build_stream_expr(pair: Pair<Rule>) -> Result<impl Into<TypedExpression>, String> {
        let mut inner = pair.into_inner();
        let first = inner
            .next()
            .ok_or_else(|| "Empty stream expression".to_string())?;

        match first.as_rule() {
            Rule::input_and_maybe_stream_functions => {
                // unnamed stream: input.fns.output
                let inp_and_funs = Self::build_input_and_maybe_stream_functions(first)?;
                let out_pair = inner
                    .next()
                    .ok_or_else(|| "Missing output function".to_string())?;
                let out = Self::build_output_function(out_pair)?;
                Ok(UnnamedStreamNode { inp_and_funs, out })
            }
            Rule::identifier => {
                // named stream variable: var_name.fns?.output
                let stream_variable = first.as_str().to_string();
                let mut funs = Vec::new();
                let mut out = None;

                for pair in inner {
                    match pair.as_rule() {
                        Rule::stream_function => {
                            funs.push(Self::build_stream_function(pair)?);
                        }
                        Rule::output_function => {
                            out = Some(Self::build_output_function(pair)?);
                        }
                        _ => {
                            return Err(format!(
                                "Unexpected rule in named stream: {:?}",
                                pair.as_rule()
                            ))
                        }
                    }
                }

                let out = out.ok_or_else(|| "Missing output function".to_string())?;
                Ok(NamedStreamNode {
                    stream_variable,
                    funs,
                    out,
                })
            }
            _ => Err(format!(
                "Unexpected rule in stream_expr: {:?}",
                first.as_rule()
            )),
        }
    }

    pub fn build_input_and_maybe_stream_functions(
        pair: Pair<Rule>,
    ) -> Result<InputAndMaybeStreamFunctions, String> {
        let mut inner = pair.into_inner();

        let inp_pair = inner
            .next()
            .ok_or_else(|| "Missing input function".to_string())?;
        let inp = Self::build_input_function(inp_pair)?;

        let mut funs = Vec::new();
        for stream_fn_pair in inner {
            match stream_fn_pair.as_rule() {
                Rule::stream_function => {
                    funs.push(Self::build_stream_function(stream_fn_pair)?);
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in input_and_maybe_stream_functions: {:?}",
                        stream_fn_pair.as_rule()
                    ))
                }
            }
        }

        Ok(InputAndMaybeStreamFunctions { inp, funs })
    }

    fn build_input_function(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty input function".to_string())?;

        match inner.as_rule() {
            Rule::range_fn => Self::build_range_fn(inner),
            Rule::select_all_fn => Self::build_select_all_fn(inner),
            #[cfg(feature = "io")]
            Rule::read_file_fn => Self::build_read_file_fn(inner),
            _ => Err(format!("Unknown input function: {:?}", inner.as_rule())),
        }
    }

    fn build_range_fn(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let mut inner = pair.into_inner();

        let start_pair = inner
            .next()
            .ok_or_else(|| "Missing range start".to_string())?;
        let start_str = start_pair.as_str().trim();

        let end_pair_wrapper = inner
            .next()
            .ok_or_else(|| "Missing range end".to_string())?;

        let (end_str, inclusive) = match end_pair_wrapper.as_rule() {
            Rule::inclusive_range_end => {
                let end_inner = end_pair_wrapper
                    .into_inner()
                    .next()
                    .ok_or_else(|| "Missing inclusive range value".to_string())?;
                (end_inner.as_str().trim().to_string(), true)
            }
            _ => (end_pair_wrapper.as_str().trim().to_string(), false),
        };

        let start: i64 = start_str
            .parse()
            .map_err(|_| format!("Invalid range start: {start_str}"))?;
        let end: i64 = end_str
            .parse()
            .map_err(|_| format!("Invalid range end: {end_str}"))?;

        let count = if inclusive {
            if end >= start {
                (end - start + 1) as u64
            } else {
                0u64
            }
        } else if end > start {
            (end - start) as u64
        } else {
            0u64
        };

        let code = if inclusive {
            format!("::marigold::marigold_impl::futures::stream::iter(({start}..={end})")
        } else {
            format!("::marigold::marigold_impl::futures::stream::iter(({start}..{end})")
        };
        let code = format!("{code}.map(|v| v as _));");

        Ok(InputFunctionNode {
            variability: InputVariability::Constant,
            input_count: InputCount::Known(num_bigint::BigUint::from(count)),
            code,
        })
    }

    fn build_select_all_fn(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let streams: Result<Vec<_>, _> = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::input_and_maybe_stream_functions)
            .map(|p| {
                let stream = Self::build_input_and_maybe_stream_functions(p)?;
                Ok(stream.code())
            })
            .collect();
        let streams = streams?;

        let code = format!(
            "::marigold::marigold_impl::futures::stream::select_all(vec![{}]);",
            streams
                .iter()
                .map(|s| format!("Box::pin({s})"))
                .collect::<Vec<_>>()
                .join(", ")
        );

        Ok(InputFunctionNode {
            variability: InputVariability::Variable,
            input_count: InputCount::Unknown,
            code,
        })
    }

    #[cfg(feature = "io")]
    fn build_read_file_fn(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        use std::str::FromStr;

        let mut inner = pair.into_inner();

        let path = inner
            .next()
            .ok_or_else(|| "Missing read_file path".to_string())?;
        let path_str = path.as_str().trim_matches('"');

        let format_pair = inner
            .next()
            .ok_or_else(|| "Missing read_file format".to_string())?;
        let format_str = format_pair.as_str();

        // Parse optional struct type
        let mut struct_type = None;
        let mut compression = "none".to_string();

        for option_pair in inner {
            let option_str = option_pair.as_str();
            if option_str.starts_with("struct=") {
                struct_type = Some(option_str.trim_start_matches("struct=").to_string());
            } else if option_str.starts_with("compression=") {
                compression = option_str
                    .trim_start_matches("compression=")
                    .to_string();
            }
        }

        let struct_name =
            struct_type.ok_or_else(|| "read_file requires struct= parameter".to_string())?;

        let code = match format_str {
            "csv" => match compression.as_str() {
                "none" => format!(
                    "::marigold::marigold_impl::read_csv::<{struct_name}, _>(std::path::PathBuf::from(\"{path_str}\"));"
                ),
                "gz" => format!(
                    "::marigold::marigold_impl::read_csv_gz::<{struct_name}, _>(std::path::PathBuf::from(\"{path_str}\"));"
                ),
                other => return Err(format!("Unknown compression format: {other}")),
            },
            other => return Err(format!("Unknown read_file format: {other}")),
        };

        Ok(InputFunctionNode {
            variability: InputVariability::Variable,
            input_count: InputCount::Unknown,
            code,
        })
    }

    fn build_stream_function(pair: Pair<Rule>) -> Result<StreamFunctionNode, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty stream function".to_string())?;

        let (kind, code) = match inner.as_rule() {
            Rule::map_fn => (StreamFunctionKind::Map, Self::build_map_fn(inner)?),
            Rule::filter_fn => (StreamFunctionKind::Filter, Self::build_filter_fn(inner)?),
            Rule::filter_map_fn => {
                (StreamFunctionKind::FilterMap, Self::build_filter_map_fn(inner)?)
            }
            Rule::permutations_fn => {
                let (n, code) = Self::build_permutations_fn(inner)?;
                (StreamFunctionKind::Permutations(n), code)
            }
            Rule::permutations_with_replacement_fn => {
                let (n, code) = Self::build_permutations_with_replacement_fn(inner)?;
                (StreamFunctionKind::PermutationsWithReplacement(n), code)
            }
            Rule::combinations_fn => {
                let (n, code) = Self::build_combinations_fn(inner)?;
                (StreamFunctionKind::Combinations(n), code)
            }
            Rule::keep_first_n_fn => {
                let (n, code) = Self::build_keep_first_n_fn(inner)?;
                (
                    StreamFunctionKind::KeepFirstN(n),
                    Self::build_keep_first_n_fn(inner.clone())?.1,
                );
                (StreamFunctionKind::KeepFirstN(n), code)
            }
            Rule::chain_fn => Self::build_chain_fn(inner)?,
            Rule::fold_fn => (StreamFunctionKind::Fold, Self::build_fold_fn(inner)?),
            Rule::ok_fn => (
                StreamFunctionKind::Ok,
                "filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap())".to_string(),
            ),
            Rule::ok_or_panic_fn => (
                StreamFunctionKind::OkOrPanic,
                "map(|r| r.unwrap())".to_string(),
            ),
            _ => return Err(format!("Unknown stream function: {:?}", inner.as_rule())),
        };

        Ok(StreamFunctionNode { kind, code })
    }

    fn build_map_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mapping_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing map function".to_string())?
            .as_str();

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!("map(|v| async move {{{mapping_fn}(v)}}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2))"));

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!("map({mapping_fn})"));
    }

    fn build_filter_fn(pair: Pair<Rule>) -> Result<String, String> {
        let filter_fn = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing filter function".to_string())?
            .as_str();
        Ok(format!("filter(|v| futures::future::ready({filter_fn}(*v)))"))
    }

    fn build_filter_map_fn(pair: Pair<Rule>) -> Result<String, String> {
        let fn_name = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing filter_map function".to_string())?
            .as_str();

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!("filter_map(|v| async move {{ {fn_name}(v) }})"));

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!("filter_map(|v| futures::future::ready({fn_name}(v)))"));
    }

    fn build_permutations_fn(pair: Pair<Rule>) -> Result<(u64, String), String> {
        let k_str = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations k".to_string())?
            .as_str()
            .trim();
        let k: u64 = k_str
            .parse()
            .map_err(|_| format!("Invalid permutations k: {k_str}"))?;
        let code = format!("permutations({k})");
        Ok((k, code))
    }

    fn build_permutations_with_replacement_fn(pair: Pair<Rule>) -> Result<(u64, String), String> {
        let k_str = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations_with_replacement k".to_string())?
            .as_str()
            .trim();
        let k: u64 = k_str
            .parse()
            .map_err(|_| format!("Invalid permutations_with_replacement k: {k_str}"))?;
        let code = format!("permutations_with_replacement({k})");
        Ok((k, code))
    }

    fn build_combinations_fn(pair: Pair<Rule>) -> Result<(u64, String), String> {
        let k_str = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing combinations k".to_string())?
            .as_str()
            .trim();
        let k: u64 = k_str
            .parse()
            .map_err(|_| format!("Invalid combinations k: {k_str}"))?;
        let code = format!("combinations({k})");
        Ok((k, code))
    }

    fn build_keep_first_n_fn(pair: Pair<Rule>) -> Result<(u64, String), String> {
        let mut inner = pair.into_inner();
        let n_str = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n n".to_string())?
            .as_str()
            .trim();
        let sort_fn = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n sort function".to_string())?
            .as_str()
            .trim();
        let n: u64 = n_str
            .parse()
            .map_err(|_| format!("Invalid keep_first_n n: {n_str}"))?;
        let code = format!("keep_first_n({n}, {sort_fn})");
        Ok((n, code))
    }

    fn build_chain_fn(pair: Pair<Rule>) -> Result<(StreamFunctionKind, String), String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing chain stream argument".to_string())?;

        if inner.as_rule() != Rule::input_and_maybe_stream_functions {
            return Err(format!(
                "Expected input_and_maybe_stream_functions in chain, got: {:?}",
                inner.as_rule()
            ));
        }

        let stream = Self::build_input_and_maybe_stream_functions(inner)?;
        let code = format!("chain({})", stream.code());

        Ok((StreamFunctionKind::Chain(Box::new(stream)), code))
    }

    /// Build output function node
    fn build_output_function(pair: Pair<Rule>) -> Result<OutputFunctionNode, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty output function".to_string())?;

        match inner.as_rule() {
            Rule::return_fn => Ok(OutputFunctionNode {
                stream_prefix: "".to_string(),
                stream_postfix: "".to_string(),
                returning: true,
            }),
            Rule::write_file_fn => {
                // Parse write_file(path, format[, compression=none|gz])
                let mut inner_pairs = inner.into_inner();

                let path_pair = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing write_file path".to_string())?;
                let path_str = path_pair.as_str().trim_matches('"');

                let format_pair = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing write_file format".to_string())?;
                let format_str = format_pair.as_str();

                let mut compression = "none".to_string();
                for option_pair in inner_pairs {
                    let option_str = option_pair.as_str();
                    if option_str.starts_with("compression=") {
                        compression = option_str
                            .trim_start_matches("compression=")
                            .to_string();
                    }
                }

                let (stream_prefix, stream_postfix) = match (format_str, compression.as_str()) {
                    ("csv", "none") => (
                        format!("::marigold::marigold_impl::write_csv(Box::pin("),
                        format!("), \"{path_str}\").await;"),
                    ),
                    ("csv", "gz") => (
                        format!("::marigold::marigold_impl::write_csv_gz(Box::pin("),
                        format!("), \"{path_str}\").await;"),
                    ),
                    (fmt, comp) => {
                        return Err(format!(
                            "Unknown write_file format/compression combination: {fmt}/{comp}"
                        ))
                    }
                };

                Ok(OutputFunctionNode {
                    stream_prefix,
                    stream_postfix,
                    returning: false,
                })
            }
            _ => Err(format!(
                "Unknown output function: {:?}",
                inner.as_rule()
            )),
        }
    }

    fn build_fold_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut inner = pair.into_inner();

        let initial_value = inner
            .next()
            .ok_or_else(|| "Missing fold initial value".to_string())?
            .as_str()
            .trim()
            .to_string();

        let fun_pair = inner
            .next()
            .ok_or_else(|| "Missing fold function".to_string())?;
        let fun = fun_pair.as_str().trim();

        let number_or_constructor = if initial_value
            .chars()
            .next()
            .map(|c| c.is_alphabetic() || c == '_')
            .unwrap_or(false)
        {
            // It's a constructor or variable name, call it as a function
            format!("{initial_value}()")
        } else {
            initial_value.clone()
        };

        Ok(format!("marifold({number_or_constructor}, |acc, x| futures::future::ready({fun}(acc, x))).await"))
    }

    fn build_struct_declaration(pair: Pair<Rule>) -> Result<StructDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = inner
            .next()
            .ok_or_else(|| "Missing struct name".to_string())?;
        let name = name_pair.as_str().to_string();

        let mut fields = Vec::new();
        for field_pair in inner {
            match field_pair.as_rule() {
                Rule::struct_field => {
                    let mut field_inner = field_pair.into_inner();
                    let field_name = field_inner
                        .next()
                        .ok_or_else(|| "Missing field name".to_string())?
                        .as_str()
                        .to_string();
                    let field_type_pair = field_inner
                        .next()
                        .ok_or_else(|| "Missing field type".to_string())?;
                    let field_type = Self::build_field_type(field_type_pair)?;
                    fields.push((field_name, field_type));
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in struct: {:?}",
                        field_pair.as_rule()
                    ))
                }
            }
        }

        Ok(StructDeclarationNode { name, fields })
    }

    fn build_field_type(pair: Pair<Rule>) -> Result<crate::nodes::Type, String> {
        use std::str::FromStr;
        match pair.as_rule() {
            Rule::bounded_int_type => {
                let mut inner = pair.into_inner();
                let min_pair = inner
                    .next()
                    .ok_or_else(|| "Missing bounded int min".to_string())?;
                let max_pair = inner
                    .next()
                    .ok_or_else(|| "Missing bounded int max".to_string())?;
                let min = Self::build_bound_expr(min_pair)?;
                let max = Self::build_bound_expr(max_pair)?;
                Ok(crate::nodes::Type::BoundedInt { min, max })
            }
            Rule::bounded_uint_type => {
                let mut inner = pair.into_inner();
                let min_pair = inner
                    .next()
                    .ok_or_else(|| "Missing bounded uint min".to_string())?;
                let max_pair = inner
                    .next()
                    .ok_or_else(|| "Missing bounded uint max".to_string())?;
                let min = Self::build_bound_expr(min_pair)?;
                let max = Self::build_bound_expr(max_pair)?;
                Ok(crate::nodes::Type::BoundedUint { min, max })
            }
            Rule::field_type => {
                let type_str = pair.as_str();
                crate::nodes::Type::from_str(type_str)
                    .map_err(|_| format!("Unknown field type: {type_str}"))
            }
            _ => {
                let type_str = pair.as_str();
                crate::nodes::Type::from_str(type_str)
                    .map_err(|_| format!("Unknown field type rule: {type_str}"))
            }
        }
    }

    fn build_bound_expr(
        pair: Pair<Rule>,
    ) -> Result<crate::nodes::BoundExpr, String> {
        crate::bound_expr::build_bound_expr(pair)
    }

    fn get_identifier(pair: &Pair<Rule>) -> Result<String, String> {
        pair.clone()
            .into_inner()
            .find(|p| p.as_rule() == Rule::identifier)
            .map(|p| p.as_str().to_string())
            .ok_or_else(|| format!("Missing identifier in {:?}", pair.as_rule()))
    }

    fn get_enum_body(pair: &Pair<Rule>) -> Result<String, String> {
        pair.clone()
            .into_inner()
            .find(|p| p.as_rule() == Rule::enum_body)
            .map(|p| p.as_str().to_string())
            .ok_or_else(|| "Missing enum body".to_string())
    }

    fn build_fn_declaration(pair: Pair<Rule>) -> Result<FnDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = inner
            .next()
            .ok_or_else(|| "Missing fn name".to_string())?;
        let name = name_pair.as_str().to_string();

        let mut parameters = Vec::new();
        let mut output_type = String::new();
        let mut body = String::new();

        for pair in inner {
            match pair.as_rule() {
                Rule::fn_param => {
                    let mut param_inner = pair.into_inner();
                    let param_name = param_inner
                        .next()
                        .ok_or_else(|| "Missing param name".to_string())?
                        .as_str()
                        .to_string();
                    let param_type = param_inner
                        .next()
                        .ok_or_else(|| "Missing param type".to_string())?
                        .as_str()
                        .to_string();
                    parameters.push((param_name, param_type));
                }
                Rule::fn_return_type => {
                    output_type = pair
                        .into_inner()
                        .next()
                        .ok_or_else(|| "Missing return type".to_string())?
                        .as_str()
                        .to_string();
                }
                Rule::fn_body => {
                    body = pair.as_str().to_string();
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in fn declaration: {:?}",
                        pair.as_rule()
                    ))
                }
            }
        }

        Ok(FnDeclarationNode {
            name,
            parameters,
            output_type,
            body,
        })
    }
}
