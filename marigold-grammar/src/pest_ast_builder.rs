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

    fn build_expression(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty expression".to_string())?;

        match inner.as_rule() {
            Rule::unnamed_stream_expr => {
                let stream = Self::build_unnamed_stream(inner)?;
                Ok(stream.into())
            }
            Rule::named_stream_expr => {
                let stream = Self::build_named_stream(inner)?;
                Ok(stream.into())
            }
            Rule::stream_variable_expr => {
                let node = Self::build_stream_variable(inner)?;
                Ok(node.into())
            }
            Rule::stream_variable_from_prior_expr => {
                let node = Self::build_stream_variable_from_prior(inner)?;
                Ok(node.into())
            }
            Rule::struct_declaration => {
                let node = Self::build_struct_declaration(inner)?;
                Ok(TypedExpression::StructDeclaration(node))
            }
            Rule::enum_declaration => {
                let name = Self::get_identifier_str(&inner)?;
                let body = Self::get_enum_body_str(&inner)?;
                Ok(crate::nodes::parse_enum(name, body))
            }
            Rule::fn_declaration => {
                let node = Self::build_fn_declaration(inner)?;
                Ok(TypedExpression::FnDeclaration(node))
            }
            _ => Err(format!("Unknown expression type: {:?}", inner.as_rule())),
        }
    }

    fn build_unnamed_stream(pair: Pair<Rule>) -> Result<UnnamedStreamNode, String> {
        let mut inner = pair.into_inner();
        let inp_and_funs_pair = inner
            .next()
            .ok_or_else(|| "Missing input_and_maybe_stream_functions".to_string())?;
        let inp_and_funs = Self::build_input_and_maybe_stream_functions(inp_and_funs_pair)?;
        let out_pair = inner
            .next()
            .ok_or_else(|| "Missing output function".to_string())?;
        let out = Self::build_output_function(out_pair)?;
        Ok(UnnamedStreamNode { inp_and_funs, out })
    }

    fn build_named_stream(pair: Pair<Rule>) -> Result<NamedStreamNode, String> {
        let mut inner = pair.into_inner();
        let name_pair = inner
            .next()
            .ok_or_else(|| "Missing stream variable name".to_string())?;
        let stream_variable = name_pair.as_str().to_string();

        let mut funs = Vec::new();
        let mut out = None;

        for p in inner {
            match p.as_rule() {
                Rule::stream_function => {
                    funs.push(Self::build_stream_function(p)?);
                }
                Rule::output_function => {
                    out = Some(Self::build_output_function(p)?);
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in named stream: {:?}",
                        p.as_rule()
                    ))
                }
            }
        }

        let out = out.ok_or_else(|| "Missing output function in named stream".to_string())?;
        Ok(NamedStreamNode {
            stream_variable,
            funs,
            out,
        })
    }

    fn build_stream_variable(pair: Pair<Rule>) -> Result<StreamVariableNode, String> {
        let mut inner = pair.into_inner();
        let name_pair = inner
            .next()
            .ok_or_else(|| "Missing stream variable name".to_string())?;
        let variable_name = name_pair.as_str().to_string();

        let inp_pair = inner
            .next()
            .ok_or_else(|| "Missing input function".to_string())?;
        let inp = Self::build_input_function(inp_pair)?;

        let mut funs = Vec::new();
        for p in inner {
            if p.as_rule() == Rule::stream_function {
                funs.push(Self::build_stream_function(p)?);
            }
        }

        Ok(StreamVariableNode {
            variable_name,
            inp,
            funs,
        })
    }

    fn build_stream_variable_from_prior(
        pair: Pair<Rule>,
    ) -> Result<StreamVariableFromPriorStreamVariableNode, String> {
        let mut inner = pair.into_inner();
        let name_pair = inner
            .next()
            .ok_or_else(|| "Missing stream variable name".to_string())?;
        let variable_name = name_pair.as_str().to_string();

        let prior_pair = inner
            .next()
            .ok_or_else(|| "Missing prior stream variable name".to_string())?;
        let prior_stream_variable = prior_pair.as_str().to_string();

        let mut funs = Vec::new();
        for p in inner {
            if p.as_rule() == Rule::stream_function {
                funs.push(Self::build_stream_function(p)?);
            }
        }

        Ok(StreamVariableFromPriorStreamVariableNode {
            variable_name,
            prior_stream_variable,
            funs,
        })
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
            if stream_fn_pair.as_rule() == Rule::stream_function {
                funs.push(Self::build_stream_function(stream_fn_pair)?);
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

        let start_str = inner
            .next()
            .ok_or_else(|| "Missing range start".to_string())?
            .as_str()
            .trim()
            .to_string();

        let end_wrapper = inner
            .next()
            .ok_or_else(|| "Missing range end".to_string())?;

        let (end_str, inclusive) = match end_wrapper.as_rule() {
            Rule::inclusive_range_end => {
                let val = end_wrapper
                    .into_inner()
                    .next()
                    .ok_or_else(|| "Missing inclusive range value".to_string())?
                    .as_str()
                    .trim()
                    .to_string();
                (val, true)
            }
            _ => (end_wrapper.as_str().trim().to_string(), false),
        };

        let start: i64 = start_str
            .parse()
            .map_err(|_| format!("Invalid range start: {start_str}"))?;
        let end: i64 = end_str
            .parse()
            .map_err(|_| format!("Invalid range end: {end_str}"))?;

        let count = if inclusive {
            (end - start + 1).max(0) as u64
        } else {
            (end - start).max(0) as u64
        };

        let code = if inclusive {
            format!("::marigold::marigold_impl::futures::stream::iter(({start}..={end}).map(|v| v as _));")
        } else {
            format!("::marigold::marigold_impl::futures::stream::iter(({start}..{end}).map(|v| v as _));")
        };

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
            .map(|p| Self::build_input_and_maybe_stream_functions(p).map(|s| s.code()))
            .collect();
        let streams = streams?;

        let args = streams
            .iter()
            .map(|s| format!("Box::pin({s})"))
            .collect::<Vec<_>>()
            .join(", ");

        let code = format!(
            "::marigold::marigold_impl::futures::stream::select_all(vec![{args}]);"
        );

        Ok(InputFunctionNode {
            variability: InputVariability::Variable,
            input_count: InputCount::Unknown,
            code,
        })
    }

    #[cfg(feature = "io")]
    fn build_read_file_fn(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let mut inner = pair.into_inner();

        let path_str = inner
            .next()
            .ok_or_else(|| "Missing read_file path".to_string())?
            .as_str()
            .trim_matches('"')
            .to_string();

        let format_str = inner
            .next()
            .ok_or_else(|| "Missing read_file format".to_string())?
            .as_str()
            .to_string();

        let mut struct_type = None;
        let mut compression = "none".to_string();

        for option_pair in inner {
            let s = option_pair.as_str();
            if let Some(t) = s.strip_prefix("struct=") {
                struct_type = Some(t.to_string());
            } else if let Some(c) = s.strip_prefix("compression=") {
                compression = c.to_string();
            }
        }

        let struct_name =
            struct_type.ok_or_else(|| "read_file requires struct= parameter".to_string())?;

        let code = match (format_str.as_str(), compression.as_str()) {
            ("csv", "none") => format!(
                "::marigold::marigold_impl::read_csv::<{struct_name}, _>(std::path::PathBuf::from(\"{path_str}\"));"
            ),
            ("csv", "gz") => format!(
                "::marigold::marigold_impl::read_csv_gz::<{struct_name}, _>(std::path::PathBuf::from(\"{path_str}\"));"
            ),
            (fmt, comp) => {
                return Err(format!("Unknown read_file format/compression: {fmt}/{comp}"))
            }
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
                let n = Self::peek_numeric_arg(&inner)?;
                (
                    StreamFunctionKind::KeepFirstN(n),
                    Self::build_keep_first_n_fn(inner)?,
                )
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
        Ok((k, format!("permutations({k})")))
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
        Ok((k, format!("permutations_with_replacement({k})")))
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
        Ok((k, format!("combinations({k})")))
    }

    fn peek_numeric_arg(pair: &Pair<Rule>) -> Result<u64, String> {
        let n_str = pair
            .clone()
            .into_inner()
            .next()
            .ok_or_else(|| "Missing numeric argument".to_string())?
            .as_str()
            .trim()
            .to_string();
        n_str
            .parse()
            .map_err(|_| format!("Invalid numeric argument: {n_str}"))
    }

    fn build_keep_first_n_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut inner = pair.into_inner();
        let n_str = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n n".to_string())?
            .as_str()
            .trim()
            .to_string();
        let sort_fn = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n sort function".to_string())?
            .as_str()
            .trim()
            .to_string();
        let n: u64 = n_str
            .parse()
            .map_err(|_| format!("Invalid keep_first_n n: {n_str}"))?;
        Ok(format!("keep_first_n({n}, {sort_fn})"))
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

    fn build_fold_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut inner = pair.into_inner();

        let initial_value = inner
            .next()
            .ok_or_else(|| "Missing fold initial value".to_string())?
            .as_str()
            .trim()
            .to_string();

        let fun = inner
            .next()
            .ok_or_else(|| "Missing fold function".to_string())?
            .as_str()
            .trim()
            .to_string();

        let number_or_constructor = if initial_value
            .chars()
            .next()
            .map(|c| c.is_alphabetic() || c == '_')
            .unwrap_or(false)
        {
            format!("{initial_value}()")
        } else {
            initial_value.clone()
        };

        Ok(format!("marifold({number_or_constructor}, |acc, x| futures::future::ready({fun}(acc, x))).await"))
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
                let mut inner_pairs = inner.into_inner();

                let path_str = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing write_file path".to_string())?
                    .as_str()
                    .trim_matches('"')
                    .to_string();

                let format_str = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing write_file format".to_string())?
                    .as_str()
                    .to_string();

                let mut compression = "none".to_string();
                for option_pair in inner_pairs {
                    let s = option_pair.as_str();
                    if let Some(c) = s.strip_prefix("compression=") {
                        compression = c.to_string();
                    }
                }

                let (stream_prefix, stream_postfix) = match (format_str.as_str(), compression.as_str()) {
                    ("csv", "none") => (
                        "::marigold::marigold_impl::write_csv(Box::pin(".to_string(),
                        format!("), \"{path_str}\").await;"),
                    ),
                    ("csv", "gz") => (
                        "::marigold::marigold_impl::write_csv_gz(Box::pin(".to_string(),
                        format!("), \"{path_str}\").await;"),
                    ),
                    (fmt, comp) => {
                        return Err(format!(
                            "Unknown write_file format/compression: {fmt}/{comp}"
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

    fn build_struct_declaration(pair: Pair<Rule>) -> Result<StructDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name = inner
            .next()
            .ok_or_else(|| "Missing struct name".to_string())?
            .as_str()
            .to_string();

        let mut fields = Vec::new();
        for field_pair in inner {
            if field_pair.as_rule() == Rule::struct_field {
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
        }

        Ok(StructDeclarationNode { name, fields })
    }

    fn build_field_type(pair: Pair<Rule>) -> Result<crate::nodes::Type, String> {
        use std::str::FromStr;
        match pair.as_rule() {
            Rule::bounded_int_type => {
                let mut inner = pair.into_inner();
                let min = Self::build_bound_expr(
                    inner.next().ok_or_else(|| "Missing bounded int min".to_string())?,
                )?;
                let max = Self::build_bound_expr(
                    inner.next().ok_or_else(|| "Missing bounded int max".to_string())?,
                )?;
                Ok(crate::nodes::Type::BoundedInt { min, max })
            }
            Rule::bounded_uint_type => {
                let mut inner = pair.into_inner();
                let min = Self::build_bound_expr(
                    inner.next().ok_or_else(|| "Missing bounded uint min".to_string())?,
                )?;
                let max = Self::build_bound_expr(
                    inner.next().ok_or_else(|| "Missing bounded uint max".to_string())?,
                )?;
                Ok(crate::nodes::Type::BoundedUint { min, max })
            }
            _ => {
                let type_str = pair.as_str();
                crate::nodes::Type::from_str(type_str)
                    .map_err(|_| format!("Unknown field type: {type_str}"))
            }
        }
    }

    fn build_bound_expr(pair: Pair<Rule>) -> Result<crate::nodes::BoundExpr, String> {
        crate::bound_expr::build_bound_expr(pair)
    }

    fn get_identifier_str(pair: &Pair<Rule>) -> Result<String, String> {
        pair.clone()
            .into_inner()
            .find(|p| p.as_rule() == Rule::identifier)
            .map(|p| p.as_str().to_string())
            .ok_or_else(|| format!("Missing identifier in {:?}", pair.as_rule()))
    }

    fn get_enum_body_str(pair: &Pair<Rule>) -> Result<String, String> {
        pair.clone()
            .into_inner()
            .find(|p| p.as_rule() == Rule::enum_body)
            .map(|p| p.as_str().to_string())
            .ok_or_else(|| "Missing enum body".to_string())
    }

    fn build_fn_declaration(pair: Pair<Rule>) -> Result<FnDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name = inner
            .next()
            .ok_or_else(|| "Missing fn name".to_string())?
            .as_str()
            .to_string();

        let mut parameters = Vec::new();
        let mut output_type = String::new();
        let mut body = String::new();

        for p in inner {
            match p.as_rule() {
                Rule::fn_param => {
                    let mut param_inner = p.into_inner();
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
                    output_type = p
                        .into_inner()
                        .next()
                        .ok_or_else(|| "Missing return type".to_string())?
                        .as_str()
                        .to_string();
                }
                Rule::fn_body => {
                    body = p.as_str().to_string();
                }
                _ => {
                    return Err(format!(
                        "Unexpected rule in fn declaration: {:?}",
                        p.as_rule()
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
