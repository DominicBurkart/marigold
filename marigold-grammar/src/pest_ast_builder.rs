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

    /// Build a TypedExpression from an expr rule
    fn build_expression(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();
        let first = next_pair(&mut inner, "expression has no children")?;

        match first.as_rule() {
            Rule::fn_declaration => Self::build_fn_declaration(first).map(Into::into),
            Rule::struct_declaration => Self::build_struct_declaration(first).map(Into::into),
            Rule::enum_declaration => Self::build_enum_declaration(first).map(Into::into),
            Rule::stream_variable_assignment => {
                Self::build_stream_variable(first).map(Into::into)
            }
            Rule::stream_variable_from_prior_stream_variable_assignment => {
                Self::build_stream_variable_from_prior(first).map(Into::into)
            }
            Rule::named_stream => Self::build_named_stream(first).map(Into::into),
            Rule::unnamed_stream => Self::build_unnamed_stream(first).map(Into::into),
            other => Err(format!("Unknown expression rule: {:?}", other)),
        }
    }

    /// Build a FnDeclarationNode from a fn_declaration rule
    fn build_fn_declaration(pair: Pair<Rule>) -> Result<FnDeclarationNode, String> {
        let full_text = pair.as_str().to_string();
        let mut inner = pair.into_inner();

        let name_pair = next_pair(&mut inner, "fn_declaration missing fn_name")?;
        let name = name_pair.as_str().to_string();

        // Collect parameters
        let mut parameters = Vec::new();
        let mut output_type = String::new();
        let mut body = String::new();

        for p in inner {
            match p.as_rule() {
                Rule::fn_param => {
                    let mut param_inner = p.into_inner();
                    let param_name = next_pair(&mut param_inner, "fn_param missing name")?
                        .as_str()
                        .to_string();
                    let param_type = next_pair(&mut param_inner, "fn_param missing type")?
                        .as_str()
                        .to_string();
                    parameters.push((param_name, param_type));
                }
                Rule::fn_return_type => {
                    output_type = p.as_str().trim().to_string();
                }
                Rule::fn_body => {
                    body = p.as_str().to_string();
                }
                _ => {} // Skip whitespace/other tokens
            }
        }

        // If we couldn't parse structured params, fall back to raw text extraction
        if output_type.is_empty() {
            // Extract return type from full text using simple pattern matching
            output_type = extract_return_type_from_fn(&full_text);
            body = extract_body_from_fn(&full_text);
        }

        Ok(FnDeclarationNode {
            name,
            parameters,
            output_type,
            body,
        })
    }

    /// Build a StructDeclarationNode from a struct_declaration rule
    fn build_struct_declaration(pair: Pair<Rule>) -> Result<StructDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = next_pair(&mut inner, "struct_declaration missing struct_name")?;
        let name = name_pair.as_str().to_string();

        let mut fields = Vec::new();
        for field_pair in inner {
            if field_pair.as_rule() == Rule::struct_field {
                let mut field_inner = field_pair.into_inner();
                let field_name = next_pair(&mut field_inner, "struct_field missing name")?
                    .as_str()
                    .to_string();
                let field_type_pair =
                    next_pair(&mut field_inner, "struct_field missing type")?.as_str();
                let field_type = parse_type(field_type_pair)?;
                fields.push((field_name, field_type));
            }
        }

        Ok(StructDeclarationNode { name, fields })
    }

    /// Build an EnumDeclarationNode from an enum_declaration rule
    fn build_enum_declaration(pair: Pair<Rule>) -> Result<EnumDeclarationNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = next_pair(&mut inner, "enum_declaration missing enum_name")?;
        let name = name_pair.as_str().to_string();

        let mut variants = Vec::new();
        let mut default_variant = None;

        for variant_pair in inner {
            match variant_pair.as_rule() {
                Rule::enum_variant => {
                    let mut variant_inner = variant_pair.into_inner();
                    let variant_name =
                        next_pair(&mut variant_inner, "enum_variant missing name")?
                            .as_str()
                            .to_string();
                    let definition = variant_inner.next().map(|p| p.as_str().to_string());
                    variants.push((variant_name, definition));
                }
                Rule::default_enum_variant => {
                    let mut dev_inner = variant_pair.into_inner();
                    let dev_name =
                        next_pair(&mut dev_inner, "default_enum_variant missing name")?
                            .as_str()
                            .to_string();
                    // Check if the next token is a size or a value
                    if let Some(next_tok) = dev_inner.next() {
                        match next_tok.as_rule() {
                            Rule::number => {
                                let size: u32 = next_tok
                                    .as_str()
                                    .parse()
                                    .map_err(|e| format!("Invalid size: {e}"))?;
                                default_variant = Some(DefaultEnumVariant::Sized(dev_name, size));
                            }
                            _ => {
                                default_variant = Some(DefaultEnumVariant::WithDefaultValue(
                                    dev_name,
                                    next_tok.as_str().to_string(),
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(EnumDeclarationNode {
            name,
            variants,
            default_variant,
        })
    }

    /// Build a StreamVariableNode from a stream_variable_assignment rule
    fn build_stream_variable(pair: Pair<Rule>) -> Result<StreamVariableNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = next_pair(&mut inner, "stream_variable missing variable_name")?;
        let variable_name = name_pair.as_str().to_string();

        let stream_pair =
            next_pair(&mut inner, "stream_variable missing unnamed_stream_without_output")?;
        let (inp, funs) = Self::build_input_and_funs(stream_pair)?;

        Ok(StreamVariableNode {
            variable_name,
            inp,
            funs,
        })
    }

    /// Build a StreamVariableFromPriorStreamVariableNode
    fn build_stream_variable_from_prior(
        pair: Pair<Rule>,
    ) -> Result<StreamVariableFromPriorStreamVariableNode, String> {
        let mut inner = pair.into_inner();

        let name_pair = next_pair(
            &mut inner,
            "stream_variable_from_prior missing variable_name",
        )?;
        let variable_name = name_pair.as_str().to_string();

        let prior_pair = next_pair(
            &mut inner,
            "stream_variable_from_prior missing prior_stream_variable",
        )?;
        let prior_stream_variable = prior_pair.as_str().to_string();

        let mut funs = Vec::new();
        for p in inner {
            if matches!(
                p.as_rule(),
                Rule::map_fun
                    | Rule::filter_fun
                    | Rule::filter_map_fun
                    | Rule::permutations_fun
                    | Rule::permutations_with_replacement_fun
                    | Rule::combinations_fun
                    | Rule::keep_first_n_fun
                    | Rule::fold_fun
                    | Rule::ok_fun
                    | Rule::ok_or_panic_fun
            ) {
                funs.push(Self::build_stream_function(p)?);
            }
        }

        Ok(StreamVariableFromPriorStreamVariableNode {
            variable_name,
            prior_stream_variable,
            funs,
        })
    }

    /// Build a NamedStreamNode from a named_stream rule
    fn build_named_stream(pair: Pair<Rule>) -> Result<NamedStreamNode, String> {
        let mut inner = pair.into_inner();

        let var_pair = next_pair(&mut inner, "named_stream missing stream_variable")?;
        let stream_variable = var_pair.as_str().to_string();

        let mut funs = Vec::new();
        let mut out = None;

        for p in inner {
            match p.as_rule() {
                Rule::map_fun
                | Rule::filter_fun
                | Rule::filter_map_fun
                | Rule::permutations_fun
                | Rule::permutations_with_replacement_fun
                | Rule::combinations_fun
                | Rule::keep_first_n_fun
                | Rule::fold_fun
                | Rule::ok_fun
                | Rule::ok_or_panic_fun => {
                    funs.push(Self::build_stream_function(p)?);
                }
                Rule::output_fun => {
                    out = Some(Self::build_output_function(p)?);
                }
                _ => {}
            }
        }

        let out = out.ok_or_else(|| "named_stream missing output_fun".to_string())?;
        Ok(NamedStreamNode {
            stream_variable,
            funs,
            out,
        })
    }

    /// Build an UnnamedStreamNode from an unnamed_stream rule
    fn build_unnamed_stream(pair: Pair<Rule>) -> Result<UnnamedStreamNode, String> {
        let mut inner = pair.into_inner();

        // First child is always the input + functions without output
        let stream_pair =
            next_pair(&mut inner, "unnamed_stream missing unnamed_stream_without_output")?;
        let (inp, funs) = Self::build_input_and_funs(stream_pair)?;

        // Last child is the output function
        let out_pair = next_pair(&mut inner, "unnamed_stream missing output_fun")?;
        let out = Self::build_output_function(out_pair)?;

        Ok(UnnamedStreamNode {
            inp_and_funs: InputAndMaybeStreamFunctions { inp, funs },
            out,
        })
    }

    /// Build (InputFunctionNode, Vec<StreamFunctionNode>) from an unnamed_stream_without_output rule
    fn build_input_and_funs(
        pair: Pair<Rule>,
    ) -> Result<(InputFunctionNode, Vec<StreamFunctionNode>), String> {
        let mut inner = pair.into_inner();

        let inp_pair = next_pair(&mut inner, "unnamed_stream_without_output missing input_fun")?;
        let inp = Self::build_input_function(inp_pair)?;

        let mut funs = Vec::new();
        for p in inner {
            match p.as_rule() {
                Rule::map_fun
                | Rule::filter_fun
                | Rule::filter_map_fun
                | Rule::permutations_fun
                | Rule::permutations_with_replacement_fun
                | Rule::combinations_fun
                | Rule::keep_first_n_fun
                | Rule::fold_fun
                | Rule::ok_fun
                | Rule::ok_or_panic_fun => {
                    funs.push(Self::build_stream_function(p)?);
                }
                _ => {}
            }
        }

        Ok((inp, funs))
    }

    /// Build an InputFunctionNode from an input_fun rule
    fn build_input_function(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        use crate::nodes::{InputCount, InputVariability};

        let mut inner = pair.into_inner();
        let first = next_pair(&mut inner, "input_fun has no children")?;

        match first.as_rule() {
            Rule::range_fun => Self::build_range_fun(first),
            Rule::select_all_fun => Self::build_select_all_fun(first),
            other => Err(format!("Unknown input_fun rule: {:?}", other)),
        }
    }

    fn build_range_fun(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        use crate::nodes::{InputCount, InputVariability};

        let raw = pair.as_str();
        let mut inner = pair.into_inner();

        // Check for enum range: range(EnumName)
        if let Some(first) = inner.next() {
            match first.as_rule() {
                Rule::identifier => {
                    // Enum range: range(EnumName)
                    let enum_name = first.as_str().to_string();
                    let code = format!(
                        "::marigold::marigold_impl::futures::stream::iter({enum_name}::__marigold_variants())"
                    );
                    return Ok(InputFunctionNode {
                        variability: InputVariability::Constant,
                        input_count: InputCount::Enum(enum_name),
                        code,
                    });
                }
                Rule::number => {
                    // Numeric range: range(start, end) or range(start, =end)
                    let start_str = first.as_str();
                    let start: i64 = start_str
                        .parse()
                        .map_err(|e| format!("Invalid range start {start_str}: {e}"))?;

                    // Next should be end
                    if let Some(end_pair) = inner.next() {
                        let end_str = end_pair.as_str().trim_start_matches('=');
                        let inclusive = end_pair.as_rule() == Rule::inclusive_range_end
                            || end_pair.as_str().starts_with('=');
                        let end: i64 = end_str
                            .parse()
                            .map_err(|e| format!("Invalid range end {end_str}: {e}"))?;

                        let (count, code) = if inclusive {
                            let count = (end - start + 1).max(0) as u64;
                            let code =
                                format!("::marigold::marigold_impl::futures::stream::iter({start}i32..={end}i32)");
                            (count, code)
                        } else {
                            let count = (end - start).max(0) as u64;
                            let code =
                                format!("::marigold::marigold_impl::futures::stream::iter({start}i32..{end}i32)");
                            (count, code)
                        };

                        return Ok(InputFunctionNode {
                            variability: InputVariability::Constant,
                            input_count: InputCount::Known(
                                num_bigint::BigUint::from(count),
                            ),
                            code,
                        });
                    }
                }
                _ => {}
            }
        }

        // Fallback: attempt to extract from raw text
        Err(format!("Could not parse range_fun from: {raw}"))
    }

    fn build_select_all_fun(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        use crate::nodes::{InputCount, InputVariability};
        use num_traits::Zero;

        let mut stream_codes = Vec::new();
        let mut total_count: num_bigint::BigUint = num_bigint::BigUint::zero();
        let mut all_known = true;

        for inner_stream in pair.into_inner() {
            if inner_stream.as_rule() == Rule::unnamed_stream_without_output {
                let (inp, funs) = Self::build_input_and_funs(inner_stream)?;

                // Accumulate count
                match &inp.input_count {
                    InputCount::Known(n) => total_count += n,
                    _ => all_known = false,
                }

                // Build stream code
                let inp_code = &inp.code;
                let funs_code = funs
                    .iter()
                    .map(|f| f.code.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                let stream_code = if funs_code.is_empty() {
                    format!("{{use ::marigold::marigold_impl::*; {inp_code}}}")
                } else {
                    format!("{{use ::marigold::marigold_impl::*; {inp_code}.{funs_code}}}")
                };
                stream_codes.push(stream_code);
            }
        }

        let input_count = if all_known {
            InputCount::Known(total_count)
        } else {
            InputCount::Unknown
        };

        let streams_joined = stream_codes.join(", ");
        let code = format!(
            "::marigold::marigold_impl::futures::stream::select_all(vec![{streams_joined}])"
        );

        Ok(InputFunctionNode {
            variability: InputVariability::Variable,
            input_count,
            code,
        })
    }

    /// Build a StreamFunctionNode from a stream function rule
    fn build_stream_function(pair: Pair<Rule>) -> Result<StreamFunctionNode, String> {
        let rule = pair.as_rule();
        match rule {
            Rule::map_fun => {
                let fn_name = pair
                    .into_inner()
                    .next()
                    .ok_or("map_fun missing fn_name")?
                    .as_str()
                    .to_string();
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::Map,
                    code: format!("map(|v| {fn_name}(v))"),
                })
            }
            Rule::filter_fun => {
                let fn_name = pair
                    .into_inner()
                    .next()
                    .ok_or("filter_fun missing fn_name")?
                    .as_str()
                    .to_string();
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::Filter,
                    code: format!("filter(|v| {fn_name}(v))"),
                })
            }
            Rule::filter_map_fun => {
                let fn_name = pair
                    .into_inner()
                    .next()
                    .ok_or("filter_map_fun missing fn_name")?
                    .as_str()
                    .to_string();
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::FilterMap,
                    code: format!("filter_map(|v| {fn_name}(v))"),
                })
            }
            Rule::permutations_fun => {
                let k_str = pair
                    .into_inner()
                    .next()
                    .ok_or("permutations_fun missing k")?
                    .as_str();
                let k: u64 = k_str
                    .parse()
                    .map_err(|e| format!("Invalid permutations k: {e}"))?;
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::Permutations(k),
                    code: format!("permutations({k}).await"),
                })
            }
            Rule::permutations_with_replacement_fun => {
                let k_str = pair
                    .into_inner()
                    .next()
                    .ok_or("permutations_with_replacement_fun missing k")?
                    .as_str();
                let k: u64 = k_str
                    .parse()
                    .map_err(|e| format!("Invalid permutations_with_replacement k: {e}"))?;
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::PermutationsWithReplacement(k),
                    code: format!("permutations_with_replacement({k}).await"),
                })
            }
            Rule::combinations_fun => {
                let k_str = pair
                    .into_inner()
                    .next()
                    .ok_or("combinations_fun missing k")?
                    .as_str();
                let k: u64 = k_str
                    .parse()
                    .map_err(|e| format!("Invalid combinations k: {e}"))?;
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::Combinations(k),
                    code: format!("combinations({k}).await"),
                })
            }
            Rule::keep_first_n_fun => {
                let mut kfn_inner = pair.into_inner();
                let k_str = kfn_inner
                    .next()
                    .ok_or("keep_first_n_fun missing k")?
                    .as_str();
                let fn_name = kfn_inner
                    .next()
                    .ok_or("keep_first_n_fun missing fn_name")?
                    .as_str()
                    .to_string();
                let k: u64 = k_str
                    .parse()
                    .map_err(|e| format!("Invalid keep_first_n k: {e}"))?;
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::KeepFirstN(k),
                    code: format!("keep_first_n({k}, {fn_name}).await"),
                })
            }
            Rule::fold_fun => {
                let mut fold_inner = pair.into_inner();
                let init_str = fold_inner
                    .next()
                    .ok_or("fold_fun missing init")?
                    .as_str()
                    .to_string();
                let fn_name = fold_inner
                    .next()
                    .ok_or("fold_fun missing fn_name")?
                    .as_str()
                    .to_string();
                Ok(StreamFunctionNode {
                    kind: StreamFunctionKind::Fold,
                    code: format!(
                        "collect_and_apply(|v| futures::stream::once(async move {{ v.into_iter().fold({init_str}, {fn_name}) }})  ).await.flatten()"
                    ),
                })
            }
            Rule::ok_fun => Ok(StreamFunctionNode {
                kind: StreamFunctionKind::Ok,
                code: "map(Ok)".to_string(),
            }),
            Rule::ok_or_panic_fun => Ok(StreamFunctionNode {
                kind: StreamFunctionKind::OkOrPanic,
                code: "map(|r| r.unwrap())".to_string(),
            }),
            other => Err(format!("Unknown stream function rule: {:?}", other)),
        }
    }

    /// Build an OutputFunctionNode from an output_fun rule
    fn build_output_function(pair: Pair<Rule>) -> Result<OutputFunctionNode, String> {
        let mut inner = pair.into_inner();
        let first = next_pair(&mut inner, "output_fun has no children")?;

        match first.as_rule() {
            Rule::return_fun => Ok(OutputFunctionNode {
                stream_prefix: String::new(),
                stream_postfix: String::new(),
                returning: true,
            }),
            Rule::write_file_fun => {
                let mut wf_inner = first.into_inner();
                let path = next_pair(&mut wf_inner, "write_file missing path")?
                    .as_str()
                    .to_string();
                let format = next_pair(&mut wf_inner, "write_file missing format")?
                    .as_str()
                    .to_string();
                let stream_prefix = String::new();
                let stream_postfix = format!(
                    ".write_file(\"{path}\", {format})",
                );
                Ok(OutputFunctionNode {
                    stream_prefix,
                    stream_postfix,
                    returning: false,
                })
            }
            other => Err(format!("Unknown output_fun rule: {:?}", other)),
        }
    }
}

/// Extract the return type from a raw fn declaration string.
/// e.g. `fn foo(x: i32) -> i32 { x }` → `i32`
fn extract_return_type_from_fn(fn_text: &str) -> String {
    if let Some(arrow_pos) = fn_text.find("->") {
        let after_arrow = fn_text[arrow_pos + 2..].trim();
        // Return type ends at the opening brace
        if let Some(brace_pos) = after_arrow.find('{') {
            return after_arrow[..brace_pos].trim().to_string();
        }
        return after_arrow.to_string();
    }
    String::new()
}

/// Extract the body from a raw fn declaration string (content between first `{` and last `}`).
fn extract_body_from_fn(fn_text: &str) -> String {
    if let Some(open) = fn_text.find('{') {
        if let Some(close) = fn_text.rfind('}') {
            return fn_text[open + 1..close].to_string();
        }
    }
    String::new()
}

/// Parse a type string into a `Type` enum variant.
fn parse_type(s: &str) -> Result<Type, String> {
    match s.trim() {
        "u8" => Ok(Type::U8),
        "u16" => Ok(Type::U16),
        "u32" => Ok(Type::U32),
        "u64" => Ok(Type::U64),
        "u128" => Ok(Type::U128),
        "usize" => Ok(Type::USize),
        "i8" => Ok(Type::I8),
        "i16" => Ok(Type::I16),
        "i32" => Ok(Type::I32),
        "i64" => Ok(Type::I64),
        "i128" => Ok(Type::I128),
        "isize" => Ok(Type::ISize),
        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        "bool" => Ok(Type::Bool),
        "char" => Ok(Type::Char),
        other => {
            // Check for str(N) pattern
            if other.starts_with("str(") && other.ends_with(')') {
                let n_str = &other[4..other.len() - 1];
                let n: u32 = n_str
                    .parse()
                    .map_err(|e| format!("Invalid str size {n_str}: {e}"))?;
                return Ok(Type::Str(n));
            }
            // Check for Option<...> pattern
            if other.starts_with("Option<") && other.ends_with('>') {
                let inner = &other[7..other.len() - 1];
                let inner_type = parse_type(inner)?;
                return Ok(Type::Option(Box::new(inner_type)));
            }
            // Treat as custom type
            arrayvec::ArrayString::try_from(other)
                .map(Type::Custom)
                .map_err(|e| format!("Custom type name too long: {e}"))
        }
    }
}

/// Tokenize a Rust type string that may contain nested angle brackets, e.g.
/// `&[[i32; 3]; 3]`, `Option<Vec<i32>>`, `HashMap<String, i32>`.
///
/// Returns the top-level tokens split on commas at nesting depth 0.
#[allow(dead_code)]
fn tokenize_type_params(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut depth: i32 = 0;

    for ch in s.chars() {
        match ch {
            '<' | '[' | '(' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ']' | ')' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                if !current.trim().is_empty() {
                    result.push(current.trim().to_string());
                }
                current = String::new();
            }
            _ => current.push(ch),
        }
    }

    if !current.is_empty() {
        result.push(current.trim().to_string());
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_type_primitives() {
        assert_eq!(parse_type("u8").unwrap(), Type::U8);
        assert_eq!(parse_type("i32").unwrap(), Type::I32);
        assert_eq!(parse_type("bool").unwrap(), Type::Bool);
        assert_eq!(parse_type("f64").unwrap(), Type::F64);
    }

    #[test]
    fn test_parse_type_str() {
        assert_eq!(parse_type("str(10)").unwrap(), Type::Str(10));
    }

    #[test]
    fn test_parse_type_option() {
        assert_eq!(
            parse_type("Option<i32>").unwrap(),
            Type::Option(Box::new(Type::I32))
        );
    }

    #[test]
    fn test_parse_type_custom() {
        let t = parse_type("MyCustomType").unwrap();
        assert!(matches!(t, Type::Custom(_)));
    }

    #[test]
    fn test_extract_return_type() {
        assert_eq!(
            extract_return_type_from_fn("fn foo(x: i32) -> i32 { x }"),
            "i32"
        );
        assert_eq!(
            extract_return_type_from_fn("fn bar() -> Vec<i32> { vec![] }"),
            "Vec<i32>"
        );
    }

    #[test]
    fn test_extract_body() {
        assert_eq!(extract_body_from_fn("fn foo(x: i32) -> i32 { x * 2 }"), " x * 2 ");
    }

    #[test]
    fn test_tokenize_type_params_simple() {
        assert_eq!(
            tokenize_type_params("i32, u64, bool"),
            vec!["i32", "u64", "bool"]
        );
    }

    #[test]
    fn test_tokenize_type_params_nested() {
        // Nested angle brackets should not be split on inner commas
        let result = tokenize_type_params("HashMap<String, i32>, Vec<u8>");
        assert_eq!(result, vec!["HashMap<String, i32>", "Vec<u8>"]);
    }

    #[test]
    fn test_tokenize_type_params_array_ref() {
        // Array ref with semicolons should be treated as a single token
        let result = tokenize_type_params("&[[i32; 3]; 3]");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], "&[[i32; 3]; 3]");
    }

    #[test]
    fn test_build_program_empty() {
        // An empty input produces an empty expression list
        use crate::parser::{MarigoldPestParser, Rule};
        use pest::Parser;
        let pairs = MarigoldPestParser::parse(Rule::program, "").expect("parse failed");
        let exprs = PestAstBuilder::build_program(pairs).expect("build failed");
        assert!(exprs.is_empty());
    }

    #[test]
    fn test_fn_declaration_builds_correctly() {
        use crate::parser::{MarigoldPestParser, Rule};
        use pest::Parser;
        let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 1).return";
        let pairs = MarigoldPestParser::parse(Rule::program, src).expect("parse failed");
        let exprs = PestAstBuilder::build_program(pairs).expect("build failed");
        let fn_decl = exprs
            .iter()
            .find_map(|e| {
                if let TypedExpression::FnDeclaration(f) = e {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("no FnDeclaration found");
        assert_eq!(fn_decl.name, "double");
        assert_eq!(fn_decl.output_type, "i32");
        assert!(!fn_decl.body.is_empty());
    }

    #[test]
    fn test_fn_array_param_type_preserved() {
        // Regression test: array/slice param types must not be stripped.
        // The grammar rule `fn_param_type` must accept arbitrary Rust type syntax.
        use crate::parser::{MarigoldPestParser, Rule};
        use pest::Parser;

        let src =
            "fn take_matrix(m: &[[i32; 3]; 3]) -> i32 { 0 }\nrange(0, 1).return";
        let pairs = MarigoldPestParser::parse(Rule::program, src).expect("parse failed");
        let exprs = PestAstBuilder::build_program(pairs).expect("build failed");

        let fn_decl = exprs
            .iter()
            .find_map(|e| {
                if let TypedExpression::FnDeclaration(f) = e {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("no FnDeclaration");

        // The parameter type must be preserved verbatim.
        let code = fn_decl.code();
        assert!(
            code.contains("m: &[[i32; 3]; 3]"),
            "Array param type must be preserved verbatim, got: {code}"
        );
        assert!(
            code.contains(": &[[i32; 3]; 3]"),
            "Second array param should appear verbatim, got: {code}"
        );
    }

    #[test]
    fn test_fn_simple_types_still_work_after_param_grammar_change() {
        let code = crate::parser::parse_marigold("fn double(x: i32) -> i32 { x * 2 }")
            .expect("Simple fn should still parse after grammar change");
        assert!(code.contains("x: i32"), "param type i32 should be present");
        assert!(code.contains("-> i32"), "return type i32 should be present");
    }
}
