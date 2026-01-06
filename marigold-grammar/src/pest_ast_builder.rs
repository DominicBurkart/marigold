#![forbid(unsafe_code)]

use crate::nodes::*;
use pest::iterators::{Pair, Pairs};
use std::str::FromStr;

#[cfg(feature = "pest-parser")]
use crate::parser::Rule;

/// AST builder for converting Pest parse trees to Marigold AST nodes
#[cfg(feature = "pest-parser")]
pub struct PestAstBuilder;

#[cfg(feature = "pest-parser")]
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
            Rule::stream_variable_declaration => Self::build_stream_variable_declaration(inner),
            Rule::struct_decl => Self::build_struct_decl(inner),
            Rule::enum_decl => Self::build_enum_decl(inner),
            Rule::fn_decl => Self::build_fn_decl(inner),
            rule => unreachable!("expr should only contain expression types, got: {:?}", rule),
        }
    }

    /// Build a stream expression (unnamed or named)
    fn build_stream(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();

        // First element determines if this is unnamed (input_function) or named (identifier)
        let first = inner.next().ok_or_else(|| "Empty stream".to_string())?;

        match first.as_rule() {
            Rule::input_function => {
                // Unnamed stream: input_function.stream_function*.output_function
                let inp = Self::build_input_function(first)?;
                let mut funs = Vec::new();

                // Collect stream functions
                while let Some(next) = inner.peek() {
                    if next.as_rule() == Rule::stream_function {
                        funs.push(Self::build_stream_function(inner.next().unwrap())?);
                    } else if next.as_rule() == Rule::output_function {
                        break;
                    } else {
                        inner.next(); // Skip unknown rules
                    }
                }

                // Get output function (should be last)
                let out = Self::build_output_function(
                    inner
                        .next()
                        .ok_or_else(|| "Missing output function".to_string())?,
                )?;

                Ok(TypedExpression::from(UnnamedStreamNode {
                    inp_and_funs: InputAndMaybeStreamFunctions { inp, funs },
                    out,
                }))
            }
            Rule::identifier => {
                // Named stream: variable_name.stream_function*.output_function
                let stream_variable = first.as_str().to_string();
                let mut funs = Vec::new();

                // Collect stream functions
                while let Some(next) = inner.peek() {
                    if next.as_rule() == Rule::stream_function {
                        funs.push(Self::build_stream_function(inner.next().unwrap())?);
                    } else if next.as_rule() == Rule::output_function {
                        break;
                    } else {
                        inner.next(); // Skip unknown rules
                    }
                }

                // Get output function (should be last)
                let out = Self::build_output_function(
                    inner
                        .next()
                        .ok_or_else(|| "Missing output function".to_string())?,
                )?;

                Ok(TypedExpression::from(NamedStreamNode {
                    stream_variable,
                    funs,
                    out,
                }))
            }
            _ => Err(format!("Unexpected stream start: {:?}", first.as_rule())),
        }
    }

    /// Build stream variable declaration
    fn build_stream_variable_declaration(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();

        // First identifier is the variable name
        let variable_name = inner
            .next()
            .ok_or_else(|| "Missing variable name".to_string())?
            .as_str()
            .to_string();

        // Next should be either input_function or identifier (for prior stream variable)
        let second = inner
            .next()
            .ok_or_else(|| "Missing right-hand side of assignment".to_string())?;

        match second.as_rule() {
            Rule::input_function => {
                // var = input_function.stream_function*
                let inp = Self::build_input_function(second)?;
                let mut funs = Vec::new();

                // Collect stream functions
                for stream_fn in inner {
                    if stream_fn.as_rule() == Rule::stream_function {
                        funs.push(Self::build_stream_function(stream_fn)?);
                    }
                }

                Ok(TypedExpression::from(StreamVariableNode {
                    variable_name,
                    inp,
                    funs,
                }))
            }
            Rule::identifier => {
                // var = other_var.stream_function*
                let prior_stream_variable = second.as_str().to_string();
                let mut funs = Vec::new();

                // Collect stream functions
                for stream_fn in inner {
                    if stream_fn.as_rule() == Rule::stream_function {
                        funs.push(Self::build_stream_function(stream_fn)?);
                    }
                }

                Ok(TypedExpression::from(
                    StreamVariableFromPriorStreamVariableNode {
                        variable_name,
                        prior_stream_variable,
                        funs,
                    },
                ))
            }
            _ => Err(format!(
                "Unexpected assignment source: {:?}",
                second.as_rule()
            )),
        }
    }

    /// Build struct declaration
    fn build_struct_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();

        let struct_name = inner
            .next()
            .ok_or_else(|| "Missing struct name".to_string())?
            .as_str()
            .to_string();

        let braced_content = inner
            .next()
            .ok_or_else(|| "Missing struct body".to_string())?
            .as_str()
            .to_string();

        // Parse fields using regex (matching LALRPOP approach)
        let fields = Self::parse_struct_fields(&braced_content)?;

        Ok(TypedExpression::from(StructDeclarationNode {
            name: struct_name,
            fields,
        }))
    }

    /// Build enum declaration
    fn build_enum_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();

        let enum_name = inner
            .next()
            .ok_or_else(|| "Missing enum name".to_string())?
            .as_str()
            .to_string();

        let braced_content = inner
            .next()
            .ok_or_else(|| "Missing enum body".to_string())?
            .as_str()
            .to_string();

        // Use the existing parse_enum function from nodes.rs
        let expr = crate::nodes::parse_enum(enum_name, braced_content);
        Ok(expr)
    }

    /// Build function declaration
    fn build_fn_decl(pair: Pair<Rule>) -> Result<TypedExpression, String> {
        let mut inner = pair.into_inner();

        let name = inner
            .next()
            .ok_or_else(|| "Missing function name".to_string())?
            .as_str()
            .to_string();

        // Collect parameters
        let mut parameters = Vec::new();
        let mut return_type_parts = Vec::new();
        let mut body = String::new();

        for item in inner {
            match item.as_rule() {
                Rule::fn_param_list => {
                    for param in item.into_inner() {
                        if param.as_rule() == Rule::fn_param {
                            parameters.push(Self::parse_fn_param(param)?);
                        }
                    }
                }
                Rule::fn_return_type => {
                    for part in item.into_inner() {
                        return_type_parts.push(part.as_str().to_string());
                    }
                }
                Rule::fn_body => {
                    body = item.as_str().to_string();
                }
                _ => {}
            }
        }

        let output_type = return_type_parts.join(" ");

        Ok(TypedExpression::from(FnDeclarationNode {
            name,
            parameters,
            output_type,
            body,
        }))
    }

    /// Parse function parameter
    fn parse_fn_param(pair: Pair<Rule>) -> Result<(String, String), String> {
        let mut parts = pair.into_inner();
        let param_name = parts
            .next()
            .ok_or_else(|| "Missing parameter name".to_string())?
            .as_str()
            .to_string();

        // Check if there's an ampersand and type
        let mut param_type = String::new();
        for part in parts {
            param_type.push_str(part.as_str());
        }

        Ok((param_name, param_type))
    }

    /// Build input function node
    fn build_input_function(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty input function".to_string())?;

        match inner.as_rule() {
            Rule::range_input => Self::build_range_input(inner),
            Rule::read_file_csv_input => Self::build_read_file_csv_input(inner),
            Rule::select_all_input => Self::build_select_all_input(inner),
            _ => Err(format!("Unknown input function: {:?}", inner.as_rule())),
        }
    }

    /// Build range input function
    fn build_range_input(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let mut inner = pair.into_inner();
        let n1 = inner
            .next()
            .ok_or_else(|| "Missing range start".to_string())?
            .as_str();
        let n2 = inner
            .next()
            .ok_or_else(|| "Missing range end".to_string())?
            .as_str();

        let count = (n2
            .parse::<num_bigint::BigInt>()
            .map_err(|e| format!("Could not parse range end: {}", e))?
            - n1.parse::<num_bigint::BigInt>()
                .map_err(|e| format!("Could not parse range start: {}", e))?)
        .to_biguint()
        .ok_or_else(|| "Range count is negative".to_string())?;

        Ok(InputFunctionNode {
            variability: InputVariability::Constant,
            input_count: InputCount::Known(count),
            code: format!("::marigold::marigold_impl::futures::stream::iter({n1}..{n2})"),
        })
    }

    fn build_read_file_csv_input(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let mut inner = pair.into_inner();

        // Extract path (quoted_string) - keep quotes for LALRPOP compatibility
        let path = inner
            .next()
            .ok_or_else(|| "Missing path in read_file".to_string())?
            .as_str();

        // Extract struct type (free_text_identifier)
        // Note: Pest only captures non-literal tokens, so we skip "csv", "struct", "=" literals
        let deserialization_struct = inner
            .next()
            .ok_or_else(|| "Missing struct type in read_file".to_string())?
            .as_str();

        // Check for optional infer_compression parameter (boolean_value)
        let infer_compression = inner.next().map(|p| p.as_str() == "true");

        // Determine whether to use gzip compression
        let use_gzip = match infer_compression {
            Some(false) => false, // Explicitly disabled
            Some(true) | None => {
                // Auto-detect from file extension (check if path ends with .gz")
                // Strip quotes from path to check extension
                let path_without_quotes = &path[1..path.len() - 1];
                path_without_quotes.ends_with(".gz")
            }
        };

        // Generate code based on compression
        let code = if use_gzip {
            format!(
                "
           ::marigold::marigold_impl::csv_async::AsyncDeserializer::from_reader(
             ::marigold::marigold_impl::async_compression::tokio::bufread::GzipDecoder::new(
              ::marigold::marigold_impl::tokio::io::BufReader::new(
                ::marigold::marigold_impl::tokio::fs::File::open({path})
                  .await
                  .expect(\"Marigold could not open file\")
              )
             ).compat()
           ).into_deserialize::<{deserialization_struct}>()
           "
            )
        } else {
            format!(
                "
           ::marigold::marigold_impl::csv_async::AsyncDeserializer::from_reader(
              ::marigold::marigold_impl::tokio::fs::File::open({path})
               .await
               .expect(\"Marigold could not open file\")
               .compat()
           ).into_deserialize::<{deserialization_struct}>()
           "
            )
        };

        Ok(InputFunctionNode {
            variability: InputVariability::Variable,
            input_count: InputCount::Unknown,
            code,
        })
    }

    /// Build select_all input
    fn build_select_all_input(pair: Pair<Rule>) -> Result<InputFunctionNode, String> {
        let mut streams: Vec<InputAndMaybeStreamFunctions> = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::input_and_maybe_stream_functions {
                let stream = Self::build_input_and_maybe_stream_functions(inner)?;
                streams.push(stream);
            }
        }

        if streams.is_empty() {
            return Err("select_all requires at least one stream".to_string());
        }

        let variability = crate::type_aggregation::aggregate_input_variability(
            streams.iter().map(|s| s.inp.variability.clone()),
        );
        let input_count = crate::type_aggregation::aggregate_input_count(
            streams.iter().map(|s| s.inp.input_count.clone()),
        );

        let stream_code = streams
            .iter()
            .map(|stream| {
                let code = stream.code();
                format!("::marigold::marigold_impl::run_stream::run_stream({code})")
            })
            .collect::<Vec<_>>()
            .join(",\n");

        let code = format!(
            "::marigold::marigold_impl::futures::prelude::stream::select_all::select_all([{stream_code}])"
        );

        Ok(InputFunctionNode {
            variability,
            input_count,
            code,
        })
    }

    /// Build input_and_maybe_stream_functions node
    fn build_input_and_maybe_stream_functions(
        pair: Pair<Rule>,
    ) -> Result<InputAndMaybeStreamFunctions, String> {
        let mut inner = pair.into_inner();

        let inp = Self::build_input_function(inner.next().ok_or_else(|| {
            "Missing input function in input_and_maybe_stream_functions".to_string()
        })?)?;

        let mut funs = Vec::new();
        for item in inner {
            if item.as_rule() == Rule::stream_function {
                funs.push(Self::build_stream_function(item)?);
            }
        }

        Ok(InputAndMaybeStreamFunctions { inp, funs })
    }

    /// Build stream function node
    fn build_stream_function(pair: Pair<Rule>) -> Result<StreamFunctionNode, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty stream function".to_string())?;

        let code = match inner.as_rule() {
            Rule::map_fn => Self::build_map_fn(inner)?,
            Rule::filter_fn => Self::build_filter_fn(inner)?,
            Rule::filter_map_fn => Self::build_filter_map_fn(inner)?,
            Rule::permutations_fn => Self::build_permutations_fn(inner)?,
            Rule::permutations_with_replacement_fn => {
                Self::build_permutations_with_replacement_fn(inner)?
            }
            Rule::combinations_fn => Self::build_combinations_fn(inner)?,
            Rule::keep_first_n_fn => Self::build_keep_first_n_fn(inner)?,
            Rule::fold_fn => Self::build_fold_fn(inner)?,
            Rule::ok_fn => {
                "filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap())".to_string()
            }
            Rule::ok_or_panic_fn => "map(|r| r.unwrap())".to_string(),
            _ => return Err(format!("Unknown stream function: {:?}", inner.as_rule())),
        };

        Ok(StreamFunctionNode { code })
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

        #[cfg(not(any(feature = "tokio", feature = "async-std")))]
        return Ok(format!(
            "filter(|v| ::marigold::marigold_impl::futures::future::ready({filter_fn}(v)))"
        ));

        #[cfg(any(feature = "tokio", feature = "async-std"))]
        return Ok(format!("map(|v| async move {{ if {filter_fn}(&v) {{ Some(v) }} else {{ None }} }}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).filter_map(|v| v)"));
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
        return Ok(format!("map(|v| async move {{ {filter_map_fn}(v) }}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).filter_map(|v| v)"));
    }

    fn build_permutations_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations size".to_string())?
            .as_str();
        Ok(format!("permutations({n}).await"))
    }

    fn build_permutations_with_replacement_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing permutations_with_replacement size".to_string())?
            .as_str();
        Ok(format!("permutations_with_replacement({n}).await"))
    }

    fn build_combinations_fn(pair: Pair<Rule>) -> Result<String, String> {
        let n = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Missing combinations size".to_string())?
            .as_str();
        Ok(format!("combinations({n}).await"))
    }

    fn build_keep_first_n_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut inner = pair.into_inner();
        let n = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n size".to_string())?
            .as_str();
        let value_fn = inner
            .next()
            .ok_or_else(|| "Missing keep_first_n value function".to_string())?
            .as_str();
        Ok(format!("keep_first_n({n}, {value_fn}).await"))
    }

    fn build_fold_fn(pair: Pair<Rule>) -> Result<String, String> {
        let mut inner = pair.into_inner();
        let state = inner
            .next()
            .ok_or_else(|| "Missing fold state".to_string())?
            .as_str();
        let fun = inner
            .next()
            .ok_or_else(|| "Missing fold function".to_string())?
            .as_str();

        let number_or_constructor = match state.trim().parse::<f64>() {
            Ok(_) => state.to_string(),
            Err(_) => format!("{state}()"),
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
                // Parse write_file(path, format[, compression=none|gz])
                let mut inner_pairs = inner.into_inner();

                // Extract path (quoted_string) - keep quotes for LALRPOP compatibility
                let path_pair = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing path in write_file".to_string())?;
                let path = path_pair.as_str();

                // Extract format (write_file_format)
                let format_pair = inner_pairs
                    .next()
                    .ok_or_else(|| "Missing format in write_file".to_string())?;
                let format = format_pair.as_str();

                // Only "csv" format is currently supported
                if format != "csv" {
                    return Err(format!("Unsupported write_file format: {}", format));
                }

                // Extract optional compression (write_file_compression)
                let compression = if let Some(compression_pair) = inner_pairs.next() {
                    // compression_pair is write_file_compression, get its inner compression_type
                    let compression_type = compression_pair
                        .into_inner()
                        .next()
                        .ok_or_else(|| "Missing compression type".to_string())?;
                    Some(compression_type.as_str())
                } else {
                    None
                };

                // Determine whether to use gzip based on compression parameter or .gz extension
                let use_gzip = match compression {
                    Some("gz") => true,
                    Some("none") => false,
                    None => path.ends_with(".gz\""), // Auto-detect from extension
                    Some(other) => {
                        return Err(format!("Invalid compression type: {}", other));
                    }
                };

                // Generate stream_prefix and stream_postfix based on compression
                if use_gzip {
                    Self::generate_write_file_with_gzip(path)
                } else {
                    Self::generate_write_file_no_compression(path)
                }
            }
            _ => Err(format!("Unknown output function: {:?}", inner.as_rule())),
        }
    }

    fn generate_write_file_with_gzip(path: &str) -> Result<OutputFunctionNode, String> {
        Ok(OutputFunctionNode {
            stream_prefix: format!(
                "{{
          if let Some(parent) = ::std::path::Path::new({path}).parent() {{
            ::marigold::marigold_impl::tokio::fs::create_dir_all(parent)
              .await
              .expect(\"could not create parent directory for output file\");
          }}

          static WRITER: ::marigold::marigold_impl::once_cell::sync::OnceCell<
            ::marigold::marigold_impl::tokio::sync::Mutex<
              ::marigold::marigold_impl::csv_async::AsyncSerializer<
                  ::marigold::marigold_impl::tokio_util::compat::Compat<
                    ::marigold::marigold_impl::async_compression::tokio::write::GzipEncoder<
                        ::marigold::marigold_impl::writer::Writer
                    >
                  >
              >
            >
          > = ::marigold::marigold_impl::once_cell::sync::OnceCell::new();

          WRITER.set(
            ::marigold::marigold_impl::tokio::sync::Mutex::new(
              ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                ::marigold::marigold_impl::async_compression::tokio::write::GzipEncoder::new(
                  ::marigold::marigold_impl::writer::Writer::file(
                    ::marigold::marigold_impl::tokio::fs::File::create({path})
                       .await
                       .expect(\"Could not write to file\")
                  )
                )
                .compat_write()
              )
            )
          ).expect(\"Could not put CSV writer into OnceCell\");

          let mut stream_to_write =

         "
            ),
            stream_postfix: "
            ;
            stream_to_write.filter_map(
              |v| async move {
                WRITER
                  .get()
                  .expect(\"Could not get CSV writer from OnceCell\")
                  .lock()
                  .await
                  .serialize(v)
                  .await
                  .expect(\"could not write record to CSV\");
                None
              }
            ).chain(
              // after the stream is complete, flush the writer.
              ::marigold::marigold_impl::futures::stream::iter(0..1)
                .filter_map(|_v| async {
                  let mut serializer_guard = WRITER
                    .get() // gets Mutex<...>
                    .expect(\"Could not get CSV writer from OnceCell\")
                    .lock()
                    .await;
                  let mut serializer = std::mem::replace(
                    &mut *serializer_guard,
                    ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                      ::marigold::marigold_impl::async_compression::tokio::write::GzipEncoder::new(
                        ::marigold::marigold_impl::writer::Writer::vector()
                      )
                      .compat_write()
                    )
                  );
                  serializer
                    .into_inner()
                    .await
                    .expect(\"Could not get underlying writer from serializer\")
                    .get_mut()
                    .shutdown()
                    .await
                    .expect(\"Could not shut down underlying writer\");
                  None
                })
            )
        }"
            .to_string(),
            returning: false,
        })
    }

    fn generate_write_file_no_compression(path: &str) -> Result<OutputFunctionNode, String> {
        Ok(OutputFunctionNode {
            stream_prefix: format!(
                "{{
            if let Some(parent) = ::std::path::Path::new({path}).parent() {{
              ::marigold::marigold_impl::tokio::fs::create_dir_all(parent)
                .await
                .expect(\"could not create parent directory for output file\");
            }}

            static WRITER: ::marigold::marigold_impl::once_cell::sync::OnceCell<
              ::marigold::marigold_impl::tokio::sync::Mutex<
                ::marigold::marigold_impl::csv_async::AsyncSerializer<
                  ::marigold::marigold_impl::tokio_util::compat::Compat<
                    ::marigold::marigold_impl::writer::Writer
                  >
                >
              >
            > = ::marigold::marigold_impl::once_cell::sync::OnceCell::new();

            WRITER.set(
              ::marigold::marigold_impl::tokio::sync::Mutex::new(
                ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                  ::marigold::marigold_impl::writer::Writer::file(
                    ::marigold::marigold_impl::tokio::fs::File::create({path})
                       .await
                       .expect(\"Could not write to file\")
                  )
                  .compat_write()
                )
              )
            ).expect(\"Could not put CSV writer into OnceCell\");

            let mut stream_to_write =

           "
            ),
            stream_postfix: "
              ;
              stream_to_write.filter_map(
                |v| async move {
                  WRITER
                    .get()
                    .expect(\"Could not get CSV writer from OnceCell\")
                    .lock()
                    .await
                    .serialize(v)
                    .await
                    .expect(\"could not write record to CSV\");
                  None
                }
              ).chain(
                // after the stream is complete, flush the writer.
                ::marigold::marigold_impl::futures::stream::iter(0..1)
                  .filter_map(|_v| async {
                      let mut serializer_guard = WRITER
                        .get() // gets Mutex<...>
                        .expect(\"Could not get CSV writer from OnceCell\")
                        .lock()
                        .await;
                      let mut serializer = std::mem::replace(
                        &mut *serializer_guard,
                        ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                            ::marigold::marigold_impl::writer::Writer::vector()
                              .compat_write()
                        )
                      );
                      serializer
                        .into_inner()
                        .await
                        .expect(\"Could not get underlying writer from serializer\")
                        .get_mut()
                        .shutdown()
                        .await
                        .expect(\"Could not shut down underlying writer\");
                      None
                  })
              )
          }"
            .to_string(),
            returning: false,
        })
    }

    /// Parse struct fields from braced content (matching LALRPOP regex approach)
    fn parse_struct_fields(braced_content: &str) -> Result<Vec<(String, Type)>, String> {
        use once_cell::sync::Lazy;
        use regex::Regex;

        static WHITESPACE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[\s]+").unwrap());
        static FIELD_DECLARATION: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"([\S]+)[\s]*:[\s]*(.*)").unwrap());

        let cleaned = WHITESPACE.replace_all(braced_content, " ");
        let content = &cleaned[1..cleaned.len() - 1]; // Remove surrounding braces

        let fields = content
            .split(',')
            .filter_map(|t| {
                FIELD_DECLARATION.captures(t).map(|c| {
                    (
                        c[1].to_string(),
                        Type::from_str(&c[2]).expect("could not parse type in struct definition"),
                    )
                })
            })
            .collect::<Vec<_>>();

        Ok(fields)
    }
}
