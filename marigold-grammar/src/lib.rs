#![forbid(unsafe_code)]

use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

pub use itertools;

pub mod ast;

#[cfg(feature = "static_analysis")]
pub mod static_analysis;


#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Parse error: {0}")]
    ParseError(#[from] pest::error::Error<Rule>),
}

pub fn marigold_parse(s: &str) -> Result<ast::MarigoldProgram, GrammarError> {
    let mut pairs = PARSER::parse(Rule::program, s)?;
    let program = pairs.next().unwrap();
    parse_program(program)
}

fn parse_program(
    program: pest::iterators::Pair<Rule>,
) -> Result<ast::MarigoldProgram, GrammarError> {
    assert_eq!(program.as_rule(), Rule::program);

    let statement_list = program.into_inner().next().unwrap();
    assert_eq!(statement_list.as_rule(), Rule::statement_list);
    
    let pairs = statement_list.into_inner().into_iter();
    let mut streams = Vec::new();
    let mut functions = Vec::new();
    let mut structs = Vec::new();
    let mut variables = Vec::new();
    
    for pair in pairs {
        match pair.as_rule() {
            Rule::statement => {
                let statement = pair.into_inner().next().unwrap();
                match statement.as_rule() {
                    Rule::stream => streams.push(parse_stream(statement)?),
                    Rule::function_def => functions.push(parse_function_def(statement)?),
                    Rule::struct_def => structs.push(parse_struct_def(statement)?),
                    Rule::stream_variable => variables.push(parse_stream_variable(statement)?),
                    _ => unimplemented!("unknown statement type: {:?}", statement.as_rule()),
                }
            },
            _ => {
                // Skip whitespace and other non-statement elements
                // Note: WHITESPACE is marked as silent (_) so it shouldn't appear here
            }
        }
    }

    Ok(ast::MarigoldProgram { streams, functions, structs, variables })
}

fn parse_stream(stream: pest::iterators::Pair<Rule>) -> Result<ast::Stream, GrammarError> {
    assert_eq!(stream.as_rule(), Rule::stream);

    let mut pairs = stream.into_inner().into_iter();
    let n_transformations = pairs.len() - 2;

    let input = parse_input(pairs.next().unwrap())?;

    let mut transformations = Vec::with_capacity(n_transformations);
    for _ in 0..n_transformations {
        transformations.push(parse_transformation(pairs.next().unwrap())?);
    }

    let output = parse_output(pairs.next().unwrap())?;

    Ok(ast::Stream {
        input,
        transformations,
        output,
    })
}

fn parse_input(input: pest::iterators::Pair<Rule>) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(input.as_rule(), Rule::input);

    let specific_input = input.into_inner().next().unwrap();
    match specific_input.as_rule() {
        Rule::read_file => parse_read_file(specific_input),
        Rule::range_input => parse_range_input(specific_input),
        Rule::select_all_input => parse_select_all_input(specific_input),
        Rule::variable_ref => parse_variable_ref(specific_input),
        _ => unimplemented!("no parser for input type {:?}", specific_input.as_rule()),
    }
}

fn parse_read_file(
    specific_input: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(specific_input.as_rule(), Rule::read_file);

    // read_file path_string
    let mut rules = specific_input
        .into_inner()
        .into_iter()
        .collect::<Vec<pest::iterators::Pair<Rule>>>();

    // Extract the filepath
    let filepath = rules.remove(0).as_str();

    let mut type_ident: Option<String> = None;
    let mut format: ast::DataStreamFormat = ast::DataStreamFormat::INFER;

    // Check the rest, handling in any order
    let mut unhandled_rules = Vec::new();
    for rule in rules {
        match rule.as_rule() {
            Rule::data_stream_format => {
                let format_str = rule.as_str();
                format = match format_str {
                    "csv" => ast::DataStreamFormat::CSV,
                    _ => ast::DataStreamFormat::INFER,
                };
            }
            Rule::input_format => {
                // Handle wrapper rule - extract the inner data_stream_format
                let inner_rule = rule.into_inner().next().unwrap();
                let format_str = inner_rule.as_str();
                format = match format_str {
                    "csv" => ast::DataStreamFormat::CSV,
                    _ => ast::DataStreamFormat::INFER,
                };
            }
            Rule::input_struct => {
                type_ident = Some(rule.as_str().to_string());
            }
            _ => unhandled_rules.push(rule.as_rule()),
        }
    }

    if !unhandled_rules.is_empty() {
        return Err(GrammarError::ParseError(pest::error::Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: format!("unhandled rule: {:?}", unhandled_rules),
            },
            pest::Position::from_start(""),
        )));
    }

    let out = ast::StreamInput {
        format,
        source: Box::new(ast::RuntimeAccessibleFile {
            path: filepath.trim_matches('"').to_string(),
        }),
        type_ident,
    };

    // if all rules were processed, return
    Ok(out)
}

fn parse_range_input(
    specific_input: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(specific_input.as_rule(), Rule::range_input);
    
    let mut inner = specific_input.into_inner();
    let start: i32 = inner.next().unwrap().as_str().parse().unwrap();
    let end: i32 = inner.next().unwrap().as_str().parse().unwrap();
    
    Ok(ast::StreamInput {
        format: ast::DataStreamFormat::INFER,
        source: Box::new(ast::RangeInput { start, end }),
        type_ident: None,
    })
}

fn parse_variable_ref(
    specific_input: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(specific_input.as_rule(), Rule::variable_ref);
    
    let name = specific_input.as_str().to_string();
    
    Ok(ast::StreamInput {
        format: ast::DataStreamFormat::INFER,
        source: Box::new(ast::VariableReference { name }),
        type_ident: None,
    })
}

fn parse_output(output: pest::iterators::Pair<Rule>) -> Result<ast::StreamOutput, GrammarError> {
    assert_eq!(output.as_rule(), Rule::output);
    let specific_output = output.into_inner().next().unwrap();
    match specific_output.as_rule() {
        Rule::write_file => parse_write_file(specific_output),
        Rule::return_output => parse_return_output(specific_output),
        _ => unimplemented!("no parser for output type {:?}", specific_output.as_rule()),
    }
}

fn parse_write_file(
    specific_output: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamOutput, GrammarError> {
    assert_eq!(specific_output.as_rule(), Rule::write_file);
    let specific_output_rule: Rule = specific_output.as_rule();
    let mut rules = specific_output
        .into_inner()
        .into_iter()
        .collect::<Vec<pest::iterators::Pair<Rule>>>();

    // Extract the filepath
    let filepath = rules.remove(0).as_str();

    let mut output_format = ast::DataStreamFormat::INFER;

    // Check the rest, handling in any order
    let mut unhandled_rules = Vec::new();
    for rule in rules {
        match rule.as_rule() {
            Rule::data_stream_format => {
                let format_str = rule.as_str();
                output_format = match format_str {
                    "csv" => ast::DataStreamFormat::CSV,
                    _ => ast::DataStreamFormat::INFER,
                };
            }
            Rule::output_format => {
                // Handle wrapper rule - extract the inner data_stream_format
                let inner_rule = rule.into_inner().next().unwrap();
                let format_str = inner_rule.as_str();
                output_format = match format_str {
                    "csv" => ast::DataStreamFormat::CSV,
                    _ => ast::DataStreamFormat::INFER,
                };
            }
            _ => unhandled_rules.push(rule.as_rule()),
        }
    }

    let target = Box::new(ast::RuntimeAccessibleFile {
        path: filepath.trim_matches('"').to_string(),
    });

    if !unhandled_rules.is_empty() {
        return Err(GrammarError::ParseError(pest::error::Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: format!("unhandled rule: {:?}", unhandled_rules),
            },
            pest::Position::from_start(""),
        )));
    }

    Ok(ast::StreamOutput {
        format: output_format,
        target,
    })
}

fn parse_return_output(
    specific_output: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamOutput, GrammarError> {
    assert_eq!(specific_output.as_rule(), Rule::return_output);
    
    Ok(ast::StreamOutput {
        format: ast::DataStreamFormat::INFER,
        target: Box::new(ast::ReturnTarget {}),
    })
}

fn parse_transformation(
    transformation: pest::iterators::Pair<Rule>,
) -> Result<Box<dyn ast::StreamTransformation>, GrammarError> {
    assert_eq!(transformation.as_rule(), Rule::transformation);
    let specific_transformation = transformation.into_inner().next().unwrap();
    match specific_transformation.as_rule() {
        Rule::ok_or_panic => Ok(Box::new(ast::OkOrPanic {})),
        Rule::map_transform => {
            let function_name = specific_transformation.into_inner().next().unwrap().as_str().to_string();
            Ok(Box::new(ast::MapTransformation { function_name }))
        },
        Rule::filter_transform => {
            let function_name = specific_transformation.into_inner().next().unwrap().as_str().to_string();
            Ok(Box::new(ast::FilterTransformation { function_name }))
        },
        Rule::fold_transform => {
            let mut inner = specific_transformation.into_inner();
            let initial_value = inner.next().unwrap().as_str().to_string();
            let accumulator_function = inner.next().unwrap().as_str().to_string();
            Ok(Box::new(ast::FoldTransformation { initial_value, accumulator_function }))
        },
        Rule::permutations_transform => {
            let size: i32 = specific_transformation.into_inner().next().unwrap().as_str().parse().unwrap();
            Ok(Box::new(ast::PermutationsTransformation { size }))
        },
        Rule::combinations_transform => {
            let size: i32 = specific_transformation.into_inner().next().unwrap().as_str().parse().unwrap();
            Ok(Box::new(ast::CombinationsTransformation { size }))
        },
        Rule::keep_first_n_transform => {
            let mut inner = specific_transformation.into_inner();
            let n: i32 = inner.next().unwrap().as_str().parse().unwrap();
            let comparator_function = inner.next().unwrap().as_str().to_string();
            Ok(Box::new(ast::KeepFirstNTransformation { n, comparator_function }))
        },
        Rule::permutations_with_replacement_transform => {
            let size: i32 = specific_transformation.into_inner().next().unwrap().as_str().parse().unwrap();
            Ok(Box::new(ast::PermutationsWithReplacementTransformation { size }))
        },
        _ => unimplemented!(
            "no parser for transformation type {:?}",
            specific_transformation.as_rule()
        ),
    }
}

fn parse_function_def(
    function_def: pest::iterators::Pair<Rule>,
) -> Result<ast::FunctionDefinition, GrammarError> {
    assert_eq!(function_def.as_rule(), Rule::function_def);
    
    let specific_function = function_def.into_inner().next().unwrap();
    let rule = specific_function.as_rule();
    let mut inner = specific_function.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let mut parameters = Vec::new();
    
    match rule {
        Rule::function_def_with_params => {
            let params = inner.next().unwrap(); // Should be function_params
            for param in params.into_inner() {
                let mut param_inner = param.into_inner();
                let param_name = param_inner.next().unwrap().as_str().to_string();
                let param_type = param_inner.next().unwrap().as_str().to_string();
                parameters.push((param_name, param_type));
            }
        },
        Rule::function_def_no_params => {
            // No parameters, skip
        },
        _ => unreachable!(),
    }
    
    let return_type = inner.next().unwrap().as_str().to_string();
    let body = inner.next().unwrap().as_str().to_string();
    
    Ok(ast::FunctionDefinition {
        name,
        parameters,
        return_type,
        body,
    })
}

fn parse_stream_variable(
    stream_variable: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamVariable, GrammarError> {
    assert_eq!(stream_variable.as_rule(), Rule::stream_variable);
    
    let mut inner = stream_variable.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let input = parse_input(inner.next().unwrap())?;
    
    Ok(ast::StreamVariable { name, input })
}

fn parse_struct_def(
    struct_def: pest::iterators::Pair<Rule>,
) -> Result<ast::StructDefinition, GrammarError> {
    assert_eq!(struct_def.as_rule(), Rule::struct_def);
    
    let mut inner = struct_def.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let struct_body = inner.next().unwrap(); // struct_body
    
    let mut fields = Vec::new();
    if let Some(struct_fields) = struct_body.into_inner().next() {
        // struct_fields exists
        for field in struct_fields.into_inner() {
            let mut field_inner = field.into_inner();
            let field_name = field_inner.next().unwrap().as_str().to_string();
            let field_type = field_inner.next().unwrap().as_str().to_string();
            fields.push((field_name, field_type));
        }
    }
    
    Ok(ast::StructDefinition { name, fields })
}

fn parse_select_all_input(
    specific_input: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(specific_input.as_rule(), Rule::select_all_input);
    
    let mut inputs = Vec::new();
    for input_pair in specific_input.into_inner() {
        inputs.push(parse_input(input_pair)?);
    }
    
    
    Ok(ast::StreamInput {
        format: ast::DataStreamFormat::INFER,
        source: Box::new(ast::SelectAllInput { inputs }),
        type_ident: None,
    })
}

#[derive(Parser)]
#[grammar = "marigold.pest"]
pub struct PARSER;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    
    #[test]
    fn csv_in_out() {
        let parsed = marigold_parse(
            r#"
            read_file("./woof.csv", csv, struct=woof)
                .ok_or_panic()
                .write_file("miaow.csv", csv)
            
            read_file("poof.csv")
                .write_file("doof.csv", csv)
        "#,
        )
        .unwrap();

        assert_eq!(
            parsed,
            MarigoldProgram {
                streams: vec![
                    Stream {
                        input: StreamInput {
                            format: DataStreamFormat::CSV,
                            source: Box::new(RuntimeAccessibleFile {
                                path: "./woof.csv".to_string()
                            }),
                            type_ident: Option::Some("woof".to_string())
                        },
                        transformations: vec![Box::new(OkOrPanic {})],
                        output: StreamOutput {
                            format: DataStreamFormat::CSV,
                            target: Box::new(RuntimeAccessibleFile {
                                path: "miaow.csv".to_string()
                            }),
                        }
                    },
                    Stream {
                        input: StreamInput {
                            format: DataStreamFormat::INFER,
                            source: Box::new(RuntimeAccessibleFile {
                                path: "poof.csv".to_string()
                            }),
                            type_ident: None
                        },
                        transformations: Vec::new(),
                        output: StreamOutput {
                            format: DataStreamFormat::CSV,
                            target: Box::new(RuntimeAccessibleFile {
                                path: "doof.csv".to_string()
                            }),
                        }
                    },
                ],
                functions: vec![],
                structs: vec![],
                variables: vec![],
            }
        )
    }

    // TDD Tests for Missing Grammar Features

    #[test]
    fn range_input_simple() {
        let parsed = marigold_parse(
            r#"
            range(0, 3)
                .write_file("output.csv", csv)
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        let stream = &parsed.streams[0];
        
        // Should parse range as input source
        assert_eq!(stream.input.format, DataStreamFormat::INFER);
        assert_eq!(stream.transformations.len(), 0);
    }

    #[test] 
    fn return_output_simple() {
        let parsed = marigold_parse(
            r#"
            read_file("test.csv")
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        let stream = &parsed.streams[0];
        
        // Should parse .return as output type
        assert_eq!(stream.transformations.len(), 0);
        // TODO: Add ReturnOutput type to OutputType enum
    }

    #[test]
    fn range_to_return() {
        let parsed = marigold_parse(
            r#"
            range(0, 5)
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        // TODO: Verify range input and return output
    }

    #[test]
    fn basic_map_transformation() {
        let parsed = marigold_parse(
            r#"
            range(0, 3)
                .map(double)
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        let stream = &parsed.streams[0];
        
        // Should have map transformation
        assert_eq!(stream.transformations.len(), 1);
        // TODO: Add MapTransformation type
    }

    #[test] 
    fn basic_filter_transformation() {
        let parsed = marigold_parse(
            r#"
            range(0, 10)
                .filter(is_odd)
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        let stream = &parsed.streams[0];
        
        // Should have filter transformation
        assert_eq!(stream.transformations.len(), 1);
        // TODO: Add FilterTransformation type
    }

    #[test]
    fn chained_transformations() {
        let parsed = marigold_parse(
            r#"
            range(0, 10)
                .filter(is_odd)
                .map(double)
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 1);
        let stream = &parsed.streams[0];
        
        // Should have both transformations in order
        assert_eq!(stream.transformations.len(), 2);
    }

    #[test]
    fn function_definition_simple() {
        let parsed = marigold_parse(
            r#"
            fn double(x: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
                x * 2
            %%%MARIGOLD_FUNCTION_END%%%

            range(0, 3)
                .map(double)
                .return
        "#,
        )
        .unwrap();

        // Should parse function definition and stream
        assert_eq!(parsed.streams.len(), 1);
        assert_eq!(parsed.functions.len(), 1);
        assert_eq!(parsed.structs.len(), 0);
        assert_eq!(parsed.variables.len(), 0);
        
        let function = &parsed.functions[0];
        assert_eq!(function.name, "double");
        assert_eq!(function.parameters, vec![("x".to_string(), "i32".to_string())]);
        assert_eq!(function.return_type, "i32");
    }

    #[test]
    fn multiple_streams() {
        let parsed = marigold_parse(
            r#"
            range(0, 3)
                .return

            range(5, 8)
                .return
        "#,
        )
        .unwrap();

        assert_eq!(parsed.streams.len(), 2);
    }

    #[test]
    fn stream_variable_assignment() {
        let parsed = marigold_parse(
            r#"
            digits = range(0, 10)

            digits
                .filter(is_odd)
                .return
        "#,
        )
        .unwrap();

        // Should parse variable assignment and usage
        assert_eq!(parsed.streams.len(), 1);
        assert_eq!(parsed.functions.len(), 0);
        assert_eq!(parsed.structs.len(), 0);
        assert_eq!(parsed.variables.len(), 1);
        
        let variable = &parsed.variables[0];
        assert_eq!(variable.name, "digits");
        // TODO: Also need to support variable usage in streams
    }
}
