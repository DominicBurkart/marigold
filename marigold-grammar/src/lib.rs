#![forbid(unsafe_code)]

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use lalrpop_util::ParseError;
extern crate proc_macro;
use crate::ast::Token;

pub use itertools;

pub mod nodes;
mod type_aggregation;

lalrpop_mod!(#[allow(clippy::all)] pub ast);

lazy_static! {
    static ref PARSER: ast::ProgramParser = ast::ProgramParser::new();
}

pub fn marigold_parse<'a>(
    s: &'a str,
) -> Result<String, ParseError<usize, Token<'a>, &'static str>> {
    PARSER.parse(s)
}

fn parse_program(
    program: pest::iterators::Pair<Rule>,
) -> Result<ast::MarigoldProgram, GrammarError> {
    assert_eq!(program.as_rule(), Rule::program);

    let pairs = program.into_inner().into_iter();
    let mut streams = Vec::new();
    let mut functions = Vec::new();
    let mut variables = Vec::new();
    
    for pair in pairs {
        match pair.as_rule() {
            Rule::statement => {
                let statement = pair.into_inner().next().unwrap();
                match statement.as_rule() {
                    Rule::stream => streams.push(parse_stream(statement)?),
                    Rule::function_def => functions.push(parse_function_def(statement)?),
                    Rule::stream_variable => variables.push(parse_stream_variable(statement)?),
                    _ => unimplemented!("unknown statement type: {:?}", statement.as_rule()),
                }
            },
            _ => unimplemented!("unknown program element: {:?}", pair.as_rule()),
        }
    }

    Ok(ast::MarigoldProgram { streams, functions, variables })
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
        Rule::variable_ref => parse_variable_ref(specific_input),
        _ => unimplemented!("no parser for input type {:?}", specific_input.as_rule()),
    }
}

fn parse_read_file(
    specific_input: pest::iterators::Pair<Rule>,
) -> Result<ast::StreamInput, GrammarError> {
    assert_eq!(specific_input.as_rule(), Rule::read_file);
    let specific_input_rule: Rule = specific_input.as_rule();
    let fields = specific_input.into_inner();
    let mut params = HashMap::with_capacity(fields.len());
    fields.into_iter().for_each(|field| {
        let field_rule = field.as_rule();
        params.insert(field.as_rule(), field).map(|_| {
            unimplemented!(
                "duplicate entry for rule {:?} while parsing {:?}",
                field_rule,
                specific_input_rule
            )
        });
    });

    // generate stream input
    let out = ast::StreamInput {
        format: {
            if params.contains_key(&Rule::input_format) {
                let specific_format = params
                    .remove(&Rule::input_format)
                    .unwrap()
                    .into_inner()
                    .into_iter()
                    .next()
                    .unwrap()
                    .into_inner()
                    .into_iter()
                    .next()
                    .unwrap()
                    .as_rule();
                match specific_format {
                    Rule::csv_data_stream_format => ast::DataStreamFormat::CSV,
                    _ => unimplemented!("unknown data stream format {:?}", specific_format),
                }
            } else {
                ast::DataStreamFormat::INFER
            }
        },
        source: Box::new(ast::RuntimeAccessibleFile {
            path: params
                .remove(&Rule::file_path)
                .expect("file_path not found")
                .into_inner()
                .map(|character| character.as_str().chars().collect_vec())
                .into_iter()
                .flatten()
                .collect(),
        }),
        type_ident: params.remove(&Rule::input_struct).map(|input_struct| {
            input_struct
                .into_inner()
                .into_iter()
                .map(|character| character.as_str().chars().collect_vec())
                .flatten()
                .collect()
        }),
    };

    // check we didn't miss any parsed rules
    if !params.is_empty() {
        let unhandled_rules = params.keys().collect_vec();
        if unhandled_rules.len().is_one() {
            unimplemented!(
                "rule not accounted for while parsing {:?}: {:?}",
                specific_input_rule,
                unhandled_rules.get(0).unwrap()
            )
        }
        unimplemented!(
            "{} rules not accounted for while parsing {:?}: {:?}",
            params.len(),
            specific_input_rule,
            unhandled_rules
        )
    }

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
    let fields = specific_output.into_inner();
    let mut params = HashMap::with_capacity(fields.len());
    fields.into_iter().for_each(|field| {
        let field_rule = field.as_rule();
        params.insert(field.as_rule(), field).map(|_| {
            unimplemented!(
                "duplicate entry for rule {:?} while parsing {:?}",
                field_rule,
                specific_output_rule
            )
        });
    });

    let output_format = {
        let output_rule = params
            .remove(&Rule::output_format)
            .expect("output format is mandatory")
            .into_inner()
            .into_iter()
            .next()
            .unwrap()
            .into_inner()
            .into_iter()
            .next()
            .unwrap()
            .as_rule();
        match output_rule {
            Rule::csv_data_stream_format => ast::DataStreamFormat::CSV,
            _ => unimplemented!(
                "unimplemented format for {:?}: {:?}",
                specific_output_rule,
                output_rule
            ),
        }
    };

    let target = Box::new(ast::RuntimeAccessibleFile {
        path: params
            .remove(&Rule::file_path)
            .expect("file_path not found")
            .into_inner()
            .map(|character| character.as_str().chars().collect_vec())
            .into_iter()
            .flatten()
            .collect(),
    });

    // check we didn't miss any parsed rules
    if !params.is_empty() {
        let unhandled_rules = params.keys().collect_vec();
        if unhandled_rules.len().is_one() {
            unimplemented!(
                "rule not accounted for while parsing {:?}: {:?}",
                specific_output_rule,
                unhandled_rules.get(0).unwrap()
            )
        }
        unimplemented!(
            "{} rules not accounted for while parsing {:?}: {:?}",
            params.len(),
            specific_output_rule,
            unhandled_rules
        )
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
            fn double(x: i32) -> i32 {
                x * 2
            }

            range(0, 3)
                .map(double)
                .return
        "#,
        )
        .unwrap();

        // Should parse function definition and stream
        assert_eq!(parsed.streams.len(), 1);
        assert_eq!(parsed.functions.len(), 1);
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
        assert_eq!(parsed.variables.len(), 1);
        
        let variable = &parsed.variables[0];
        assert_eq!(variable.name, "digits");
        // TODO: Also need to support variable usage in streams
    }
}
