#![forbid(unsafe_code)]

use std::collections::HashMap;

use itertools::Itertools;
use num_traits::One;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

pub use itertools;
pub mod ast;
#[cfg(feature = "static_analysis")]
pub mod static_analysis;

#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Parse error")]
    ParseError(#[from] pest::error::Error<Rule>),
}

pub fn marigold_parse<'a>(s: &'a str) -> Result<ast::MarigoldProgram, GrammarError> {
    let parsed = PARSER::parse(Rule::program, s.trim())?.next().unwrap();
    parse_program(parsed)
}

fn parse_program(
    program: pest::iterators::Pair<Rule>,
) -> Result<ast::MarigoldProgram, GrammarError> {
    assert_eq!(program.as_rule(), Rule::program);

    let pairs = program.into_inner().into_iter();
    let mut streams = Vec::with_capacity(pairs.len());
    for pair in pairs {
        match pair.as_rule() {
            Rule::stream => streams.push(parse_stream(pair)?),
            _ => unimplemented!(),
        }
    }

    Ok(ast::MarigoldProgram { streams })
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
        source: Box::new(ast::RuntimeFile {
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

fn parse_output(output: pest::iterators::Pair<Rule>) -> Result<ast::StreamOutput, GrammarError> {
    assert_eq!(output.as_rule(), Rule::output);
    let specific_output = output.into_inner().next().unwrap();
    match specific_output.as_rule() {
        Rule::write_file => parse_write_file(specific_output),
        _ => unimplemented!("no parser for input type {:?}", specific_output.as_rule()),
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

    let target = Box::new(ast::RuntimeFile {
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

fn parse_transformation(
    transformation: pest::iterators::Pair<Rule>,
) -> Result<Box<dyn ast::StreamTransformation>, GrammarError> {
    assert_eq!(transformation.as_rule(), Rule::transformation);
    let specific_transformation = transformation.into_inner().next().unwrap();
    match specific_transformation.as_rule() {
        Rule::ok_or_panic => Ok(Box::new(ast::OkOrPanic {})),
        _ => unimplemented!(
            "no parser for input type {:?}",
            specific_transformation.as_rule()
        ),
    }
}

#[derive(Parser)]
#[grammar = "marigold.pest"]
pub struct PARSER;

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;

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
                            source: Box::new(RuntimeFile {
                                path: "./woof.csv".to_string()
                            }),
                            type_ident: Option::Some("woof".to_string())
                        },
                        transformations: vec![Box::new(OkOrPanic {})],
                        output: StreamOutput {
                            format: DataStreamFormat::CSV,
                            target: Box::new(RuntimeFile {
                                path: "miaow.csv".to_string()
                            }),
                        }
                    },
                    Stream {
                        input: StreamInput {
                            format: DataStreamFormat::INFER,
                            source: Box::new(RuntimeFile {
                                path: "poof.csv".to_string()
                            }),
                            type_ident: None
                        },
                        transformations: Vec::new(),
                        output: StreamOutput {
                            format: DataStreamFormat::CSV,
                            target: Box::new(RuntimeFile {
                                path: "doof.csv".to_string()
                            }),
                        }
                    },
                ]
            }
        )
    }
}
