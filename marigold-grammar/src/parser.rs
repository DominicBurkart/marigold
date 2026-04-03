#![forbid(unsafe_code)]

//! # Marigold Parser Module
//!
//! This module provides the Marigold parser using the Pest parsing library.
//!
//! ## Usage Examples
//!
//! Parse Marigold code:
//! ```ignore
//! let result = parse_marigold("range(0, 1).return")?;
//! ```
//!
//! Explicitly create a parser instance:
//! ```ignore
//! let parser = PestParser::new();
//! let result = parser.parse("range(0, 1).return")?;
//! ```

use pest::Parser;
use std::fmt;

/// Error type for parser
#[derive(Debug, Clone)]
pub struct MarigoldParseError(pub String);

impl fmt::Display for MarigoldParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error: {}", self.0)
    }
}

impl std::error::Error for MarigoldParseError {}

/// Pest parser backend
pub struct PestParser;

/// Pest-derived parser struct that holds the compiled grammar
#[derive(pest_derive::Parser)]
#[grammar = "marigold.pest"]
pub struct MarigoldPestParser;

impl PestParser {
    /// Create a new Pest parser instance
    pub fn new() -> Self {
        Self
    }

    pub fn analyze(
        input: &str,
    ) -> Result<crate::complexity::ProgramComplexity, MarigoldParseError> {
        let pairs = MarigoldPestParser::parse(Rule::program, input)
            .map_err(|e| MarigoldParseError(format!("Parse error: {}", e)))?;
        let expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)
            .map_err(MarigoldParseError)?;
        Ok(crate::complexity::analyze_program(&expressions))
    }

    /// Internal function: parse input and build AST
    fn parse_input(input: &str) -> Result<String, String> {
        // Stage 1: Parse with Pest grammar
        let pairs = MarigoldPestParser::parse(Rule::program, input)
            .map_err(|e| format!("Parse error: {}", e))?;

        // Stage 2: Build AST from parse tree
        let expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)?;

        // Stage 3: Validate bounded types (if any)
        let resolved_bounds = Self::validate_bounded_types(&expressions)?;

        // Stage 4: Generate Rust code (replicating LALRPOP logic)
        Self::generate_rust_code(expressions, resolved_bounds)
    }

    fn validate_bounded_types(
        expressions: &[crate::nodes::TypedExpression],
    ) -> Result<Option<crate::bound_resolution::ResolvedBounds>, String> {
        let symbol_table = crate::symbol_table::SymbolTable::from_expressions(expressions);

        if !symbol_table.has_bounded_types() {
            return Ok(None);
        }

        let mut resolver = crate::bound_resolution::BoundResolver::new(&symbol_table);
        match resolver.resolve_all() {
            Ok(bounds) => Ok(Some(bounds)),
            Err(errors) => {
                let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
                Err(format!(
                    "Bounded type validation failed:\n  - {}",
                    error_messages.join("\n  - ")
                ))
            }
        }
    }

    /// Generate Rust code from AST expressions
    fn generate_rust_code(
        expressions: Vec<crate::nodes::TypedExpression>,
        resolved_bounds: Option<crate::bound_resolution::ResolvedBounds>,
    ) -> Result<String, String> {
        let struct_bounds = Self::build_struct_bounds_map(&resolved_bounds);
        let mut output = "async {\n    use ::marigold::marigold_impl::*;\n    ".to_string();

        // 1. Generate enums and structs
        let enums_and_structs = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StructDeclaration(s) => {
                    let field_bounds = struct_bounds.get(&s.name);
                    Some(s.code_with_bounds(field_bounds))
                }
                crate::nodes::TypedExpression::EnumDeclaration(e) => Some(e.code()),
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&enums_and_structs);

        // 2. Generate functions
        let functions = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::FnDeclaration(f) => Some(f.code()),
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&functions);

        // 3. Generate stream variable declarations
        let stream_variable_declarations = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StreamVariable(v) => Some(v.declaration_code()),
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    Some(v.declaration_code())
                }
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&stream_variable_declarations);

        // 4. Collect returning streams
        let returning_stream_vec = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::UnnamedReturningStream(s) => Some(s.code()),
                crate::nodes::TypedExpression::NamedReturningStream(s) => Some(s.code()),
                _ => None,
            })
            .collect::<Vec<_>>();

        let n_returning_streams = returning_stream_vec.len();

        // Generate returning stream variables
        output.push_str(
            &returning_stream_vec
                .iter()
                .zip(0..n_returning_streams)
                .map(|(stream_def, i)| {
                    format!("let returning_stream_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 5. Collect non-returning streams
        let non_returning_streams = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::UnnamedNonReturningStream(s) => Some(s.code()),
                crate::nodes::TypedExpression::NamedNonReturningStream(s) => Some(s.code()),
                _ => None,
            })
            .collect::<Vec<_>>();

        output.push_str(
            &non_returning_streams
                .iter()
                .enumerate()
                .map(|(i, stream_def)| {
                    format!("let non_returning_stream_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 6. Collect stream variable runners
        let stream_variable_runners = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StreamVariable(v) => Some(v.runner_code()),
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    Some(v.runner_code())
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        output.push_str(
            &stream_variable_runners
                .iter()
                .enumerate()
                .map(|(i, stream_def)| {
                    format!("let stream_variable_runners_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 7. Build streams array
        let mut streams_string = "vec![\n".to_string();

        streams_string.push_str(
            &(0..n_returning_streams)
                .map(|i| format!("returning_stream_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str(
            &(0..non_returning_streams.len())
                .map(|i| format!("non_returning_stream_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str(
            &(0..stream_variable_runners.len())
                .map(|i| format!("stream_variable_runners_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str("]\n");

        // 8. Generate stream array with type inference helper if needed
        if n_returning_streams > 0 {
            output.push_str(
                "
        /// silly function that uses generics to infer the output type (StreamItem) via generics, so that
        /// we can provide the streams as an array of Pin<Box<dyn Stream<Item=StreamItem>>>.
        #[inline(always)]
        fn typed_stream_vec<StreamItem>(v: Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>>) -> Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>> {
         v
        }
        "
            );
            output.push_str(&format!(
                "let streams_array = typed_stream_vec({streams_string});"
            ));
        } else {
            output.push_str(&format!(
                "let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = {streams_string};"
            ));
        }

        // 9. Generate select_all and collect/return
        output.push_str("let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);");

        if n_returning_streams == 0 {
            output.push_str("all_streams.collect::<Vec<()>>().await;\n");
        } else {
            output.push_str("all_streams\n");
        }

        output.push_str("}\n");

        Ok(output)
    }

    fn build_struct_bounds_map(
        resolved_bounds: &Option<crate::bound_resolution::ResolvedBounds>,
    ) -> std::collections::HashMap<
        String,
        std::collections::HashMap<String, crate::nodes::ResolvedFieldBounds>,
    > {
        let mut struct_bounds = std::collections::HashMap::new();

        if let Some(bounds) = resolved_bounds {
            for resolved in bounds.bounds() {
                struct_bounds
                    .entry(resolved.struct_name.clone())
                    .or_insert_with(std::collections::HashMap::new)
                    .insert(
                        resolved.field_name.clone(),
                        crate::nodes::ResolvedFieldBounds {
                            min: resolved.min,
                            max: resolved.max,
                        },
                    );
            }
        }

        struct_bounds
    }

    /// Parse a Marigold program string and return the generated Rust code
    pub fn parse(&self, input: &str) -> Result<String, MarigoldParseError> {
        Self::parse_input(input).map_err(MarigoldParseError)
    }

    /// Get the name of this parser (for debugging/logging purposes)
    pub fn name(&self) -> &'static str {
        "Pest"
    }
}

impl Default for PestParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Factory function that returns the default parser
pub fn get_parser() -> PestParser {
    PestParser::new()
}

/// Convenience function that uses the default parser to parse input
pub fn parse_marigold(input: &str) -> Result<String, MarigoldParseError> {
    PestParser::new().parse(input)
}
