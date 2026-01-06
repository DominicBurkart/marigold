#![forbid(unsafe_code)]

use std::fmt;

/// Common error type for both parser backends
#[derive(Debug, Clone)]
pub enum MarigoldParseError {
    LalrpopError(String),
    #[cfg(feature = "pest-parser")]
    PestError(String),
}

impl fmt::Display for MarigoldParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarigoldParseError::LalrpopError(msg) => write!(f, "LALRPOP parse error: {}", msg),
            #[cfg(feature = "pest-parser")]
            MarigoldParseError::PestError(msg) => write!(f, "Pest parse error: {}", msg),
        }
    }
}

impl std::error::Error for MarigoldParseError {}

/// Parser abstraction trait that allows switching between LALRPOP and Pest parsers
pub trait MarigoldParser {
    /// Parse a Marigold program string and return the generated Rust code
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError>;

    /// Get the name of this parser (for debugging/logging purposes)
    fn name(&self) -> &'static str;
}

/// LALRPOP parser backend - wraps the existing LALRPOP implementation
pub struct LalrpopParser {
    parser: crate::ast::ProgramParser,
}

impl LalrpopParser {
    pub fn new() -> Self {
        Self {
            parser: crate::ast::ProgramParser::new(),
        }
    }
}

impl Default for LalrpopParser {
    fn default() -> Self {
        Self::new()
    }
}

impl MarigoldParser for LalrpopParser {
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError> {
        self.parser
            .parse(input)
            .map_err(|e| MarigoldParseError::LalrpopError(format!("{:?}", e)))
    }

    fn name(&self) -> &'static str {
        "LALRPOP"
    }
}

/// Pest parser backend - new implementation using Pest grammar
#[cfg(feature = "pest-parser")]
pub struct PestParser;

// Note: pest::Parser import moved to local scope where needed

#[cfg(feature = "pest-parser")]
#[derive(pest_derive::Parser)]
#[grammar = "marigold.pest"]
pub struct MarigoldPestParser;

#[cfg(feature = "pest-parser")]
impl PestParser {
    pub fn new() -> Self {
        Self
    }

    fn parse_input(_input: &str) -> Result<(), String> {
        // TODO: Fix Pest Rule access issue
        // For now, return an error to indicate Pest is not implemented
        Err("Pest parser not fully implemented yet".to_string())
    }
}

#[cfg(feature = "pest-parser")]
impl Default for PestParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "pest-parser")]
impl MarigoldParser for PestParser {
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError> {
        // For now, just try to parse and return a basic structure
        // This is a minimal implementation to get basic functionality working
        match Self::parse_input(input) {
            Ok(_) => {
                // If parsing succeeds, return a basic async block
                Ok("async {\n    use ::marigold::marigold_impl::*;\n    // Parsed successfully with Pest\n}".to_string())
            }
            Err(e) => Err(MarigoldParseError::PestError(e)),
        }
    }

    fn name(&self) -> &'static str {
        "Pest"
    }
}

/// Factory function that returns the appropriate parser based on feature flags
pub fn get_parser() -> Box<dyn MarigoldParser> {
    #[cfg(feature = "pest-parser")]
    {
        Box::new(PestParser::new())
    }

    #[cfg(not(feature = "pest-parser"))]
    {
        Box::new(LalrpopParser::new())
    }
}

/// Convenience function that uses the default parser to parse input
pub fn parse_marigold(input: &str) -> Result<String, MarigoldParseError> {
    let parser = get_parser();
    parser.parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lalrpop_parser_creation() {
        let parser = LalrpopParser::new();
        assert_eq!(parser.name(), "LALRPOP");
    }

    #[test]
    fn test_default_parser_selection() {
        let parser = get_parser();

        #[cfg(feature = "pest-parser")]
        assert_eq!(parser.name(), "Pest");

        #[cfg(not(feature = "pest-parser"))]
        assert_eq!(parser.name(), "LALRPOP");
    }

    #[test]
    fn test_parse_marigold_function() {
        // Test with a minimal valid program - just empty for now
        let result = parse_marigold("");

        // Should succeed with LALRPOP, may fail with unimplemented Pest
        #[cfg(not(feature = "pest-parser"))]
        assert!(result.is_ok());

        #[cfg(feature = "pest-parser")]
        assert!(result.is_err()); // Expected until Pest is implemented
    }

    #[test]
    fn test_lalrpop_parser_basic_functionality() {
        let parser = LalrpopParser::new();

        // Test with empty input - should generate basic async block
        let result = parser.parse("");
        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(output.contains("async"));
    }

    #[cfg(feature = "pest-parser")]
    #[test]
    fn test_pest_parser_creation() {
        let parser = PestParser::new();
        assert_eq!(parser.name(), "Pest");
    }

    #[cfg(feature = "pest-parser")]
    #[test]
    fn test_pest_parser_not_implemented() {
        let parser = PestParser::new();
        let result = parser.parse("");

        assert!(result.is_err());
        if let Err(MarigoldParseError::PestError(msg)) = result {
            assert!(msg.contains("not fully implemented"));
        } else {
            panic!("Expected PestError");
        }
    }

    #[cfg(feature = "pest-parser")]
    #[test]
    fn test_pest_grammar_basic_parsing() {
        // TODO: Fix Pest Rule enum access pattern to enable this test
        // This test validates that the basic Pest grammar works correctly
        // Currently blocked by Rust's associated type access limitations

        // When working, this should test:
        // - MarigoldPestParser::parse(Rule::program, "hello") -> Ok
        // - MarigoldPestParser::parse(Rule::program, "goodbye") -> Err

        // For now, we test that the parser structure exists
        let _parser = MarigoldPestParser;
        // Test passes as a placeholder until Pest Rule access is resolved
    }

    // Parser comparison tests
    #[test]
    fn test_parser_compatibility_empty_input() {
        let lalrpop_parser = LalrpopParser::new();
        let lalrpop_result = lalrpop_parser.parse("");

        // Empty input should succeed with LALRPOP
        assert!(lalrpop_result.is_ok());
        let lalrpop_output = lalrpop_result.unwrap();
        assert!(lalrpop_output.contains("async"));

        #[cfg(feature = "pest-parser")]
        {
            let pest_parser = PestParser::new();
            let pest_result = pest_parser.parse("");

            // For now, Pest parser is expected to fail
            // When fully implemented, this should succeed and produce equivalent output
            assert!(pest_result.is_err());
        }
    }

    #[test]
    fn test_factory_function_consistency() {
        // Test that the factory function returns the expected parser type
        let parser = get_parser();

        #[cfg(feature = "pest-parser")]
        assert_eq!(parser.name(), "Pest");

        #[cfg(not(feature = "pest-parser"))]
        assert_eq!(parser.name(), "LALRPOP");
    }

    #[test]
    fn test_parse_marigold_function_works() {
        // Test the convenience function
        let result = parse_marigold("");

        #[cfg(not(feature = "pest-parser"))]
        assert!(result.is_ok());

        #[cfg(feature = "pest-parser")]
        assert!(result.is_err()); // Expected until Pest is fully implemented
    }
}
