//! Bound Expression Parser
//!
//! This module provides parsing functionality for bound expressions used in
//! bounded numerical types (`boundedInt` and `boundedUint`).
//!
//! ## Supported Syntax
//!
//! - **Literals**: Integer values like `42`, `-5`, `0`
//! - **Type References**: `TypeName.operation()` where operation is:
//!   - `len()` - Number of enum variants
//!   - `min()` - Minimum bound value
//!   - `max()` - Maximum bound value
//!   - `cardinality()` - Total count of possible values
//! - **Arithmetic**: Binary operations `+`, `-`, `*`, `/` with standard precedence
//! - **Parentheses**: Grouping for explicit precedence
//!
//! ## Examples
//!
//! ```ignore
//! use marigold_grammar::bound_expr::parse_bound_expr;
//!
//! // Simple literal
//! let expr = parse_bound_expr("42").unwrap();
//!
//! // Type reference
//! let expr = parse_bound_expr("MyEnum.len()").unwrap();
//!
//! // Arithmetic expression
//! let expr = parse_bound_expr("MyEnum.len() - 1").unwrap();
//! ```

use crate::nodes::{ArithOp, BoundExpr, BoundOp};
use std::fmt;

/// Errors that can occur when parsing bound expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundParseError {
    EmptyInput,
    InvalidLiteral(String),
    InvalidTypeReference(String),
    InvalidOperator(String),
    UnexpectedToken(String),
    UnmatchedParenthesis,
    TrailingInput(String),
}

impl fmt::Display for BoundParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundParseError::EmptyInput => write!(f, "Empty bound expression"),
            BoundParseError::InvalidLiteral(s) => write!(f, "Invalid literal: {}", s),
            BoundParseError::InvalidTypeReference(s) => write!(f, "Invalid type reference: {}", s),
            BoundParseError::InvalidOperator(s) => write!(f, "Invalid operator: {}", s),
            BoundParseError::UnexpectedToken(s) => write!(f, "Unexpected token: {}", s),
            BoundParseError::UnmatchedParenthesis => write!(f, "Unmatched parenthesis"),
            BoundParseError::TrailingInput(s) => {
                write!(f, "Trailing input after expression: {}", s)
            }
        }
    }
}

impl std::error::Error for BoundParseError {}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Number(i128),
    Identifier(String),
    Dot,
    LParen,
    RParen,
    Plus,
    Minus,
    Star,
    Slash,
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            let c = self.input[self.pos..].chars().next().unwrap();
            if c.is_whitespace() {
                self.pos += c.len_utf8();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.pos >= self.input.len() {
            return None;
        }

        let remaining = &self.input[self.pos..];
        let c = remaining.chars().next().unwrap();

        match c {
            '.' => {
                self.pos += 1;
                Some(Token::Dot)
            }
            '(' => {
                self.pos += 1;
                Some(Token::LParen)
            }
            ')' => {
                self.pos += 1;
                Some(Token::RParen)
            }
            '+' => {
                self.pos += 1;
                Some(Token::Plus)
            }
            '-' => {
                let next_char = remaining.chars().nth(1);
                if let Some(nc) = next_char {
                    if nc.is_ascii_digit() {
                        let is_start_or_after_op = self.pos == 0 || {
                            let prev = self.input[..self.pos].trim_end();
                            prev.ends_with('(')
                                || prev.ends_with('+')
                                || prev.ends_with('-')
                                || prev.ends_with('*')
                                || prev.ends_with('/')
                                || prev.ends_with(',')
                                || prev.is_empty()
                        };

                        if is_start_or_after_op {
                            return self.parse_number();
                        }
                    }
                }
                self.pos += 1;
                Some(Token::Minus)
            }
            '*' => {
                self.pos += 1;
                Some(Token::Star)
            }
            '/' => {
                self.pos += 1;
                Some(Token::Slash)
            }
            _ if c.is_ascii_digit() => self.parse_number(),
            _ if c.is_alphabetic() || c == '_' => self.parse_identifier(),
            _ => None,
        }
    }

    fn parse_number(&mut self) -> Option<Token> {
        let start = self.pos;
        let remaining = &self.input[self.pos..];
        let mut chars = remaining.chars().peekable();

        if chars.peek() == Some(&'-') {
            self.pos += 1;
            chars.next();
        }

        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() || c == '_' {
                self.pos += c.len_utf8();
                chars.next();
            } else {
                break;
            }
        }

        let num_str: String = self.input[start..self.pos]
            .chars()
            .filter(|&c| c != '_')
            .collect();
        num_str.parse::<i128>().ok().map(Token::Number)
    }

    fn parse_identifier(&mut self) -> Option<Token> {
        let start = self.pos;
        let remaining = &self.input[self.pos..];

        for c in remaining.chars() {
            if c.is_alphanumeric() || c == '_' {
                self.pos += c.len_utf8();
            } else {
                break;
            }
        }

        Some(Token::Identifier(self.input[start..self.pos].to_string()))
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn parse(&mut self) -> Result<BoundExpr, BoundParseError> {
        if self.current.is_none() {
            return Err(BoundParseError::EmptyInput);
        }

        let expr = self.parse_additive()?;

        if self.current.is_some() {
            return Err(BoundParseError::TrailingInput(format!(
                "{:?}",
                self.current
            )));
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<BoundExpr, BoundParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match &self.current {
                Some(Token::Plus) => ArithOp::Add,
                Some(Token::Minus) => ArithOp::Sub,
                _ => break,
            };

            self.advance();
            let right = self.parse_multiplicative()?;
            left = BoundExpr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<BoundExpr, BoundParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match &self.current {
                Some(Token::Star) => ArithOp::Mul,
                Some(Token::Slash) => ArithOp::Div,
                _ => break,
            };

            self.advance();
            let right = self.parse_unary()?;
            left = BoundExpr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<BoundExpr, BoundParseError> {
        if let Some(Token::Minus) = &self.current {
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(0)),
                op: ArithOp::Sub,
                right: Box::new(expr),
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<BoundExpr, BoundParseError> {
        match self.current.take() {
            Some(Token::Number(n)) => {
                self.advance();
                Ok(BoundExpr::Literal(n))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_additive()?;
                match &self.current {
                    Some(Token::RParen) => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(BoundParseError::UnmatchedParenthesis),
                }
            }
            Some(Token::Identifier(name)) => {
                self.advance();
                self.parse_type_reference(name)
            }
            Some(t) => {
                self.current = Some(t.clone());
                Err(BoundParseError::UnexpectedToken(format!("{:?}", t)))
            }
            None => Err(BoundParseError::UnexpectedToken("end of input".to_string())),
        }
    }

    fn parse_type_reference(&mut self, type_name: String) -> Result<BoundExpr, BoundParseError> {
        match &self.current {
            Some(Token::Dot) => {
                self.advance();
            }
            _ => {
                return Err(BoundParseError::InvalidTypeReference(format!(
                    "Expected '.' after type name '{}'",
                    type_name
                )));
            }
        }

        let method_name = match self.current.take() {
            Some(Token::Identifier(name)) => {
                self.advance();
                name
            }
            t => {
                self.current = t;
                return Err(BoundParseError::InvalidTypeReference(format!(
                    "Expected method name after '{}.', found {:?}",
                    type_name, self.current
                )));
            }
        };

        match &self.current {
            Some(Token::LParen) => {
                self.advance();
            }
            _ => {
                return Err(BoundParseError::InvalidTypeReference(format!(
                    "Expected '(' after '{}.{}'",
                    type_name, method_name
                )));
            }
        }

        match &self.current {
            Some(Token::RParen) => {
                self.advance();
            }
            _ => {
                return Err(BoundParseError::InvalidTypeReference(format!(
                    "Expected ')' after '{}.{}('",
                    type_name, method_name
                )));
            }
        }

        let bound_op = match method_name.as_str() {
            "len" => BoundOp::Len,
            "min" => BoundOp::Min,
            "max" => BoundOp::Max,
            "cardinality" => BoundOp::Cardinality,
            _ => {
                return Err(BoundParseError::InvalidTypeReference(format!(
                    "Unknown method '{}' on type '{}'. Valid methods are: len, min, max, cardinality",
                    method_name, type_name
                )));
            }
        };

        let type_ref = arrayvec::ArrayString::from(&type_name).map_err(|_| {
            BoundParseError::InvalidTypeReference(format!(
                "Type name '{}' is too long (max 256 characters)",
                type_name
            ))
        })?;

        Ok(BoundExpr::TypeReference(type_ref, bound_op))
    }
}

pub fn parse_bound_expr(input: &str) -> Result<BoundExpr, BoundParseError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(BoundParseError::EmptyInput);
    }

    let mut parser = Parser::new(trimmed);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_positive_literal() {
        let result = parse_bound_expr("42");
        assert_eq!(result, Ok(BoundExpr::Literal(42)));
    }

    #[test]
    fn test_parse_negative_literal() {
        let result = parse_bound_expr("-5");
        assert_eq!(result, Ok(BoundExpr::Literal(-5)));
    }

    #[test]
    fn test_parse_zero() {
        let result = parse_bound_expr("0");
        assert_eq!(result, Ok(BoundExpr::Literal(0)));
    }

    #[test]
    fn test_parse_large_number() {
        let result = parse_bound_expr("170141183460469231731687303715884105727");
        assert_eq!(
            result,
            Ok(BoundExpr::Literal(170141183460469231731687303715884105727))
        );
    }

    #[test]
    fn test_parse_type_reference_len() {
        let result = parse_bound_expr("MyEnum.len()");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("MyEnum").unwrap(),
                BoundOp::Len
            ))
        );
    }

    #[test]
    fn test_parse_type_reference_min() {
        let result = parse_bound_expr("SomeType.min()");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("SomeType").unwrap(),
                BoundOp::Min
            ))
        );
    }

    #[test]
    fn test_parse_type_reference_max() {
        let result = parse_bound_expr("SomeType.max()");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("SomeType").unwrap(),
                BoundOp::Max
            ))
        );
    }

    #[test]
    fn test_parse_type_reference_cardinality() {
        let result = parse_bound_expr("BoundedField.cardinality()");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("BoundedField").unwrap(),
                BoundOp::Cardinality
            ))
        );
    }

    #[test]
    fn test_parse_addition() {
        let result = parse_bound_expr("1 + 2");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(1)),
                op: ArithOp::Add,
                right: Box::new(BoundExpr::Literal(2)),
            })
        );
    }

    #[test]
    fn test_parse_subtraction() {
        let result = parse_bound_expr("10 - 3");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(10)),
                op: ArithOp::Sub,
                right: Box::new(BoundExpr::Literal(3)),
            })
        );
    }

    #[test]
    fn test_parse_multiplication() {
        let result = parse_bound_expr("4 * 5");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(4)),
                op: ArithOp::Mul,
                right: Box::new(BoundExpr::Literal(5)),
            })
        );
    }

    #[test]
    fn test_parse_division() {
        let result = parse_bound_expr("20 / 4");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(20)),
                op: ArithOp::Div,
                right: Box::new(BoundExpr::Literal(4)),
            })
        );
    }

    #[test]
    fn test_parse_type_reference_subtraction() {
        let result = parse_bound_expr("MyEnum.len() - 1");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::TypeReference(
                    arrayvec::ArrayString::from("MyEnum").unwrap(),
                    BoundOp::Len
                )),
                op: ArithOp::Sub,
                right: Box::new(BoundExpr::Literal(1)),
            })
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        let result = parse_bound_expr("2 + 3 * 4");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(2)),
                op: ArithOp::Add,
                right: Box::new(BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::Literal(3)),
                    op: ArithOp::Mul,
                    right: Box::new(BoundExpr::Literal(4)),
                }),
            })
        );
    }

    #[test]
    fn test_parse_parentheses() {
        let result = parse_bound_expr("(2 + 3) * 4");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::Literal(2)),
                    op: ArithOp::Add,
                    right: Box::new(BoundExpr::Literal(3)),
                }),
                op: ArithOp::Mul,
                right: Box::new(BoundExpr::Literal(4)),
            })
        );
    }

    #[test]
    fn test_parse_complex_expression() {
        let result = parse_bound_expr("MyEnum.len() * 2 - 1");
        assert_eq!(
            result,
            Ok(BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::TypeReference(
                        arrayvec::ArrayString::from("MyEnum").unwrap(),
                        BoundOp::Len
                    )),
                    op: ArithOp::Mul,
                    right: Box::new(BoundExpr::Literal(2)),
                }),
                op: ArithOp::Sub,
                right: Box::new(BoundExpr::Literal(1)),
            })
        );
    }

    #[test]
    fn test_parse_whitespace_handling() {
        let result = parse_bound_expr("  42  ");
        assert_eq!(result, Ok(BoundExpr::Literal(42)));

        let result = parse_bound_expr("  MyEnum  .  len  (  )  ");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("MyEnum").unwrap(),
                BoundOp::Len
            ))
        );
    }

    #[test]
    fn test_error_empty_input() {
        let result = parse_bound_expr("");
        assert_eq!(result, Err(BoundParseError::EmptyInput));

        let result = parse_bound_expr("   ");
        assert_eq!(result, Err(BoundParseError::EmptyInput));
    }

    #[test]
    fn test_error_invalid_type_reference() {
        let result = parse_bound_expr("MyEnum.invalid()");
        assert!(matches!(
            result,
            Err(BoundParseError::InvalidTypeReference(_))
        ));
    }

    #[test]
    fn test_error_missing_parentheses() {
        let result = parse_bound_expr("MyEnum.len");
        assert!(matches!(
            result,
            Err(BoundParseError::InvalidTypeReference(_))
        ));
    }

    #[test]
    fn test_error_unmatched_parenthesis() {
        let result = parse_bound_expr("(1 + 2");
        assert_eq!(result, Err(BoundParseError::UnmatchedParenthesis));
    }

    #[test]
    fn test_underscore_in_identifier() {
        let result = parse_bound_expr("My_Enum_Type.len()");
        assert_eq!(
            result,
            Ok(BoundExpr::TypeReference(
                arrayvec::ArrayString::from("My_Enum_Type").unwrap(),
                BoundOp::Len
            ))
        );
    }

    #[test]
    fn test_number_with_underscores() {
        let result = parse_bound_expr("1_000_000");
        assert_eq!(result, Ok(BoundExpr::Literal(1_000_000)));
    }
}
