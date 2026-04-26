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