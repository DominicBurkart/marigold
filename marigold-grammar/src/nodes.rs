use regex::Regex;
use std::str::FromStr;

const MAX_CUSTOM_TYPE_SIZE: usize = 9_999;
const MAX_TYPE_REFERENCE_SIZE: usize = 256;

/// A bound expression for specifying min/max bounds in bounded types.
///
/// Bound expressions can be literal values, references to other types'
/// properties (like enum lengths), or arithmetic combinations.
///
/// # Examples
///
/// - `BoundExpr::Literal(10)` - The literal value 10
/// - `BoundExpr::TypeReference("Color", BoundOp::Len)` - The number of Color variants
/// - `BoundExpr::BinaryOp { ... }` - An arithmetic expression
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum BoundExpr {
    /// A literal integer value
    Literal(i128),
    /// A reference to a type property (e.g., `Color.len()`)
    TypeReference(arrayvec::ArrayString<MAX_TYPE_REFERENCE_SIZE>, BoundOp),
    /// A binary arithmetic operation
    BinaryOp {
        left: Box<BoundExpr>,
        op: ArithOp,
        right: Box<BoundExpr>,
    },
}

/// Operations that can be performed on type references in bound expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BoundOp {
    /// Number of enum variants (or field count for bounded types)
    Len,
    /// Minimum bound value
    Min,
    /// Maximum bound value
    Max,
    /// Total count of possible values (max - min + 1)
    Cardinality,
}

/// Arithmetic operators for bound expression calculations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
}

pub enum TypedExpression {
    UnnamedReturningStream(UnnamedStreamNode), // like `range(0, 10).return`
    UnnamedNonReturningStream(UnnamedStreamNode), // like `range(0, 10).write_file("boop.csv", csv)`
    StreamVariable(StreamVariableNode),        // like `digits = range(0, 10)`
    StreamVariableFromPriorStreamVariable(StreamVariableFromPriorStreamVariableNode), // like `odds = digits.filter(is_odd)`
    NamedReturningStream(NamedStreamNode), // like `digits.return`
    NamedNonReturningStream(NamedStreamNode), // like `digits.write_file("boop.csv", csv)`
    StructDeclaration(StructDeclarationNode), // like `struct Cat { meowing: bool }`
    EnumDeclaration(EnumDeclarationNode),  // like `enum Sound { meow = "meow" }`
    FnDeclaration(FnDeclarationNode),      // like `fn foo(i: i32) -> i32 { i }`
}

impl From<UnnamedStreamNode> for TypedExpression {
    fn from(node: UnnamedStreamNode) -> Self {
        if node.out.returning {
            return TypedExpression::UnnamedReturningStream(node);
        }
        TypedExpression::UnnamedNonReturningStream(node)
    }
}

impl From<NamedStreamNode> for TypedExpression {
    fn from(node: NamedStreamNode) -> Self {
        if node.out.returning {
            return TypedExpression::NamedReturningStream(node);
        }
        TypedExpression::NamedNonReturningStream(node)
    }
}

impl From<StructDeclarationNode> for TypedExpression {
    fn from(node: StructDeclarationNode) -> Self {
        TypedExpression::StructDeclaration(node)
    }
}

impl From<EnumDeclarationNode> for TypedExpression {
    fn from(node: EnumDeclarationNode) -> Self {
        TypedExpression::EnumDeclaration(node)
    }
}

impl From<StreamVariableNode> for TypedExpression {
    fn from(node: StreamVariableNode) -> Self {
        TypedExpression::StreamVariable(node)
    }
}

impl From<StreamVariableFromPriorStreamVariableNode> for TypedExpression {
    fn from(node: StreamVariableFromPriorStreamVariableNode) -> Self {
        TypedExpression::StreamVariableFromPriorStreamVariable(node)
    }
}

impl From<FnDeclarationNode> for TypedExpression {
    fn from(node: FnDeclarationNode) -> Self {
        TypedExpression::FnDeclaration(node)
    }
}

pub struct UnnamedStreamNode {
    pub inp_and_funs: InputAndMaybeStreamFunctions,
    pub out: OutputFunctionNode,
}

impl UnnamedStreamNode {
    pub fn code(&self) -> String {
        let inp_and_funs_code = self.inp_and_funs.code();
        let stream_prefix = &self.out.stream_prefix;
        let stream_postfix = &self.out.stream_postfix;
        format!("{{use ::marigold::marigold_impl::*; {stream_prefix}{inp_and_funs_code}{stream_postfix}}}")
    }
}

pub struct InputAndMaybeStreamFunctions {
    pub inp: InputFunctionNode,
    pub funs: Vec<StreamFunctionNode>,
}

impl InputAndMaybeStreamFunctions {
    pub fn code(&self) -> String {
        let inp = &self.inp.code;
        match self.funs.len() {
            0 => inp.to_string(),
            _ => {
                let intermediate = self
                    .funs
                    .iter()
                    .map(|f| f.code.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                format!("{inp}.{intermediate}")
            }
        }
    }
}

pub struct NamedStreamNode {
    pub stream_variable: String,
    pub funs: Vec<StreamFunctionNode>,
    pub out: OutputFunctionNode,
}

impl NamedStreamNode {
    pub fn code(&self) -> String {
        let stream_variable = &self.stream_variable;
        let intermediate = match self.funs.len() {
            0 => "".to_string(),
            _ => format!(
                ".",
            ),
        };
        let stream_prefix = &self.out.stream_prefix;
        let stream_postfix = &self.out.stream_postfix;
        format!("{{use ::marigold::marigold_impl::*; {stream_prefix}{stream_variable}.get(){intermediate}{stream_postfix}}}")
    }
}
