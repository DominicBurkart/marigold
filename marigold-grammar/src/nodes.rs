use regex::Regex;
use std::str::FromStr;

const MAX_CUSTOM_TYPE_SIZE: usize = 9_999;

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
    pub inp: InputFunctionNode,
    pub funs: Vec<StreamFunctionNode>,
    pub out: OutputFunctionNode,
}

impl UnnamedStreamNode {
    pub fn code(&self) -> String {
        let inp = &self.inp.code;
        let intermediate = self
            .funs
            .iter()
            .map(|f| f.code.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let stream_prefix = &self.out.stream_prefix;
        let stream_postfix = &self.out.stream_postfix;
        format!("{{use ::marigold::marigold_impl::*; {stream_prefix}{inp}.{intermediate}{stream_postfix}}}")
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
                ".{}",
                self.funs
                    .iter()
                    .map(|f| f.code.as_str())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
        };
        let stream_prefix = &self.out.stream_prefix;
        let stream_postfix = &self.out.stream_postfix;
        format!("{{use ::marigold::marigold_impl::*; {stream_prefix}{stream_variable}.get(){intermediate}{stream_postfix}}}")
    }
}

pub struct StreamVariableNode {
    pub variable_name: String,
    pub inp: InputFunctionNode,
    pub funs: Vec<StreamFunctionNode>,
}

impl StreamVariableNode {
    pub fn declaration_code(&self) -> String {
        let variable_name = &self.variable_name;
        let inp = &self.inp.code;
        let intermediate = match self.funs.len() {
            0 => "".to_string(),
            _ => {
                format!(
                    ".{}",
                    self.funs
                        .iter()
                        .map(|f| f.code.as_str())
                        .collect::<Vec<_>>()
                        .join(".")
                )
            }
        };
        format!("let mut {variable_name} = {{use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new({inp}{intermediate})}};")
    }

    pub fn runner_code(&self) -> String {
        let variable_name = &self.variable_name;
        format!(
            "{{
                let runner_future = Box::pin({variable_name}.run());
                ::marigold::marigold_impl::multi_consumer_stream::RunFutureAsStream::new(runner_future)
            }}"
        )
    }
}

pub struct StreamVariableFromPriorStreamVariableNode {
    pub variable_name: String,
    pub prior_stream_variable: String,
    pub funs: Vec<StreamFunctionNode>,
}

impl StreamVariableFromPriorStreamVariableNode {
    pub fn declaration_code(&self) -> String {
        let variable_name = &self.variable_name;
        let prior_stream_variable = &self.prior_stream_variable;
        let intermediate = match self.funs.len() {
            0 => "".to_string(),
            _ => {
                format!(
                    ".{}",
                    self.funs
                        .iter()
                        .map(|f| f.code.as_str())
                        .collect::<Vec<_>>()
                        .join(".")
                )
            }
        };
        format!("let mut {variable_name} = {{use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new({prior_stream_variable}.get(){intermediate})}};")
    }

    pub fn runner_code(&self) -> String {
        let variable_name = &self.variable_name;
        format!(
            "{{
                let runner_future = Box::pin({variable_name}.run());
                ::marigold::marigold_impl::multi_consumer_stream::RunFutureAsStream::new(runner_future)
            }}"
        )
    }
}

pub struct StreamFunctionNode {
    pub code: String,
    // needs to include values for if the function supports parallelism, is lazy etc.
    // pub computational_complexity: String,
    // pub memory: String,
}

/// Number of inputs
pub enum InputCount {
    Known(num_bigint::BigUint),
    Unknown,
}

/// Whether the input is known at compile time (constant),
/// or whether it is not available until runtime (variable).
pub enum InputVariability {
    Constant,
    Variable,
}

pub struct InputFunctionNode {
    pub variability: InputVariability,
    pub input_count: InputCount,
    pub code: String,
}

pub struct OutputFunctionNode {
    pub stream_prefix: String,
    pub stream_postfix: String,
    pub returning: bool,
}

pub struct StructDeclarationNode {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

impl StructDeclarationNode {
    pub fn code(&self) -> String {
        #[cfg(not(feature = "io"))]
        let traits = &["Copy", "Clone", "Debug", "Eq", "PartialEq"].join(", ");
        #[cfg(feature = "io")]
        let traits = &[
            "Copy",
            "Clone",
            "Debug",
            "Eq",
            "PartialEq",
            "::marigold::marigold_impl::serde::Serialize",
            "::marigold::marigold_impl::serde::Deserialize",
        ]
        .join(", ");
        let name = &self.name;
        let mut struct_rep = format!(
            "
            #[derive({traits})]
            struct {name} {{
            "
        );
        for (field_name, field_type) in &self.fields {
            struct_rep.push_str(field_name.as_str());
            struct_rep.push_str(": ");
            struct_rep.push_str(field_type.primitive_to_type_string().as_str());
            struct_rep.push_str(",\n");
        }
        struct_rep.push('}');
        struct_rep
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    F32,
    F64,
    Bool,
    Char,
    Str(u32),
    Custom(arrayvec::ArrayString<MAX_CUSTOM_TYPE_SIZE>),
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Type, ()> {
        match s {
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
            _ => {
                lazy_static! {
                    static ref STRING: Regex = Regex::new(r"string_([0-9_A-Za-z]+)").unwrap();
                }
                if let Some(string_def) = STRING.captures(s) {
                    let size_str = string_def
                        .get(1)
                        .expect("Could not find size definition for string field");
                    let size = u32::from_str(size_str.as_str())
                        .expect("Could not parse string size in struct. Must be parsable as U32.");
                    return Ok(Type::Str(size));
                }
                Ok(Type::Custom(
                    arrayvec::ArrayString::<MAX_CUSTOM_TYPE_SIZE>::from(s)
                        .expect("type too big for Marigold"),
                ))
            }
        }
    }
}

impl Type {
    pub fn primitive_to_type_string(&self) -> String {
        match *self {
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::U128 => "u128".to_string(),
            Type::USize => "usize".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::I128 => "i128".to_string(),
            Type::ISize => "isize".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str(v) => format!("::marigold::marigold_impl::arrayvec::ArrayString<{v}>"),
            Type::Custom(v) => v.to_string(),
        }
    }
}

pub struct EnumDeclarationNode {
    pub name: String,
    pub fields: Vec<(String, Option<String>)>,
}

impl EnumDeclarationNode {
    #[allow(dead_code)] // only used if io/serde enabled
    fn definition_to_serde(maybe_definition: &Option<String>) -> Option<String> {
        if let Some(definition) = maybe_definition {
            return match definition.as_str() {
                "skip" => Some("skip".to_string()),
                _ => {
                    lazy_static! {
                        static ref QUOTED: Regex =
                            Regex::new(r#""(?P<serialization_value>[^"]+)+""#).unwrap();
                    }
                    if let Some(quoted_value) = QUOTED.captures(definition.as_str()) {
                        let value = &quoted_value["serialization_value"];
                        return Some(format!("rename = \"{value}\""));
                    }
                    panic!("Enum value could not be parsed: {}", definition);
                }
            };
        }
        None
    }

    pub fn code(&self) -> String {
        #[cfg(not(feature = "io"))]
        let traits = &["Copy", "Clone", "Debug", "Eq", "PartialEq"].join(", ");
        #[cfg(feature = "io")]
        let traits = &[
            "Copy",
            "Clone",
            "Debug",
            "Eq",
            "PartialEq",
            "::marigold::marigold_impl::serde::Serialize",
            "::marigold::marigold_impl::serde::Deserialize",
        ]
        .join(", ");
        let name = &self.name;
        let mut enum_rep = format!(
            "
            #[derive({traits})]
            enum {name} {{
            "
        );
        #[allow(unused_variables)] // serialization_definition only used if io/serde enabled
        for (field_name, serialization_definition) in &self.fields {
            #[cfg(feature = "io")]
            if let Some(serde_def) = Self::definition_to_serde(serialization_definition) {
                enum_rep.push_str(format!("#[serde({serde_def})]\n").as_str())
            }
            enum_rep.push_str(field_name.as_str());
            enum_rep.push_str(",\n");
        }
        enum_rep.push('}');
        enum_rep
    }
}

pub struct FnDeclarationNode {
    pub name: String,
    pub parameters: Vec<(String, String)>,
    pub output_type: String,
    pub body: String,
}

impl FnDeclarationNode {
    pub fn code(&self) -> String {
        let name = &self.name;
        let parameters_string = self
            .parameters
            .iter()
            .map(|(param_name, param_type)| format!("{param_name}: {param_type}"))
            .collect::<Vec<_>>()
            .join(", ");
        let output_type = &self.name;
        let body = &self.name;
        format!("const fn {name}({parameters_string}) -> {output_type} {body}")
    }
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<(String, String)>,
    pub output_type: String,
}
