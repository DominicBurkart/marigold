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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StreamFunctionKind {
    Map,
    Filter,
    FilterMap,
    Permutations(u64),
    PermutationsWithReplacement(u64),
    Combinations(u64),
    KeepFirstN(u64),
    Fold,
    Ok,
    OkOrPanic,
}

pub struct StreamFunctionNode {
    pub kind: StreamFunctionKind,
    pub code: String,
}

/// Number of inputs
#[derive(PartialEq, Eq, Clone)]
pub enum InputCount {
    Known(num_bigint::BigUint),
    Unknown,
}

/// Whether the input is known at compile time (constant),
/// or whether it is not available until runtime (variable).
#[derive(PartialEq, Eq, Clone)]
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

/// Resolved bounds for a struct field with a bounded type.
///
/// This struct holds the concrete min/max values after symbolic
/// expressions have been evaluated by the bound resolver.
#[derive(Debug, Clone)]
pub struct ResolvedFieldBounds {
    /// The resolved minimum bound value
    pub min: i128,
    /// The resolved maximum bound value
    pub max: i128,
}

impl StructDeclarationNode {
    pub fn code(&self) -> String {
        self.code_with_bounds(None)
    }

    pub fn code_with_bounds(
        &self,
        field_bounds: Option<&std::collections::HashMap<String, ResolvedFieldBounds>>,
    ) -> String {
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
        #[cfg(not(feature = "io"))]
        let mut struct_rep = format!(
            "
            #[derive({traits})]
            struct {name} {{
            "
        );
        #[cfg(feature = "io")]
        let mut struct_rep = format!(
            "
            #[derive({traits})]
            #[serde(crate = \"::marigold::marigold_impl::serde\")]
            struct {name} {{
            "
        );
        for (field_name, field_type) in &self.fields {
            struct_rep.push_str(field_name.as_str());
            struct_rep.push_str(": ");

            let type_str = if let Some(bounds_map) = field_bounds {
                if let Some(bounds) = bounds_map.get(field_name) {
                    field_type.primitive_to_type_string_with_resolved_bounds(
                        Some(bounds.min),
                        Some(bounds.max),
                    )
                } else {
                    field_type.primitive_to_type_string()
                }
            } else {
                field_type.primitive_to_type_string()
            };
            struct_rep.push_str(&type_str);
            struct_rep.push_str(",\n");
        }
        struct_rep.push('}');

        if let Some(bounds_map) = field_bounds {
            let bounded_fields: Vec<_> = self
                .fields
                .iter()
                .filter(|(field_name, field_type)| {
                    bounds_map.contains_key(field_name)
                        && matches!(
                            field_type,
                            Type::BoundedInt { .. } | Type::BoundedUint { .. }
                        )
                })
                .collect();

            if !bounded_fields.is_empty() {
                struct_rep.push_str(&format!("\nimpl {name} {{\n"));
                for (field_name, field_type) in bounded_fields {
                    if let Some(bounds) = bounds_map.get(field_name) {
                        let upper_field = field_name.to_uppercase();
                        let cardinality = (bounds.max - bounds.min + 1) as u128;
                        let field_type_str = field_type
                            .primitive_to_type_string_with_resolved_bounds(
                                Some(bounds.min),
                                Some(bounds.max),
                            );
                        let card_type = select_smallest_unsigned_type(cardinality);
                        struct_rep.push_str(&format!(
                            "    pub const {upper_field}_MIN: {field_type_str} = {};\n",
                            bounds.min
                        ));
                        struct_rep.push_str(&format!(
                            "    pub const {upper_field}_MAX: {field_type_str} = {};\n",
                            bounds.max
                        ));
                        struct_rep.push_str(&format!(
                            "    pub const {upper_field}_CARDINALITY: {card_type} = {cardinality};\n"
                        ));
                    }
                }
                struct_rep.push('}');
            }
        }

        struct_rep
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    Option(Box<Type>),
    BoundedInt { min: BoundExpr, max: BoundExpr },
    BoundedUint { min: BoundExpr, max: BoundExpr },
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Type, ()> {
        let s = s.trim();
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
                    static ref OPTIONAL: Regex =
                        Regex::new(r"Option[\s]*<[\s]*(.+?)[\s]*>").unwrap();
                    static ref STRING: Regex = Regex::new(r"string_([0-9_A-Za-z]+)").unwrap();
                }

                if let Some(optional_def) = OPTIONAL.captures(s) {
                    if let Ok(internal_type) = Type::from_str(
                        optional_def
                            .get(1)
                            .expect("Could not get internal type from Option")
                            .as_str(),
                    ) {
                        return Ok(Type::Option(Box::new(internal_type)));
                    }
                } else if let Some(string_def) = STRING.captures(s) {
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
        match &self {
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
            Type::Option(internal_type) => {
                let internal_type_string = internal_type.primitive_to_type_string();
                format!("Option<{internal_type_string}>")
            }
            Type::Custom(v) => v.to_string(),
            Type::BoundedInt { .. } => "i128".to_string(),
            Type::BoundedUint { .. } => "u128".to_string(),
        }
    }

    pub fn primitive_to_type_string_with_resolved_bounds(
        &self,
        resolved_min: Option<i128>,
        resolved_max: Option<i128>,
    ) -> String {
        match &self {
            Type::BoundedInt { .. } => {
                if let (Some(min), Some(max)) = (resolved_min, resolved_max) {
                    if min >= 0 {
                        select_smallest_unsigned_type(max as u128)
                    } else {
                        select_smallest_signed_type(min, max)
                    }
                } else {
                    "i128".to_string()
                }
            }
            Type::BoundedUint { .. } => {
                if let (Some(min), Some(max)) = (resolved_min, resolved_max) {
                    if min >= 0 && max >= 0 {
                        select_smallest_unsigned_type(max as u128)
                    } else {
                        "u128".to_string()
                    }
                } else {
                    "u128".to_string()
                }
            }
            _ => self.primitive_to_type_string(),
        }
    }
}

fn select_smallest_signed_type(min: i128, max: i128) -> String {
    if min >= i8::MIN as i128 && max <= i8::MAX as i128 {
        "i8".to_string()
    } else if min >= i16::MIN as i128 && max <= i16::MAX as i128 {
        "i16".to_string()
    } else if min >= i32::MIN as i128 && max <= i32::MAX as i128 {
        "i32".to_string()
    } else if min >= i64::MIN as i128 && max <= i64::MAX as i128 {
        "i64".to_string()
    } else {
        "i128".to_string()
    }
}

fn select_smallest_unsigned_type(max: u128) -> String {
    if max <= u8::MAX as u128 {
        "u8".to_string()
    } else if max <= u16::MAX as u128 {
        "u16".to_string()
    } else if max <= u32::MAX as u128 {
        "u32".to_string()
    } else if max <= u64::MAX as u128 {
        "u64".to_string()
    } else {
        "u128".to_string()
    }
}

pub struct EnumDeclarationNode {
    pub name: String,
    pub variants: Vec<(String, Option<String>)>,
    pub default_variant: Option<DefaultEnumVariant>,
}

pub enum DefaultEnumVariant {
    Sized(
        String, // name
        u32,    // size
    ),
    WithDefaultValue(
        String, // name
        String, // serialized value
    ),
}

impl EnumDeclarationNode {
    #[cfg(feature = "io")]
    fn definition_to_serde(maybe_definition: &Option<String>) -> Option<String> {
        if let Some(definition) = maybe_definition {
            return match definition.as_str() {
                "skip" => Some("skip".to_string()),
                value => Some(format!("rename = \"{value}\"")),
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
        let mut enum_rep = format!("#[derive({traits})]");
        #[cfg(feature = "io")]
        enum_rep.push_str("\n#[serde(crate = \"::marigold::marigold_impl::serde\")]");
        if let Some(default_variant) = &self.default_variant {
            #[cfg(feature = "io")]
            {
                enum_rep.push_str("\n#[serde(try_from=\"String\")]");
                enum_rep.push_str(format!("\nenum {name} {{\n").as_str());

                let mut serialized_to_name_mapping = String::new();
                for (field_name, serialization_definition) in &self.variants {
                    if let Some(s) = Self::definition_to_serde(serialization_definition) {
                        enum_rep.push_str(format!("#[serde({s})]\n").as_str());
                    }
                    enum_rep.push_str(format!("{},\n", field_name.as_str()).as_str());

                    let serialized = serialization_definition
                        .as_ref()
                        .unwrap_or(field_name)
                        .clone();
                    serialized_to_name_mapping
                        .push_str(format!("\"{serialized}\" => {field_name},\n").as_str());
                }

                #[allow(unused_assignments)]
                let mut default_serialized_mapping = String::new();
                match default_variant {
                    DefaultEnumVariant::Sized(default_name, size) => {
                        enum_rep.push_str("#[serde(skip_deserializing)]\n");
                        enum_rep.push_str(format!("{default_name}(::marigold::marigold_impl::arrayvec::ArrayString<{size}>),\n").as_str());

                        default_serialized_mapping =
                            "unknown_value => Other({{
                                let mut contents = ::marigold::marigold_impl::arrayvec::ArrayString::new();
                                for c in unknown_value.chars() {{
                                    contents.try_push(c)?;
                                }}
                                contents
                            }})".to_string();
                    }
                    DefaultEnumVariant::WithDefaultValue(default_name, serialized_value) => {
                        enum_rep.push_str(format!("#[serde(skip_deserializing, rename = \"{serialized_value}\")]\n{default_name},\n").as_str());

                        default_serialized_mapping = format!("_ => {default_name}");
                    }
                }
                enum_rep.push('}');
                enum_rep.push_str(
                    format!(
                        r#"

                impl TryFrom<String> for {name} {{
                    type Error = ::marigold::marigold_impl::arrayvec::CapacityError<char>;

                    fn try_from(s: String) -> Result<Self, Self::Error> {{
                        use {name}::*;

                        Ok(match s.as_str() {{
                            {serialized_to_name_mapping}
                            {default_serialized_mapping}
                        }})
                    }}
                }}

                "#
                    )
                    .as_str(),
                );
            }
            #[cfg(not(feature = "io"))]
            {
                enum_rep.push_str(format!("enum {name} {{\n").as_str());

                for (field_name, _) in &self.variants {
                    enum_rep.push_str(field_name.as_str());
                    enum_rep.push_str(",\n");
                }
                match default_variant {
                    DefaultEnumVariant::Sized(name, size) => {
                        enum_rep.push_str(format!("{name}(::marigold::marigold_impl::arrayvec::ArrayString<{size}>),\n").as_str());
                    }
                    DefaultEnumVariant::WithDefaultValue(name, _) => {
                        enum_rep.push_str(format!("{name},\n").as_str());
                    }
                };
                enum_rep.push('}');
            }
        } else {
            enum_rep.push_str(format!("enum {name} {{\n").as_str());

            #[allow(unused_variables)] // serialization_definition only used if io/serde enabled
            for (field_name, serialization_definition) in &self.variants {
                #[cfg(feature = "io")]
                if let Some(serde_def) = Self::definition_to_serde(serialization_definition) {
                    enum_rep.push_str(format!("#[serde({serde_def})]\n").as_str());
                }
                enum_rep.push_str(field_name.as_str());
                enum_rep.push_str(",\n");
            }

            enum_rep.push('}');
        }
        enum_rep
    }
}

pub fn parse_enum(enum_name: String, enum_contents: String) -> TypedExpression {
    lazy_static! {
        static ref ENUM_RE: Regex = Regex::new(r#"\{[\s]*(?P<variant>(?P<variant_name>[\w]+)[\s]*=[\s]*("(?P<serialized_value>[^"]+)"),?[\s]*)*(?P<default_variant>default (?P<default_variant_name>[\w]+)(\(string_(?P<default_variant_string_size>[\d]+)\))?[\s]*(=[\s]*"(?P<default_variant_serialized_value>[^"]+)")?,?[\s]*)?\}"#).unwrap();
        static ref VARIANT_RE: Regex = Regex::new(r#"(?P<variant_name>[\w]+)[\s]*=[\s]*("(?P<serialized_value>[^"]+)"),?"#).unwrap();
        static ref DEFAULT_VARIANT_RE: Regex = Regex::new(r#"default (?P<default_variant_name>[\w]+)(\(string_(?P<default_variant_string_size>[\d]+)\))?[\s]*(=[\s]*"(?P<default_variant_serialized_value>[^"]+)")?,?[\s]*"#).unwrap();
    }

    fn get_variants(enum_str: &str) -> Vec<(String, Option<String>)> {
        VARIANT_RE
            .captures_iter(enum_str)
            .map(|c| {
                (
                    c.name("variant_name").unwrap().as_str().to_string(),
                    c.name("serialized_value").map(|v| v.as_str().to_string()),
                )
            })
            .collect::<Vec<_>>()
    }

    // first, validate that the enum matches the expected format
    let cap = ENUM_RE.captures(&enum_contents);
    if cap.is_none() {
        panic!("syntax error while parsing enum {}", enum_name);
    }

    // find if there is a default variant, checking if there are multiple
    // defaults declared.
    if let Some((default_variant_name, maybe_string_size, maybe_serialized_value)) = {
        let mut default_variants = DEFAULT_VARIANT_RE
            .captures_iter(&enum_contents)
            .filter(|v| v.name("default_variant_name").is_some());

        let mut name = None;
        let mut maybe_string_size = None;
        let mut maybe_serialized_value = None;
        if let Some(default_variant) = default_variants.next() {
            name = Some(
                default_variant
                    .name("default_variant_name")
                    .unwrap()
                    .as_str()
                    .to_string(),
            );
            maybe_string_size = default_variant
                .name("default_variant_string_size")
                .map(|m| {
                    u32::from_str(m.as_str()).expect("could not parse default enum size as usize")
                });
            maybe_serialized_value = default_variant
                .name("default_variant_serialized_value")
                .map(|m| m.as_str().to_string());
        }

        if default_variants.next().is_some() {
            panic!("Multiple default variants declared in enum {}", enum_name);
        }

        name.map(|n| (n, maybe_string_size, maybe_serialized_value))
    } {
        if maybe_string_size.is_some() && maybe_serialized_value.is_some() {
            panic!("Error while parsing enum {}: default enum value can contain a serialized value or a string size, but not both", enum_name);
        }

        TypedExpression::from(EnumDeclarationNode {
            name: enum_name,
            variants: get_variants(
                &enum_contents[0..DEFAULT_VARIANT_RE.find(&enum_contents).unwrap().start()],
            ),
            default_variant: Some({
                if let Some(string_size) = maybe_string_size {
                    DefaultEnumVariant::Sized(default_variant_name, string_size)
                } else if let Some(serialized_value) = maybe_serialized_value {
                    DefaultEnumVariant::WithDefaultValue(default_variant_name, serialized_value)
                } else {
                    DefaultEnumVariant::WithDefaultValue(
                        default_variant_name.clone(),
                        default_variant_name,
                    )
                }
            }),
        })
    } else {
        // no default variant, this is simpler :)
        TypedExpression::from(EnumDeclarationNode {
            name: enum_name,
            variants: get_variants(&enum_contents),
            default_variant: None,
        })
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
        let output_type = &self.output_type;
        let body = &self.body;

        format!("const fn {name}({parameters_string}) -> {output_type} {{{body}}}")
    }
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<(String, String)>,
    pub output_type: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_primitive_to_string_bounded_int() {
        let t = Type::BoundedInt {
            min: BoundExpr::Literal(0),
            max: BoundExpr::Literal(100),
        };
        assert_eq!(t.primitive_to_type_string(), "i128");
    }

    #[test]
    fn test_type_primitive_to_string_with_resolved_bounds_nonneg_int_uses_unsigned() {
        let t = Type::BoundedInt {
            min: BoundExpr::Literal(0),
            max: BoundExpr::Literal(100),
        };
        assert_eq!(
            t.primitive_to_type_string_with_resolved_bounds(Some(0), Some(100)),
            "u8"
        );
    }

    #[test]
    fn test_type_primitive_to_string_with_resolved_bounds_i16() {
        let t = Type::BoundedInt {
            min: BoundExpr::Literal(-1000),
            max: BoundExpr::Literal(1000),
        };
        assert_eq!(
            t.primitive_to_type_string_with_resolved_bounds(Some(-1000), Some(1000)),
            "i16"
        );
    }

    #[test]
    fn test_type_primitive_to_string_with_resolved_bounds_u8() {
        let t = Type::BoundedUint {
            min: BoundExpr::Literal(0),
            max: BoundExpr::Literal(200),
        };
        assert_eq!(
            t.primitive_to_type_string_with_resolved_bounds(Some(0), Some(200)),
            "u8"
        );
    }

    #[test]
    fn test_select_smallest_signed_type() {
        assert_eq!(select_smallest_signed_type(-128, 127), "i8");
        assert_eq!(select_smallest_signed_type(-129, 127), "i16");
        assert_eq!(select_smallest_signed_type(-32768, 32767), "i16");
        assert_eq!(select_smallest_signed_type(-32769, 32767), "i32");
    }

    #[test]
    fn test_select_smallest_unsigned_type() {
        assert_eq!(select_smallest_unsigned_type(255), "u8");
        assert_eq!(select_smallest_unsigned_type(256), "u16");
        assert_eq!(select_smallest_unsigned_type(65535), "u16");
        assert_eq!(select_smallest_unsigned_type(65536), "u32");
    }
}
