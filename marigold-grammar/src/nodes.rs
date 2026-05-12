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
    /// Variant count for `range(EnumName)` — resolved to `Known` after symbol table lookup.
    Enum(String),
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

        let mut unit_variants: Vec<String> = self.variants.iter().map(|(n, _)| n.clone()).collect();
        match &self.default_variant {
            Some(DefaultEnumVariant::WithDefaultValue(default_name, _)) => {
                unit_variants.push(default_name.clone());
            }
            Some(DefaultEnumVariant::Sized(_, _)) | None => {}
        }
        let variant_count = unit_variants.len();
        let variants_list = unit_variants
            .iter()
            .map(|v| format!("{name}::{v}"))
            .collect::<Vec<_>>()
            .join(", ");
        enum_rep.push_str(&format!(
            "\n#[allow(dead_code)]\nimpl {name} {{\n    fn __marigold_variants() -> [{name}; {variant_count}] \
             {{\n        [{variants_list}]\n    }}\n}}"
        ));

        enum_rep
    }

    /// Returns the number of unit variants included in `__marigold_variants()`.
    /// This excludes `Sized` default variants (they cannot be copy-constructed).
    pub fn unit_variant_count(&self) -> usize {
        let base = self.variants.len();
        match &self.default_variant {
            Some(DefaultEnumVariant::WithDefaultValue(_, _)) => base + 1,
            Some(DefaultEnumVariant::Sized(_, _)) | None => base,
        }
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

    // ------------------------------------------------------------------
    // Code-generation invariants for the AST node `code()` methods.
    //
    // The `code()` family is the bridge from the parsed AST to the Rust
    // source that the `m!` macro expands to. Before this commit none of
    // the per-node code emitters were unit-tested in isolation; bugs were
    // only surfaced through end-to-end macro expansion, which makes
    // diagnosis expensive and lets regressions in code shape slip in.
    //
    // The tests below pin the *shape* of the emitted code (block
    // wrappers, dot-chaining, `let mut` declarations, trait derives,
    // `__marigold_variants()` helpers, etc.). They are intentionally
    // assertion-substring-based rather than full string-equality so that
    // formatting tweaks (whitespace) don't break them — only structural
    // regressions do.
    //
    // Invariants exercised here:
    //
    //   A. **Block wrapper.** Every stream-emitting node's `code()` is
    //      wrapped in `{use ::marigold::marigold_impl::*; ... }` so that
    //      the generated Rust resolves the marigold trait imports.
    //   B. **Dot-chaining.** Stream functions are joined with `.` after
    //      the input, with no leading/trailing dots when the list is
    //      empty.
    //   C. **`TypedExpression::From` dispatch.** Returning/non-returning
    //      stream nodes are dispatched to the right variant based on
    //      `out.returning`. (No data is lost.)
    //   D. **Variable declarations.** `StreamVariableNode::declaration_code`
    //      and `StreamVariableFromPriorStreamVariableNode::declaration_code`
    //      always emit `let mut <name> = ...` so that downstream
    //      runner_code can mutate the value.
    //   E. **Runner code.** `runner_code()` always references the
    //      variable it was built for and wraps its `.run()` in a
    //      `RunFutureAsStream`.
    //   F. **Struct/enum codegen.** The emitted decls always carry the
    //      core derives (`Clone`, `Debug`, `Eq`, `PartialEq`) and the
    //      enum codegen always emits the `__marigold_variants()`
    //      reflection helper.
    //   G. **Enum `unit_variant_count`.** A `WithDefaultValue` default
    //      variant is counted in `__marigold_variants()`; a `Sized`
    //      default variant is not (it cannot be `Copy`-constructed).
    //   H. **Fn codegen.** `FnDeclarationNode::code` emits `const fn` so
    //      that the body can be evaluated at macro/compile time.

    fn dummy_input(code: &str) -> InputFunctionNode {
        InputFunctionNode {
            variability: InputVariability::Constant,
            input_count: InputCount::Known(num_bigint::BigUint::from(0u32)),
            code: code.to_string(),
        }
    }

    fn dummy_fn(code: &str) -> StreamFunctionNode {
        StreamFunctionNode {
            kind: StreamFunctionKind::Map,
            code: code.to_string(),
        }
    }

    fn dummy_output(prefix: &str, postfix: &str, returning: bool) -> OutputFunctionNode {
        OutputFunctionNode {
            stream_prefix: prefix.to_string(),
            stream_postfix: postfix.to_string(),
            returning,
        }
    }

    // ---- InputAndMaybeStreamFunctions::code --------------------------

    #[test]
    fn input_and_funs_code_no_funs_is_just_input() {
        // Invariant B: with zero stream funs, the rendered code is just
        // the input's code (no trailing dot).
        let n = InputAndMaybeStreamFunctions {
            inp: dummy_input("range(0, 10)"),
            funs: vec![],
        };
        assert_eq!(n.code(), "range(0, 10)");
    }

    #[test]
    fn input_and_funs_code_single_fun_dot_joined() {
        // Invariant B: a single fun appears as `input.fun`.
        let n = InputAndMaybeStreamFunctions {
            inp: dummy_input("range(0, 10)"),
            funs: vec![dummy_fn("filter(is_odd)")],
        };
        assert_eq!(n.code(), "range(0, 10).filter(is_odd)");
    }

    #[test]
    fn input_and_funs_code_multiple_funs_dot_joined_in_order() {
        // Invariant B: multiple funs appear in source order, all joined
        // by `.`.
        let n = InputAndMaybeStreamFunctions {
            inp: dummy_input("range(0, 10)"),
            funs: vec![
                dummy_fn("filter(is_odd)"),
                dummy_fn("map(square)"),
                dummy_fn("keep_first_n(2, asc)"),
            ],
        };
        assert_eq!(
            n.code(),
            "range(0, 10).filter(is_odd).map(square).keep_first_n(2, asc)"
        );
    }

    // ---- UnnamedStreamNode::code -------------------------------------

    #[test]
    fn unnamed_stream_code_wraps_with_marigold_use_block() {
        // Invariants A + B: the whole emission is wrapped in
        // `{use ::marigold::marigold_impl::*; ... }`, and prefix/postfix
        // surround the input+funs.
        let n = UnnamedStreamNode {
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: dummy_input("range(0, 3)"),
                funs: vec![dummy_fn("filter(is_odd)")],
            },
            out: dummy_output("PREFIX_", "_POSTFIX", true),
        };
        let code = n.code();
        assert!(
            code.starts_with("{use ::marigold::marigold_impl::*;"),
            "missing marigold use block; got: {code}"
        );
        assert!(code.ends_with('}'), "must close brace; got: {code}");
        assert!(code.contains("PREFIX_range(0, 3).filter(is_odd)_POSTFIX"));
    }

    // ---- TypedExpression::From dispatch ------------------------------

    #[test]
    fn typed_expression_from_unnamed_dispatches_on_returning() {
        // Invariant C: `returning = true` -> UnnamedReturningStream;
        // `returning = false` -> UnnamedNonReturningStream.
        let returning = UnnamedStreamNode {
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: dummy_input("range(0, 1)"),
                funs: vec![],
            },
            out: dummy_output("", "", true),
        };
        assert!(matches!(
            TypedExpression::from(returning),
            TypedExpression::UnnamedReturningStream(_)
        ));

        let non_returning = UnnamedStreamNode {
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: dummy_input("range(0, 1)"),
                funs: vec![],
            },
            out: dummy_output("", "", false),
        };
        assert!(matches!(
            TypedExpression::from(non_returning),
            TypedExpression::UnnamedNonReturningStream(_)
        ));
    }

    #[test]
    fn typed_expression_from_named_dispatches_on_returning() {
        // Invariant C: same dispatch behaviour for NamedStreamNode.
        let returning = NamedStreamNode {
            stream_variable: "s".to_string(),
            funs: vec![],
            out: dummy_output("", "", true),
        };
        assert!(matches!(
            TypedExpression::from(returning),
            TypedExpression::NamedReturningStream(_)
        ));

        let non_returning = NamedStreamNode {
            stream_variable: "s".to_string(),
            funs: vec![],
            out: dummy_output("", "", false),
        };
        assert!(matches!(
            TypedExpression::from(non_returning),
            TypedExpression::NamedNonReturningStream(_)
        ));
    }

    // ---- NamedStreamNode::code ---------------------------------------

    #[test]
    fn named_stream_code_with_no_funs_calls_get_without_extra_dot() {
        // Invariant B: an empty fun list yields `<var>.get()` exactly —
        // no spurious trailing dot.
        let n = NamedStreamNode {
            stream_variable: "digits".to_string(),
            funs: vec![],
            out: dummy_output("", "", true),
        };
        let code = n.code();
        assert!(code.contains("digits.get()"), "got: {code}");
        // Make sure we did not emit `digits.get().` with nothing after.
        assert!(!code.contains("digits.get()."), "got: {code}");
    }

    #[test]
    fn named_stream_code_with_funs_dot_joins_after_get() {
        // Invariant B: funs are chained off `.get()`.
        let n = NamedStreamNode {
            stream_variable: "digits".to_string(),
            funs: vec![dummy_fn("map(double)"), dummy_fn("filter(positive)")],
            out: dummy_output("", "", true),
        };
        let code = n.code();
        assert!(
            code.contains("digits.get().map(double).filter(positive)"),
            "got: {code}"
        );
    }

    #[test]
    fn named_stream_code_wraps_with_marigold_use_block() {
        // Invariant A.
        let n = NamedStreamNode {
            stream_variable: "s".to_string(),
            funs: vec![],
            out: dummy_output("", "", true),
        };
        let code = n.code();
        assert!(code.starts_with("{use ::marigold::marigold_impl::*;"));
        assert!(code.ends_with('}'));
    }

    // ---- StreamVariableNode codegen ----------------------------------

    #[test]
    fn stream_variable_declaration_emits_let_mut_and_multi_consumer_stream() {
        // Invariant D: declaration_code emits `let mut <name> = ...`
        // wrapping a MultiConsumerStream::new(...).
        let n = StreamVariableNode {
            variable_name: "digits".to_string(),
            inp: dummy_input("range(0, 10)"),
            funs: vec![],
        };
        let code = n.declaration_code();
        assert!(code.contains("let mut digits = "), "got: {code}");
        assert!(
            code.contains("MultiConsumerStream::new(range(0, 10))"),
            "got: {code}"
        );
        assert!(code.ends_with(';'), "decl must end with `;`; got: {code}");
    }

    #[test]
    fn stream_variable_declaration_with_funs_dot_joins() {
        // Invariant B + D.
        let n = StreamVariableNode {
            variable_name: "evens".to_string(),
            inp: dummy_input("range(0, 10)"),
            funs: vec![dummy_fn("filter(is_even)")],
        };
        let code = n.declaration_code();
        assert!(
            code.contains("MultiConsumerStream::new(range(0, 10).filter(is_even))"),
            "got: {code}"
        );
    }

    #[test]
    fn stream_variable_runner_code_references_variable_and_runs() {
        // Invariant E.
        let n = StreamVariableNode {
            variable_name: "digits".to_string(),
            inp: dummy_input("range(0, 1)"),
            funs: vec![],
        };
        let code = n.runner_code();
        assert!(code.contains("digits.run()"), "got: {code}");
        assert!(code.contains("RunFutureAsStream::new"), "got: {code}");
    }

    // ---- StreamVariableFromPriorStreamVariableNode codegen ----------

    #[test]
    fn stream_variable_from_prior_declaration_uses_prior_get() {
        // Invariant D: declaration must reference `prior.get()`.
        let n = StreamVariableFromPriorStreamVariableNode {
            variable_name: "odds".to_string(),
            prior_stream_variable: "digits".to_string(),
            funs: vec![dummy_fn("filter(is_odd)")],
        };
        let code = n.declaration_code();
        assert!(code.contains("let mut odds = "), "got: {code}");
        assert!(
            code.contains("MultiConsumerStream::new(digits.get().filter(is_odd))"),
            "got: {code}"
        );
    }

    #[test]
    fn stream_variable_from_prior_declaration_no_funs_no_extra_dot() {
        // Invariant B: no funs -> no spurious trailing dot.
        let n = StreamVariableFromPriorStreamVariableNode {
            variable_name: "alias".to_string(),
            prior_stream_variable: "digits".to_string(),
            funs: vec![],
        };
        let code = n.declaration_code();
        assert!(
            code.contains("MultiConsumerStream::new(digits.get())"),
            "got: {code}"
        );
        // No trailing dot after .get().
        assert!(
            !code.contains("digits.get()."),
            "must not emit dangling dot; got: {code}"
        );
    }

    #[test]
    fn stream_variable_from_prior_runner_code_references_self_variable() {
        // Invariant E.
        let n = StreamVariableFromPriorStreamVariableNode {
            variable_name: "odds".to_string(),
            prior_stream_variable: "digits".to_string(),
            funs: vec![],
        };
        let code = n.runner_code();
        assert!(code.contains("odds.run()"), "got: {code}");
        assert!(code.contains("RunFutureAsStream::new"), "got: {code}");
    }

    // ---- FnDeclarationNode::code -------------------------------------

    #[test]
    fn fn_declaration_emits_const_fn_with_params_and_body() {
        // Invariant H: const fn so the body is usable in const contexts.
        let n = FnDeclarationNode {
            name: "is_odd".to_string(),
            parameters: vec![("i".to_string(), "&i32".to_string())],
            output_type: "bool".to_string(),
            body: "i.wrapping_rem(2) == 1".to_string(),
        };
        assert_eq!(
            n.code(),
            "const fn is_odd(i: &i32) -> bool {i.wrapping_rem(2) == 1}"
        );
    }

    #[test]
    fn fn_declaration_with_multiple_parameters_comma_separated() {
        let n = FnDeclarationNode {
            name: "add".to_string(),
            parameters: vec![
                ("a".to_string(), "i32".to_string()),
                ("b".to_string(), "i32".to_string()),
            ],
            output_type: "i32".to_string(),
            body: "a + b".to_string(),
        };
        assert_eq!(n.code(), "const fn add(a: i32, b: i32) -> i32 {a + b}");
    }

    #[test]
    fn fn_declaration_with_no_parameters_emits_empty_paren_list() {
        let n = FnDeclarationNode {
            name: "zero".to_string(),
            parameters: vec![],
            output_type: "i32".to_string(),
            body: "0".to_string(),
        };
        assert_eq!(n.code(), "const fn zero() -> i32 {0}");
    }

    // ---- StructDeclarationNode::code ---------------------------------

    #[test]
    fn struct_decl_code_emits_derive_struct_with_fields() {
        // Invariant F: core derives must be present, struct name must
        // match, and each field must appear with `name: type,`.
        let n = StructDeclarationNode {
            name: "Cat".to_string(),
            fields: vec![
                ("meowing".to_string(), Type::Bool),
                ("age".to_string(), Type::U8),
            ],
        };
        let code = n.code();
        assert!(code.contains("#[derive("), "got: {code}");
        for d in ["Copy", "Clone", "Debug", "Eq", "PartialEq"] {
            assert!(code.contains(d), "missing derive {d}; got: {code}");
        }
        assert!(code.contains("struct Cat {"), "got: {code}");
        assert!(code.contains("meowing: bool,"), "got: {code}");
        assert!(code.contains("age: u8,"), "got: {code}");
        // The closing brace must be present.
        assert!(code.contains('}'), "got: {code}");
    }

    #[test]
    fn struct_decl_code_no_fields_still_emits_struct() {
        // Invariant F: zero-field structs are valid and must compile.
        let n = StructDeclarationNode {
            name: "Empty".to_string(),
            fields: vec![],
        };
        let code = n.code();
        assert!(code.contains("struct Empty {"), "got: {code}");
    }

    #[test]
    fn struct_decl_code_with_bounds_emits_constants() {
        // Invariant F: `code_with_bounds` adds `pub const FIELD_MIN/MAX
        // /CARDINALITY` for bounded-int fields.
        use std::collections::HashMap;
        let mut bounds = HashMap::new();
        bounds.insert("age".to_string(), ResolvedFieldBounds { min: 0, max: 200 });
        let n = StructDeclarationNode {
            name: "Cat".to_string(),
            fields: vec![(
                "age".to_string(),
                Type::BoundedUint {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::Literal(200),
                },
            )],
        };
        let code = n.code_with_bounds(Some(&bounds));
        assert!(code.contains("impl Cat"), "got: {code}");
        assert!(code.contains("AGE_MIN"), "got: {code}");
        assert!(code.contains("AGE_MAX"), "got: {code}");
        assert!(code.contains("AGE_CARDINALITY"), "got: {code}");
        // 0..=200 -> cardinality 201, which fits in u8 (max 255).
        assert!(code.contains("AGE_CARDINALITY: u8 = 201;"), "got: {code}");
    }

    // ---- EnumDeclarationNode::code -----------------------------------

    #[test]
    fn enum_decl_code_emits_core_derives_and_variants() {
        // Invariant F.
        let n = EnumDeclarationNode {
            name: "Sound".to_string(),
            variants: vec![("Meow".to_string(), None), ("Bark".to_string(), None)],
            default_variant: None,
        };
        let code = n.code();
        for d in ["Copy", "Clone", "Debug", "Eq", "PartialEq"] {
            assert!(code.contains(d), "missing derive {d}; got: {code}");
        }
        assert!(code.contains("enum Sound {"), "got: {code}");
        assert!(code.contains("Meow,"), "got: {code}");
        assert!(code.contains("Bark,"), "got: {code}");
    }

    #[test]
    fn enum_decl_code_emits_marigold_variants_reflection() {
        // Invariant F: the `__marigold_variants()` helper is generated
        // for every enum so that `range(Enum)` can enumerate them.
        let n = EnumDeclarationNode {
            name: "Sound".to_string(),
            variants: vec![("Meow".to_string(), None), ("Bark".to_string(), None)],
            default_variant: None,
        };
        let code = n.code();
        assert!(
            code.contains("fn __marigold_variants() -> [Sound; 2]"),
            "got: {code}"
        );
        assert!(code.contains("[Sound::Meow, Sound::Bark]"), "got: {code}");
    }

    #[test]
    fn enum_decl_unit_variant_count_excludes_sized_default() {
        // Invariant G: a `Sized` default variant cannot be
        // copy-constructed and so is excluded from the unit count.
        let n = EnumDeclarationNode {
            name: "Sound".to_string(),
            variants: vec![("Meow".to_string(), None), ("Bark".to_string(), None)],
            default_variant: Some(DefaultEnumVariant::Sized("Other".to_string(), 32)),
        };
        assert_eq!(n.unit_variant_count(), 2);
        // And the marigold_variants array also excludes it.
        let code = n.code();
        assert!(
            code.contains("fn __marigold_variants() -> [Sound; 2]"),
            "got: {code}"
        );
    }

    #[test]
    fn enum_decl_unit_variant_count_includes_with_default_value() {
        // Invariant G: a `WithDefaultValue` default variant *is* a unit
        // variant and so is counted.
        let n = EnumDeclarationNode {
            name: "Sound".to_string(),
            variants: vec![("Meow".to_string(), None)],
            default_variant: Some(DefaultEnumVariant::WithDefaultValue(
                "Unknown".to_string(),
                "??".to_string(),
            )),
        };
        assert_eq!(n.unit_variant_count(), 2);
        let code = n.code();
        assert!(
            code.contains("fn __marigold_variants() -> [Sound; 2]"),
            "got: {code}"
        );
        assert!(code.contains("Sound::Meow, Sound::Unknown"), "got: {code}");
    }

    #[test]
    fn enum_decl_unit_variant_count_empty_variants() {
        // Invariant G: an enum with no variants and no default reports
        // zero unit variants — this is the lower-bound boundary.
        let n = EnumDeclarationNode {
            name: "Void".to_string(),
            variants: vec![],
            default_variant: None,
        };
        assert_eq!(n.unit_variant_count(), 0);
        let code = n.code();
        assert!(
            code.contains("fn __marigold_variants() -> [Void; 0]"),
            "got: {code}"
        );
    }
}
