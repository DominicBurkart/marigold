use regex::Regex;
use std::str::FromStr;

const MAX_CUSTOM_TYPE_SIZE: usize = 9_999;

pub struct StreamNode {
    pub inp: InputFunctionNode,
    pub funs: Vec<StreamFunctionNode>,
    pub out: OutputFunctionNode,
}

impl StreamNode {
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
        format!("async {{use ::marigold::marigold_impl::*; {stream_prefix}{inp}.{intermediate}{stream_postfix}}}")
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
                enum_rep.push_str(format!("#[serde({serde_def})]").as_str())
            }
            enum_rep.push_str(field_name.as_str());
            enum_rep.push_str(",\n");
        }
        enum_rep.push('}');
        enum_rep
    }
}
