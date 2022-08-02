use regex::Regex;
use std::str::FromStr;

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
    pub fields: Vec<(String, Primitive)>,
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
pub enum Primitive {
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
}

impl FromStr for Primitive {
    type Err = ();

    fn from_str(s: &str) -> Result<Primitive, ()> {
        match s {
            "u8" => Ok(Primitive::U8),
            "u16" => Ok(Primitive::U16),
            "u32" => Ok(Primitive::U32),
            "u64" => Ok(Primitive::U64),
            "u128" => Ok(Primitive::U128),
            "usize" => Ok(Primitive::USize),
            "i8" => Ok(Primitive::I8),
            "i16" => Ok(Primitive::I16),
            "i32" => Ok(Primitive::I32),
            "i64" => Ok(Primitive::I64),
            "i128" => Ok(Primitive::I128),
            "isize" => Ok(Primitive::ISize),
            "f32" => Ok(Primitive::F32),
            "f64" => Ok(Primitive::F64),
            "bool" => Ok(Primitive::Bool),
            "char" => Ok(Primitive::Char),
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
                    return Ok(Primitive::Str(size));
                }
                panic!("Could not parse type defintion: {}", s);
            }
        }
    }
}

impl Primitive {
    pub fn primitive_to_type_string(&self) -> String {
        match *self {
            Primitive::U8 => "u8".to_string(),
            Primitive::U16 => "u16".to_string(),
            Primitive::U32 => "u32".to_string(),
            Primitive::U64 => "u64".to_string(),
            Primitive::U128 => "u128".to_string(),
            Primitive::USize => "usize".to_string(),
            Primitive::I8 => "i8".to_string(),
            Primitive::I16 => "i16".to_string(),
            Primitive::I32 => "i32".to_string(),
            Primitive::I64 => "i64".to_string(),
            Primitive::I128 => "i128".to_string(),
            Primitive::ISize => "isize".to_string(),
            Primitive::F32 => "f32".to_string(),
            Primitive::F64 => "f64".to_string(),
            Primitive::Bool => "bool".to_string(),
            Primitive::Char => "char".to_string(),
            Primitive::Str(v) => format!("::marigold::marigold_impl::arrayvec::ArrayString<{v}>"),
        }
    }
}

pub enum PrimitiveValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
}

pub struct EnumDeclarationNode {
    pub name: String,
    pub fields: Vec<(String, Option<String>)>,
}

impl EnumDeclarationNode {
    fn definition_to_strum(maybe_definition: &Option<String>) -> Option<String> {
        if let Some(definition) = maybe_definition {
            return match definition.as_str() {
                "disabled" => Some("disabled".to_string()),
                "ascii_case_insensitive" => Some("ascii_case_insensitive".to_string()),
                _ => {
                    lazy_static! {
                        static ref VALUES_ARRAY: Regex =
                            Regex::new(r#"\[(("(?P<serialization_value>[^"]+)"),?[\W]*)+\]"#)
                                .unwrap();
                        static ref QUOTED: Regex =
                            Regex::new(r#""(?P<serialization_value>[^"])+""#).unwrap();
                    }
                    let array_values = VALUES_ARRAY
                        .captures_iter(definition.as_str())
                        .map(|c| {
                            let value = &c["serialization_value"];
                            format!("serialize = \"{value}\"")
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    if !array_values.is_empty() {
                        return Some(array_values);
                    }
                    if let Some(quoted_value) = QUOTED.captures(definition.as_str()) {
                        let value = &quoted_value["serialization_value"];
                        return Some(format!("serialize = \"{value}\""));
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
            "::marigold::marigold_impl::strum::EnumString",
        ]
        .join(", ");
        let name = &self.name;
        let mut enum_rep = format!(
            "
            use ::marigold::marigold_impl::strum as strum;

            #[derive({traits})]
            enum {name} {{
            "
        );
        for (field_name, serialization_definition) in &self.fields {
            if let Some(strum_def) = Self::definition_to_strum(serialization_definition) {
                enum_rep.push_str(format!("#[strum({strum_def})]").as_str());
            }
            enum_rep.push_str(field_name.as_str());
            enum_rep.push_str(",\n");
        }
        enum_rep.push('}');
        enum_rep
    }
}
