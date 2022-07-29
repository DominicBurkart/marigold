use strum_macros::EnumString;

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
            "Serialize",
            "Deserialize",
        ]
        .join(", ");
        let name = &self.name;
        let mut struct_rep = format!(
            "
            #[derive({traits})]
            struct {name} {{
            "
        ); // todo add serde de/serialize iff io feature included
        for (field_name, field_type) in &self.fields {
            struct_rep.push_str(field_name.as_str());
            struct_rep.push(':');
            struct_rep.push_str(field_type.primitive_to_type_string().as_str());
            struct_rep.push_str(",\n");
        }
        struct_rep.push('}');
        struct_rep
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, EnumString)]
pub enum Primitive {
    #[strum(serialize = "u8")]
    U8,
    #[strum(serialize = "u16")]
    U16,
    #[strum(serialize = "u32")]
    U32,
    #[strum(serialize = "u64")]
    U64,
    #[strum(serialize = "u128")]
    U128,
    #[strum(serialize = "usize")]
    USize,
    #[strum(serialize = "i8")]
    I8,
    #[strum(serialize = "i16")]
    I16,
    #[strum(serialize = "i32")]
    I32,
    #[strum(serialize = "i64")]
    I64,
    #[strum(serialize = "i128")]
    I128,
    #[strum(serialize = "isize")]
    ISize,
    #[strum(serialize = "f32")]
    F32,
    #[strum(serialize = "f64")]
    F64,
    #[strum(serialize = "bool")]
    Bool,
    #[strum(serialize = "char")]
    Char,
    #[strum()]
    Str(u32),
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
