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
