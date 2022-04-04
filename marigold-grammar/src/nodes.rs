pub struct StreamNode {
    pub inp: InputFunctionNode,
    pub funs: Vec<StreamFunctionNode>,
    pub out: String,
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
        let out = &self.out;
        format!("async {{use marigold_grammar::itertools::Itertools; {inp}.{intermediate}{out}}}")
    }
}

pub struct StreamFunctionNode {
    pub code: String,
    // pub computational_complexity: String,
    // pub memory: String,
    // pub new_era: bool,
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
