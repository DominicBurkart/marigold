pub struct StreamExpr {
    pub inp: InputFunctionExpr,
    pub funs: Vec<String>,
    pub out: String,
}

impl StreamExpr {
    pub fn code(&self) -> String {
        let inp = &self.inp.code;
        let intermediate = self.funs.join(".");
        let out = &self.out;
        format!("async {{use glass_grammar::itertools::Itertools; {inp}.{intermediate}{out}}}")
    }
}

/// Whether the input is known at compile time (constant),
/// or whether it is not available until runtime (variable).
pub enum InputVariability {
    Constant,
    Variable,
}

pub struct InputFunctionExpr {
    pub variability: InputVariability,
    pub code: String,
}
