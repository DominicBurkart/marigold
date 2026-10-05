//! Code generation that wraps every input, stream function and output with the OpenTelemetry
//! adapters of `marigold_impl::telemetry`.
//!
//! Only available with the `otel` feature. [`crate::marigold_parse`] never instruments: callers
//! opt in explicitly through [`crate::marigold_parse_instrumented`], so enabling the feature on
//! the grammar crate alone cannot change the code of other callers.
//!
//! Node ids come from [`crate::marigold_node_ids`] with [`CodegenOptions::program`] as the
//! program name, and are embedded in the generated code as string literals.

use crate::node_ids::{node_infos, NodeKind};
use crate::nodes::{
    NamedStreamNode, OutputFunctionNode, StreamFunctionKind, StreamFunctionNode,
    StreamVariableFromPriorStreamVariableNode, StreamVariableNode, UnnamedStreamNode,
};

const TELEMETRY: &str = "::marigold::marigold_impl::telemetry";

/// Naming of an instrumented program.
///
/// `program` namespaces the node ids and is the `marigold.program` fallback used when
/// `OTEL_SERVICE_NAME` is not set at run time. `file` is recorded on spans; when it is not given,
/// the generated code uses `file!()` at the place the code is expanded.
///
/// ```
/// use marigold_grammar::instrument::CodegenOptions;
///
/// let options = CodegenOptions::new("demo").with_file("demo.marigold");
/// assert_eq!(options.program(), "demo");
/// assert_eq!(options.file(), Some("demo.marigold"));
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodegenOptions {
    program: String,
    file: Option<String>,
}

impl CodegenOptions {
    pub fn new(program: impl Into<String>) -> Self {
        Self {
            program: program.into(),
            file: None,
        }
    }

    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    pub fn program(&self) -> &str {
        &self.program
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }
}

struct PlanNode {
    expr_index: usize,
    kind: NodeKind,
    ordinal: usize,
    meta: String,
    yields_results: bool,
}

pub(crate) struct Plan {
    nodes: Vec<PlanNode>,
}

impl Plan {
    pub(crate) fn new(src: &str, options: &CodegenOptions) -> Self {
        let file = match options.file() {
            Some(file) => format!("{file:?}"),
            None => "file!()".to_string(),
        };
        let nodes = node_infos(src, options.program())
            .into_iter()
            .map(|info| {
                let meta = format!(
                    "{TELEMETRY}::NodeMeta::new({:?}, {:?}, {file}, {}, {})",
                    info.id.as_str(),
                    options.program(),
                    info.range.start,
                    info.range.end
                );
                let yields_results = info.kind == NodeKind::Input
                    && src[info.range.start..info.range.end].starts_with("read_file(");
                PlanNode {
                    expr_index: info.expr_index,
                    kind: info.kind,
                    ordinal: info.ordinal,
                    meta,
                    yields_results,
                }
            })
            .collect();
        Self { nodes }
    }

    fn expr_nodes(&self, expr_index: usize, expected: usize) -> Result<Vec<&PlanNode>, String> {
        let nodes: Vec<&PlanNode> = self
            .nodes
            .iter()
            .filter(|n| n.expr_index == expr_index && n.kind != NodeKind::VariableDeclaration)
            .collect();
        let aligned =
            nodes.len() == expected && nodes.iter().enumerate().all(|(i, n)| n.ordinal == i);
        if aligned {
            Ok(nodes)
        } else {
            Err(format!(
                "internal marigold error: node ids for expression {expr_index} do not match the \
                 generated stream ({} ids, {expected} nodes)",
                nodes.len()
            ))
        }
    }

    fn call(function: &str, code: &str, node: &PlanNode, results: bool) -> String {
        let suffix = if results { "_results" } else { "" };
        format!("{TELEMETRY}::{function}{suffix}({code}, {})", node.meta)
    }

    fn chain(
        start: String,
        input: &PlanNode,
        funs: &[StreamFunctionNode],
        fun_nodes: &[&PlanNode],
    ) -> String {
        let mut code = Self::call("instrument", &start, input, input.yields_results);
        for (fun, node) in funs.iter().zip(fun_nodes) {
            let consumes_results = matches!(
                fun.kind,
                StreamFunctionKind::Ok | StreamFunctionKind::OkOrPanic
            );
            let entered = Self::call("instrument_in", &code, node, consumes_results);
            code = Self::call(
                "instrument",
                &format!("{entered}.{}", fun.code),
                node,
                false,
            );
        }
        code
    }

    fn output(chain: &str, node: &PlanNode, out: &OutputFunctionNode) -> String {
        let entered = Self::call("instrument_in", chain, node, false);
        let body = format!("{}{entered}{}", out.stream_prefix, out.stream_postfix);
        let wrapped = Self::call("instrument", &body, node, false);
        format!("{{use ::marigold::marigold_impl::*; {wrapped}}}")
    }

    pub(crate) fn unnamed(
        &self,
        expr_index: usize,
        node: &UnnamedStreamNode,
    ) -> Result<String, String> {
        let funs = &node.inp_and_funs.funs;
        let nodes = self.expr_nodes(expr_index, funs.len() + 2)?;
        let chain = Self::chain(
            node.inp_and_funs.inp.code.clone(),
            nodes[0],
            funs,
            &nodes[1..=funs.len()],
        );
        Ok(Self::output(&chain, nodes[funs.len() + 1], &node.out))
    }

    pub(crate) fn named(
        &self,
        expr_index: usize,
        node: &NamedStreamNode,
    ) -> Result<String, String> {
        let nodes = self.expr_nodes(expr_index, node.funs.len() + 2)?;
        let chain = Self::chain(
            format!("{}.get()", node.stream_variable),
            nodes[0],
            &node.funs,
            &nodes[1..=node.funs.len()],
        );
        Ok(Self::output(&chain, nodes[node.funs.len() + 1], &node.out))
    }

    fn declaration(name: &str, chain: &str) -> String {
        format!("let mut {name} = {{use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new({chain})}};")
    }

    pub(crate) fn variable(
        &self,
        expr_index: usize,
        node: &StreamVariableNode,
    ) -> Result<String, String> {
        let nodes = self.expr_nodes(expr_index, node.funs.len() + 1)?;
        let chain = Self::chain(
            node.inp.code.clone(),
            nodes[0],
            &node.funs,
            &nodes[1..=node.funs.len()],
        );
        Ok(Self::declaration(&node.variable_name, &chain))
    }

    pub(crate) fn variable_from_prior(
        &self,
        expr_index: usize,
        node: &StreamVariableFromPriorStreamVariableNode,
    ) -> Result<String, String> {
        let nodes = self.expr_nodes(expr_index, node.funs.len() + 1)?;
        let chain = Self::chain(
            format!("{}.get()", node.prior_stream_variable),
            nodes[0],
            &node.funs,
            &nodes[1..=node.funs.len()],
        );
        Ok(Self::declaration(&node.variable_name, &chain))
    }
}
