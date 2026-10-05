//! Code generation that wraps every input, stream function and output with the OpenTelemetry
//! adapters of `marigold_impl::telemetry`.
//!
//! Only available with the `otel` feature. [`crate::marigold_parse`] never instruments: callers
//! opt in explicitly through [`crate::marigold_parse_instrumented`], so enabling the feature on
//! the grammar crate alone cannot change the code of other callers.
//!
//! Node ids come from [`crate::marigold_node_ids`] with [`CodegenOptions::id_namespace`] as the
//! program name, and are embedded in the generated code as string literals. Ids depend on the
//! program text only through its content with whitespace outside string literals removed (see
//! [`crate::node_ids`]), so code generated from the token-stringified body of a macro carries
//! exactly the ids the language server computes from the same program in a `.marigold` file.
//!
//! The byte ranges embedded next to the ids are offsets into the text that was given to the code
//! generator. For the `m!` macro that is the stringified macro body, so they are approximate:
//! they do not point into the Rust source file. Ids are exact; ranges are not.
//!
//! # Call sites
//!
//! Ids are unique within one program text. Two `m!` invocations in one crate that declare the
//! same variable name, or contain identical unnamed expressions, would therefore share ids and
//! merge their metrics. [`SiteRegistry`] prevents that: the first invocation with a given set of
//! ids keeps the plain ids, and every later invocation whose ids would collide is given a
//! [`CodegenOptions::discriminator`] derived from its call site, which changes only its ids.

use crate::node_ids::{node_infos, site_hash, NodeKind};
use crate::nodes::{
    NamedStreamNode, OutputFunctionNode, StreamFunctionKind, StreamFunctionNode,
    StreamVariableFromPriorStreamVariableNode, StreamVariableNode, UnnamedStreamNode,
};

use std::collections::HashMap;
use std::sync::Mutex;

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
    discriminator: Option<String>,
}

impl CodegenOptions {
    pub fn new(program: impl Into<String>) -> Self {
        Self {
            program: program.into(),
            file: None,
            discriminator: None,
        }
    }

    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// Fold a call-site discriminator into the node ids, leaving `marigold.program` unchanged.
    pub fn with_discriminator(mut self, discriminator: impl Into<String>) -> Self {
        self.discriminator = Some(discriminator.into());
        self
    }

    pub fn program(&self) -> &str {
        &self.program
    }

    pub fn discriminator(&self) -> Option<&str> {
        self.discriminator.as_deref()
    }

    /// The program name that the node ids are hashed with: `program`, or `program#discriminator`.
    ///
    /// ```
    /// use marigold_grammar::instrument::CodegenOptions;
    ///
    /// assert_eq!(CodegenOptions::new("demo").id_namespace(), "demo");
    /// let tagged = CodegenOptions::new("demo").with_discriminator("ab12");
    /// assert_eq!(tagged.id_namespace(), "demo#ab12");
    /// ```
    pub fn id_namespace(&self) -> String {
        match &self.discriminator {
            Some(d) => format!("{}#{d}", self.program),
            None => self.program.clone(),
        }
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }
}

#[derive(Default)]
struct Sites {
    programs: HashMap<String, (String, Option<String>)>,
    owners: HashMap<String, String>,
}

/// Hands out [`CodegenOptions`] so that no two call sites of one process share node ids.
///
/// Sites are identified by an opaque string, such as the debug rendering of a macro call-site
/// span. The first site to claim a set of ids gets the plain ids. A later site whose ids are
/// already claimed gets a discriminator derived from the site string, so the result does not
/// depend on how many other sites there are. Asking again for the same site and program is
/// idempotent; asking with a changed program at a known site releases the site's old ids first,
/// which keeps long-lived expanders such as editors stable.
///
/// ```
/// use marigold_grammar::instrument::SiteRegistry;
///
/// let registry = SiteRegistry::new();
/// let first = registry.options("x = range(0, 3)\nx.return", "demo", "site-a");
/// let second = registry.options("x = range(0, 3)\nx.return", "demo", "site-b");
/// assert_eq!(first.discriminator(), None);
/// assert!(second.discriminator().is_some());
/// ```
pub struct SiteRegistry {
    sites: Mutex<Option<Sites>>,
}

impl Default for SiteRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl SiteRegistry {
    pub const fn new() -> Self {
        Self {
            sites: Mutex::new(None),
        }
    }

    pub fn options(&self, source: &str, program: &str, site: &str) -> CodegenOptions {
        let mut guard = self.sites.lock().unwrap_or_else(|e| e.into_inner());
        let sites = guard.get_or_insert_with(Sites::default);
        let key = format!("{program}\u{0}{site}");
        if let Some((known_source, discriminator)) = sites.programs.get(&key) {
            if known_source == source {
                return Self::build(program, discriminator.clone());
            }
        }
        sites.owners.retain(|_, owner| *owner != key);
        let ids_of = |options: &CodegenOptions| -> Vec<String> {
            node_infos(source, &options.id_namespace())
                .into_iter()
                .map(|n| n.id.to_string())
                .collect()
        };
        let mut options = CodegenOptions::new(program);
        if ids_of(&options)
            .iter()
            .any(|id| sites.owners.contains_key(id))
        {
            options = options.with_discriminator(site_hash(site));
        }
        for id in ids_of(&options) {
            sites.owners.insert(id, key.clone());
        }
        sites.programs.insert(
            key,
            (
                source.to_string(),
                options.discriminator().map(str::to_string),
            ),
        );
        options
    }

    fn build(program: &str, discriminator: Option<String>) -> CodegenOptions {
        let options = CodegenOptions::new(program);
        match discriminator {
            Some(d) => options.with_discriminator(d),
            None => options,
        }
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
        let nodes = node_infos(src, &options.id_namespace())
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
