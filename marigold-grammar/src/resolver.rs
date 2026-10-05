use crate::diagnostics::{Diagnostic, Severity};
use crate::span_index::{Reference, ReferenceSource, SpanIndex, SymbolKind};
use std::collections::{HashMap, HashSet};

#[cfg(test)]
thread_local! {
    static LEVENSHTEIN_CALLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

pub(crate) fn levenshtein(a: &str, b: &str) -> usize {
    #[cfg(test)]
    LEVENSHTEIN_CALLS.with(|c| c.set(c.get() + 1));
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.chars().enumerate() {
        let mut cur = vec![i + 1];
        for (j, cb) in b.iter().enumerate() {
            let substitution = prev[j] + usize::from(ca != *cb);
            cur.push(substitution.min(prev[j + 1] + 1).min(cur[j] + 1));
        }
        prev = cur;
    }
    prev[b.len()]
}

const MAX_DIAGNOSTICS: usize = 50;
const MAX_SUGGESTION_CANDIDATES: usize = 200;
const MAX_LISTED_NAMES: usize = 20;
const RUST_SCOPE_NOTE: &str =
    "it may be a Rust item in scope inside m!() and can be ignored in that case";

pub(crate) fn closest<'a>(name: &str, candidates: &[&'a str]) -> Option<&'a str> {
    let name_len = name.chars().count();
    let limit = 2.min(name_len.saturating_sub(1));
    candidates
        .iter()
        .filter(|c| c.chars().count().abs_diff(name_len) <= limit)
        .map(|c| (levenshtein(name, c), *c))
        .filter(|(d, _)| (1..=limit).contains(d))
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c)
}

struct Names<'a> {
    ordered: Vec<&'a str>,
    set: HashSet<&'a str>,
}

impl<'a> Names<'a> {
    fn of(index: &'a SpanIndex, kind: SymbolKind) -> Self {
        let mut ordered = Vec::new();
        let mut set = HashSet::new();
        for d in index.declarations.iter().filter(|d| d.kind == kind) {
            if set.insert(d.name.as_str()) {
                ordered.push(d.name.as_str());
            }
        }
        Self { ordered, set }
    }

    fn contains(&self, name: &str) -> bool {
        self.set.contains(name)
    }
}

struct Noun {
    code: &'static str,
    plural: &'static str,
    listed: &'static str,
    severity: Severity,
    singular: &'static str,
}

const VARIABLE: Noun = Noun {
    code: "undefined-stream-variable",
    plural: "stream variables",
    listed: "defined",
    severity: Severity::Warning,
    singular: "stream variable",
};
const FUNCTION: Noun = Noun {
    code: "undefined-fn",
    plural: "functions",
    listed: "declared",
    severity: Severity::Information,
    singular: "function",
};
const STRUCT: Noun = Noun {
    code: "undefined-struct",
    plural: "structs",
    listed: "declared",
    severity: Severity::Information,
    singular: "struct",
};

enum Problem {
    Undefined,
    ReadBeforeDeclared,
}

fn explain(r: &Reference, noun: &Noun, names: &Names, problem: &Problem) -> Diagnostic {
    let name = &r.name;
    let message = match problem {
        Problem::ReadBeforeDeclared => format!(
            "stream variable '{name}' is read before it is declared; a stream variable can only read variables declared above it"
        ),
        Problem::Undefined if noun.severity == Severity::Warning => {
            format!("stream variable '{name}' is not defined in this program")
        }
        Problem::Undefined => format!(
            "{} '{name}' is not declared in this program; {RUST_SCOPE_NOTE}",
            noun.singular
        ),
    };
    let mut help = String::new();
    if names.ordered.len() <= MAX_SUGGESTION_CANDIDATES {
        if let Some(close) = closest(name, &names.ordered) {
            help.push_str(&format!("did you mean '{close}'? "));
        }
    }
    if names.ordered.is_empty() {
        help.push_str(&format!(
            "no {} are {} in this program",
            noun.plural, noun.listed
        ));
    } else {
        let shown = &names.ordered[..names.ordered.len().min(MAX_LISTED_NAMES)];
        help.push_str(&format!(
            "{} {}: {}",
            noun.listed,
            noun.plural,
            shown.join(", ")
        ));
        if names.ordered.len() > shown.len() {
            help.push_str(&format!(", and {} more", names.ordered.len() - shown.len()));
        }
    }
    if noun.severity == Severity::Information {
        help.push_str(&format!(
            "; if '{name}' is a Rust item in scope inside m!(), ignore this"
        ));
    }
    Diagnostic::new(r.range, noun.severity, noun.code, message).with_help(help)
}

pub(crate) fn warnings(index: &SpanIndex) -> Vec<Diagnostic> {
    let variables = Names::of(index, SymbolKind::StreamVariable);
    let functions = Names::of(index, SymbolKind::Function);
    let structs = Names::of(index, SymbolKind::Struct);
    let mut first_declared: HashMap<&str, usize> = HashMap::new();
    for d in index
        .declarations
        .iter()
        .filter(|d| d.kind == SymbolKind::StreamVariable)
    {
        let entry = first_declared
            .entry(d.name.as_str())
            .or_insert(d.expr_index);
        *entry = (*entry).min(d.expr_index);
    }
    let mut found: Vec<(&Reference, &'static Noun, &Names, Problem)> = Vec::new();
    for r in &index.references {
        let name = r.name.as_str();
        match r.source {
            ReferenceSource::StreamStart => {
                if !variables.contains(name) {
                    found.push((r, &VARIABLE, &variables, Problem::Undefined));
                }
            }
            ReferenceSource::VariableSource => match first_declared.get(name) {
                None => found.push((r, &VARIABLE, &variables, Problem::Undefined)),
                Some(&at) if at >= r.expr_index => {
                    found.push((r, &VARIABLE, &variables, Problem::ReadBeforeDeclared))
                }
                Some(_) => {}
            },
            ReferenceSource::MapFn
            | ReferenceSource::FilterFn
            | ReferenceSource::FilterMapFn
            | ReferenceSource::FoldFn
            | ReferenceSource::KeepFirstNFn => {
                if !functions.contains(name) {
                    found.push((r, &FUNCTION, &functions, Problem::Undefined));
                }
            }
            ReferenceSource::ReadFileStruct => {
                if !structs.contains(name) {
                    found.push((r, &STRUCT, &structs, Problem::Undefined));
                }
            }
            ReferenceSource::RangeEnum => {}
        }
    }
    found.sort_by_key(|(_, noun, _, _)| noun.severity != Severity::Warning);
    let omitted = found.len().saturating_sub(MAX_DIAGNOSTICS);
    let first_omitted = found.get(MAX_DIAGNOSTICS).map(|(r, ..)| r.range);
    let mut out: Vec<Diagnostic> = found
        .iter()
        .take(MAX_DIAGNOSTICS)
        .map(|(r, noun, names, problem)| explain(r, noun, names, problem))
        .collect();
    if let Some(range) = first_omitted {
        out.push(Diagnostic::information(
            range,
            "resolver-diagnostics-truncated",
            format!(
                "{omitted} more undefined-name diagnostics were omitted; only the first {MAX_DIAGNOSTICS} are reported per file"
            ),
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levenshtein_basics() {
        assert_eq!(levenshtein("", ""), 0);
        assert_eq!(levenshtein("abc", ""), 3);
        assert_eq!(levenshtein("", "abc"), 3);
        assert_eq!(levenshtein("kitten", "sitting"), 3);
        assert_eq!(levenshtein("double", "doubel"), 2);
        assert_eq!(levenshtein("f", "g"), 1);
        assert_eq!(levenshtein("\u{fc}ber", "uber"), 1);
    }

    #[test]
    fn closest_prefers_nearest_and_ignores_exact_and_far() {
        assert_eq!(closest("doubel", &["triple", "double"]), Some("double"));
        assert_eq!(closest("double", &["double"]), None);
        assert_eq!(closest("abcdef", &["uvwxyz"]), None);
        assert_eq!(closest("ab", &["ac"]), Some("ac"));
        assert_eq!(closest("a", &["b"]), None);
        assert_eq!(closest("x", &[]), None);
    }

    fn range() -> crate::diagnostics::ByteRange {
        crate::diagnostics::ByteRange { start: 0, end: 1 }
    }

    fn fn_index(n: usize) -> SpanIndex {
        let mut index = SpanIndex::default();
        for i in 0..n {
            index.declarations.push(crate::span_index::Declaration {
                name: format!("f{i}"),
                range: range(),
                kind: SymbolKind::Function,
                expr_index: i,
            });
            index.references.push(Reference {
                name: format!("g{i}"),
                range: range(),
                source: ReferenceSource::MapFn,
                expr_index: i,
            });
        }
        index
    }

    fn variable_index(n: usize) -> SpanIndex {
        let mut index = SpanIndex::default();
        for i in 0..n {
            index.declarations.push(crate::span_index::Declaration {
                name: format!("v{i}"),
                range: range(),
                kind: SymbolKind::StreamVariable,
                expr_index: i,
            });
            index.references.push(Reference {
                name: format!("v{i}"),
                range: range(),
                source: ReferenceSource::VariableSource,
                expr_index: i + 1,
            });
            index.references.push(Reference {
                name: format!("w{i}"),
                range: range(),
                source: ReferenceSource::StreamStart,
                expr_index: i,
            });
        }
        index
    }

    fn levenshtein_calls_during(index: &SpanIndex) -> usize {
        LEVENSHTEIN_CALLS.with(|c| c.set(0));
        let _ = warnings(index);
        LEVENSHTEIN_CALLS.with(|c| c.get())
    }

    fn best_of_three(index: &SpanIndex) -> std::time::Duration {
        (0..3)
            .map(|_| {
                let start = std::time::Instant::now();
                let out = warnings(index);
                let elapsed = start.elapsed();
                std::hint::black_box(out);
                elapsed
            })
            .min()
            .unwrap()
    }

    fn assert_scales_linearly(build: fn(usize) -> SpanIndex) {
        let small = 5_000;
        let factor = 4;
        let (small_index, large_index) = (build(small), build(small * factor));
        let t_small = best_of_three(&small_index);
        let t_large = best_of_three(&large_index);
        let allowed = t_small * (factor as u32 * 5 / 2) + std::time::Duration::from_millis(50);
        assert!(
            t_large < allowed,
            "t({small})={t_small:?} t({})={t_large:?} allowed={allowed:?}",
            small * factor
        );
        assert!(t_large < std::time::Duration::from_secs(60), "{t_large:?}");
    }

    #[test]
    fn levenshtein_calls_are_bounded_independently_of_program_size() {
        let bound = MAX_DIAGNOSTICS * MAX_SUGGESTION_CANDIDATES;
        for n in [10, 150, 1_000, 20_000] {
            let calls = levenshtein_calls_during(&fn_index(n));
            assert!(calls <= bound, "n={n} calls={calls} bound={bound}");
        }
    }

    #[test]
    fn levenshtein_is_not_called_when_candidates_exceed_the_cap() {
        let calls = levenshtein_calls_during(&fn_index(MAX_SUGGESTION_CANDIDATES + 1));
        assert_eq!(calls, 0);
    }

    #[test]
    fn levenshtein_is_called_per_diagnostic_when_candidates_fit_the_cap() {
        let n = MAX_SUGGESTION_CANDIDATES;
        let calls = levenshtein_calls_during(&fn_index(n));
        assert!(calls > 0 && calls <= MAX_DIAGNOSTICS * n, "{calls}");
    }

    #[test]
    fn function_resolution_scales_linearly() {
        assert_scales_linearly(fn_index);
    }

    #[test]
    fn stream_variable_resolution_scales_linearly() {
        assert_scales_linearly(variable_index);
    }
}
