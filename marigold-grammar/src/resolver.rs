use crate::diagnostics::{Diagnostic, Severity};
use crate::span_index::{Reference, ReferenceSource, SpanIndex, SymbolKind};
use std::collections::{HashMap, HashSet};

pub(crate) fn levenshtein(a: &str, b: &str) -> usize {
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
}
