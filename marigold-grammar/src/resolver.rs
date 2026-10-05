use crate::diagnostics::Diagnostic;
use crate::span_index::{Reference, ReferenceSource, SpanIndex, SymbolKind};

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

pub(crate) fn closest<'a>(name: &str, candidates: &[&'a str]) -> Option<&'a str> {
    let limit = 2.min(name.chars().count().saturating_sub(1));
    candidates
        .iter()
        .map(|c| (levenshtein(name, c), *c))
        .filter(|(d, _)| (1..=limit).contains(d))
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c)
}

fn names_of(index: &SpanIndex, kind: SymbolKind) -> Vec<&str> {
    let mut names: Vec<&str> = Vec::new();
    for d in index.declarations.iter().filter(|d| d.kind == kind) {
        if !names.contains(&d.name.as_str()) {
            names.push(&d.name);
        }
    }
    names
}

struct Noun {
    code: &'static str,
    plural: &'static str,
    listed: &'static str,
}

const VARIABLE: Noun = Noun {
    code: "undefined-stream-variable",
    plural: "stream variables",
    listed: "defined",
};
const FUNCTION: Noun = Noun {
    code: "undefined-fn",
    plural: "functions",
    listed: "declared",
};
const STRUCT: Noun = Noun {
    code: "undefined-struct",
    plural: "structs",
    listed: "declared",
};

fn warn(r: &Reference, noun: &Noun, defined: &[&str], message: String) -> Diagnostic {
    let mut help = String::new();
    if let Some(close) = closest(&r.name, defined) {
        help.push_str(&format!("did you mean '{close}'? "));
    }
    if defined.is_empty() {
        help.push_str(&format!(
            "no {} are {} in this program",
            noun.plural, noun.listed
        ));
    } else {
        help.push_str(&format!(
            "{} {}: {}",
            noun.listed,
            noun.plural,
            defined.join(", ")
        ));
    }
    Diagnostic::warning(r.range, noun.code, message).with_help(help)
}

pub(crate) fn warnings(index: &SpanIndex) -> Vec<Diagnostic> {
    let variables = names_of(index, SymbolKind::StreamVariable);
    let functions = names_of(index, SymbolKind::Function);
    let structs = names_of(index, SymbolKind::Struct);
    let declared_before = |name: &str, expr_index: usize| {
        index.declarations.iter().any(|d| {
            d.kind == SymbolKind::StreamVariable && d.name == name && d.expr_index < expr_index
        })
    };
    let mut out = Vec::new();
    for r in &index.references {
        let name = &r.name;
        match r.source {
            ReferenceSource::StreamStart => {
                if !variables.contains(&name.as_str()) {
                    out.push(warn(
                        r,
                        &VARIABLE,
                        &variables,
                        format!("stream variable '{name}' is not defined in this program"),
                    ));
                }
            }
            ReferenceSource::VariableSource => {
                if !declared_before(name, r.expr_index) {
                    let message = if variables.contains(&name.as_str()) {
                        format!(
                            "stream variable '{name}' is used before it is declared; \
                             variables must be declared before other variables that read them"
                        )
                    } else {
                        format!("stream variable '{name}' is not defined in this program")
                    };
                    out.push(warn(r, &VARIABLE, &variables, message));
                }
            }
            ReferenceSource::MapFn
            | ReferenceSource::FilterFn
            | ReferenceSource::FilterMapFn
            | ReferenceSource::FoldFn
            | ReferenceSource::KeepFirstNFn => {
                if !functions.contains(&name.as_str()) {
                    out.push(warn(
                        r,
                        &FUNCTION,
                        &functions,
                        format!("function '{name}' is not declared in this program"),
                    ));
                }
            }
            ReferenceSource::ReadFileStruct => {
                if !structs.contains(&name.as_str()) {
                    out.push(warn(
                        r,
                        &STRUCT,
                        &structs,
                        format!("struct '{name}' is not declared in this program"),
                    ));
                }
            }
            ReferenceSource::RangeEnum => {}
        }
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
