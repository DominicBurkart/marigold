#![cfg(feature = "otel")]

use marigold_grammar::instrument::CodegenOptions;
use marigold_grammar::node_ids::NodeKind;
use marigold_grammar::{marigold_node_ids, marigold_parse_instrumented};
use proptest::prelude::*;
use std::collections::BTreeSet;

const PROGRAMS: &[&str] = &[
    "range(0, 10).return",
    "range(0, 5)\n    .map(double)\n    .filter(is_even)\n    .return",
    "digits = range(0, 10)\n\ndigits\n    .filter(is_odd)\n    .return\ndigits.return",
    "read_file(\"in dir/a  b.csv\", csv, struct=Data)\n\t.ok_or_panic()\n\t.write_file(\"out  dir/o.csv.gz\", csv)",
    "range(0, 3).return\nrange(0, 3).return\nrange(0, 3).return",
    "x = range(0, 3)\nx = x.map(f)\nx.return",
    "select_all(\n    range(0, 10).map(double),\n    range(0, 20)\n).filter(is_odd).return",
    "fn double(i: i32) -> i32 {\n    i * 2\n}\n\nrange(0, 3).map(double).return",
    "range(0, 3)\r\n.map(f)\r\n.return",
];

fn token_style(src: &str) -> String {
    let mut out = String::new();
    let mut in_string = false;
    let mut gap = false;
    for c in src.chars() {
        if c == '"' {
            in_string = !in_string;
            if gap {
                out.push(' ');
                gap = false;
            }
            out.push(c);
        } else if in_string {
            out.push(c);
        } else if c.is_ascii_whitespace() {
            gap = true;
        } else {
            if gap {
                out.push(' ');
                gap = false;
            }
            out.push(c);
        }
    }
    out
}

fn embedded_ids(code: &str) -> BTreeSet<String> {
    code.match_indices("NodeMeta::new(\"mg1-")
        .map(|(at, m)| {
            let start = at + m.len() - 4;
            code[start..start + 20].to_string()
        })
        .collect()
}

fn raw_ids(src: &str, program: &str) -> BTreeSet<String> {
    marigold_node_ids(src, program)
        .into_iter()
        .filter(|n| n.kind != NodeKind::VariableDeclaration)
        .map(|n| n.id.to_string())
        .collect()
}

fn generated_ids(src: &str) -> BTreeSet<String> {
    let code = marigold_parse_instrumented(src, &CodegenOptions::new("demo")).unwrap();
    embedded_ids(&code)
}

#[test]
fn ids_from_mangled_text_equal_ids_from_the_raw_file() {
    for raw in PROGRAMS {
        let mangled = token_style(raw);
        assert_eq!(
            generated_ids(&mangled),
            raw_ids(raw, "demo"),
            "raw: {raw:?}\nmangled: {mangled:?}"
        );
    }
}

#[test]
fn ids_from_the_raw_file_are_embedded_verbatim() {
    for raw in PROGRAMS {
        assert_eq!(generated_ids(raw), raw_ids(raw, "demo"), "{raw:?}");
    }
}

#[test]
fn string_literal_contents_still_distinguish_ids() {
    let a = generated_ids("range(0, 1).write_file(\"a b.csv\", csv)");
    let b = generated_ids("range(0, 1).write_file(\"ab.csv\", csv)");
    assert_ne!(a, b);
}

#[test]
fn ranges_in_mangled_text_are_relative_to_that_text() {
    let raw = "range(0, 5)\n    .map(double)\n    .return";
    let mangled = token_style(raw);
    let raw_nodes = marigold_node_ids(raw, "demo");
    let mangled_nodes = marigold_node_ids(&mangled, "demo");
    assert_eq!(raw_nodes.len(), mangled_nodes.len());
    assert!(raw_nodes
        .iter()
        .zip(&mangled_nodes)
        .any(|(a, b)| a.range != b.range));
    for (a, b) in raw_nodes.iter().zip(&mangled_nodes) {
        assert_eq!(a.id, b.id);
        assert_eq!(a.content_hash, b.content_hash);
    }
}

proptest! {
    #[test]
    fn any_whitespace_layout_gives_the_same_ids(
        pick in 0..PROGRAMS.len(),
        seed in proptest::collection::vec(0u8..4, 1..40),
    ) {
        let raw = PROGRAMS[pick];
        let mut out = String::new();
        let mut in_string = false;
        let mut n = 0usize;
        for c in raw.chars() {
            if c == '"' {
                in_string = !in_string;
            }
            if !in_string && c != '"' && c.is_ascii_whitespace() {
                out.push_str([" ", "\n\n", "\t", "  "][seed[n % seed.len()] as usize]);
                n += 1;
            } else if !in_string && c == '.' && out.ends_with(')') {
                out.push_str([" ", "\n    ", "", "\t"][seed[n % seed.len()] as usize]);
                n += 1;
                out.push(c);
            } else if !in_string && c == ',' {
                out.push(c);
                out.push_str([" ", "\n", "", "\t\t"][seed[n % seed.len()] as usize]);
                n += 1;
            } else {
                out.push(c);
            }
        }
        prop_assert_eq!(generated_ids(&out), raw_ids(raw, "demo"));
    }
}
