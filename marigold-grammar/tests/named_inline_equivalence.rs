use marigold_grammar::complexity::{ComplexityClass, ProgramComplexity, StreamComplexity};
use marigold_grammar::{marigold_analyze, marigold_stream_complexities};
use std::collections::{HashMap, HashSet};

const PRELUDE: &str = "fn f(a: i32) -> i32 { a }\nfn g(a: i32) -> i32 { a }\nfn keep(a: i32) -> bool { a > 1 }\nfn add(a: i32, b: i32) -> i32 { a + b }\nstruct Data { a: int[0, 5] }\n";

fn leading_ident(s: &str) -> Option<&str> {
    let end = s
        .char_indices()
        .find(|(_, c)| !(c.is_alphanumeric() || *c == '_'))
        .map_or(s.len(), |(i, _)| i);
    let ident = &s[..end];
    let starts_ok = ident
        .chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_');
    starts_ok.then_some(ident)
}

fn split_declaration(line: &str) -> Option<(&str, &str)> {
    let name = leading_ident(line)?;
    let rest = line[name.len()..].trim_start();
    let body = rest.strip_prefix('=')?;
    if body.starts_with('=') {
        return None;
    }
    Some((name, body.trim()))
}

fn is_passthrough(line: &str) -> bool {
    line.is_empty()
        || line.starts_with("fn ")
        || line.starts_with("struct ")
        || line.starts_with("enum ")
}

fn substitute(body: &str, defs: &HashMap<String, String>) -> String {
    let Some(ident) = leading_ident(body) else {
        return body.to_string();
    };
    let rest = &body[ident.len()..];
    let is_reference = rest.is_empty() || rest.starts_with('.');
    match defs.get(ident) {
        Some(definition) if is_reference => format!("{definition}{rest}"),
        _ => body.to_string(),
    }
}

fn inline(src: &str) -> String {
    let mut defs: HashMap<String, String> = HashMap::new();
    let mut out = Vec::new();
    for raw in src.lines() {
        let line = raw.trim();
        if is_passthrough(line) {
            out.push(line.to_string());
            continue;
        }
        match split_declaration(line) {
            Some((name, body)) => {
                let expanded = substitute(body, &defs);
                defs.insert(name.to_string(), expanded.clone());
                out.push(format!("{name} = {expanded}"));
            }
            None => out.push(substitute(line, &defs)),
        }
    }
    out.join("\n")
}

fn declared_names(src: &str) -> HashSet<String> {
    src.lines()
        .map(str::trim)
        .filter(|l| !is_passthrough(l))
        .filter_map(split_declaration)
        .map(|(n, _)| n.to_string())
        .collect()
}

fn references_a_variable(src: &str) -> bool {
    let names = declared_names(src);
    src.lines().map(str::trim).any(|line| {
        if is_passthrough(line) {
            return false;
        }
        let body = split_declaration(line).map_or(line, |(_, b)| b);
        leading_ident(body).is_some_and(|i| {
            names.contains(i) && {
                let rest = &body[i.len()..];
                rest.is_empty() || rest.starts_with('.')
            }
        })
    })
}

fn stream_diffs(label: &str, named: &StreamComplexity, inlined: &StreamComplexity) -> Vec<String> {
    let mut out = Vec::new();
    macro_rules! field {
        ($f:ident) => {
            if format!("{:?}", named.$f) != format!("{:?}", inlined.$f) {
                out.push(format!(
                    "{label}: {} named={:?} inline={:?}",
                    stringify!($f),
                    named.$f,
                    inlined.$f
                ));
            }
        };
    }
    field!(cardinality);
    field!(time_class);
    field!(exact_time);
    field!(space_class);
    field!(exact_space);
    field!(collects_input);
    out
}

fn program_diffs(named: &ProgramComplexity, inlined: &ProgramComplexity) -> Vec<String> {
    let mut out = Vec::new();
    if named.streams.len() != inlined.streams.len() {
        out.push(format!(
            "stream count named={} inline={}",
            named.streams.len(),
            inlined.streams.len()
        ));
        return out;
    }
    for (i, (n, t)) in named.streams.iter().zip(&inlined.streams).enumerate() {
        out.extend(stream_diffs(
            &format!("stream {i} ({})", n.description),
            n,
            t,
        ));
    }
    macro_rules! total {
        ($f:ident) => {
            if format!("{:?}", named.$f) != format!("{:?}", inlined.$f) {
                out.push(format!(
                    "{} named={:?} inline={:?}",
                    stringify!($f),
                    named.$f,
                    inlined.$f
                ));
            }
        };
    }
    total!(program_time);
    total!(program_exact_time);
    total!(program_space);
    total!(program_exact_space);
    total!(program_cardinality);
    out
}

fn node_diffs(named_src: &str, inline_src: &str) -> Vec<String> {
    let named = marigold_stream_complexities(named_src).expect("named program must parse");
    let inlined = marigold_stream_complexities(inline_src).expect("inline program must parse");
    let mut out = Vec::new();
    if named.len() != inlined.len() {
        out.push(format!(
            "node count named={} inline={}",
            named.len(),
            inlined.len()
        ));
        return out;
    }
    for (n, t) in named.iter().zip(&inlined) {
        if n.name != t.name || n.expr_index != t.expr_index {
            out.push(format!(
                "node identity named=({:?},{}) inline=({:?},{})",
                n.name, n.expr_index, t.name, t.expr_index
            ));
            continue;
        }
        let label = format!(
            "node #{} {}",
            n.expr_index,
            n.name.as_deref().unwrap_or("<output stream>")
        );
        out.extend(stream_diffs(&label, &n.complexity, &t.complexity));
    }
    out
}

fn assert_equivalent_pair(label: &str, named_src: &str, inline_src: &str) {
    let named = marigold_analyze(named_src)
        .unwrap_or_else(|e| panic!("{label}: named program failed to analyze: {e}"));
    let inlined = marigold_analyze(inline_src)
        .unwrap_or_else(|e| panic!("{label}: inline program failed to analyze: {e}"));
    let mut diffs = program_diffs(&named, &inlined);
    diffs.extend(node_diffs(named_src, inline_src));
    assert!(
        diffs.is_empty(),
        "{label}: named != inline\n--- named ---\n{named_src}\n--- inline ---\n{inline_src}\n--- differences ---\n{}",
        diffs.join("\n")
    );
}

fn assert_equivalent(label: &str, body: &str) {
    let named_src = format!("{PRELUDE}{body}");
    let inline_src = inline(&named_src);
    assert!(
        !references_a_variable(&inline_src),
        "{label}: generated twin still references a variable:\n{inline_src}"
    );
    assert_equivalent_pair(label, &named_src, &inline_src);
}

macro_rules! equivalence_cases {
    ($($(#[$meta:meta])* $name:ident => $body:expr;)*) => {
        $(
            $(#[$meta])*
            #[test]
            fn $name() {
                assert_equivalent(stringify!($name), $body);
            }
        )*
    };
}

mod inliner {
    use super::*;

    #[test]
    fn substitutes_a_consumer() {
        assert_eq!(
            inline("x = range(0, 5).map(f)\nx.map(g).return"),
            "x = range(0, 5).map(f)\nrange(0, 5).map(f).map(g).return"
        );
    }

    #[test]
    fn substitutes_a_zero_fun_consumer() {
        assert_eq!(
            inline("x = range(0, 5)\nx.return"),
            "x = range(0, 5)\nrange(0, 5).return"
        );
    }

    #[test]
    fn substitutes_through_a_chain() {
        assert_eq!(
            inline("x = range(0, 5)\ny = x.map(f)\nz = y.permutations(2)\nz.return"),
            "x = range(0, 5)\ny = range(0, 5).map(f)\nz = range(0, 5).map(f).permutations(2)\nrange(0, 5).map(f).permutations(2).return"
        );
    }

    #[test]
    fn substitutes_a_zero_fun_link() {
        assert_eq!(
            inline("x = range(0, 5)\ny = x\ny.return"),
            "x = range(0, 5)\ny = range(0, 5)\nrange(0, 5).return"
        );
    }

    #[test]
    fn leaves_function_style_sources_alone() {
        let src = "x = select_all(range(0, 2), range(0, 3))\nread_file(\"d.csv\", csv, struct=Data).ok().return";
        assert_eq!(
            inline(src),
            "x = select_all(range(0, 2), range(0, 3))\nread_file(\"d.csv\", csv, struct=Data).ok().return"
        );
    }

    #[test]
    fn leaves_undeclared_references_alone() {
        assert_eq!(inline("nope.map(f).return"), "nope.map(f).return");
    }

    #[test]
    fn does_not_substitute_a_prefix_of_a_longer_identifier() {
        assert_eq!(
            inline("x = range(0, 5)\nxs.map(f).return"),
            "x = range(0, 5)\nxs.map(f).return"
        );
    }

    #[test]
    fn redeclaration_uses_the_definition_in_scope_at_that_line() {
        assert_eq!(
            inline("x = range(0, 5)\nx.return\nx = range(0, 7)\nx.return"),
            "x = range(0, 5)\nrange(0, 5).return\nx = range(0, 7)\nrange(0, 7).return"
        );
    }

    #[test]
    fn self_referential_redeclaration_expands_the_prior_definition() {
        assert_eq!(
            inline("x = range(0, 5)\nx = x.permutations(2)\nx.return"),
            "x = range(0, 5)\nx = range(0, 5).permutations(2)\nrange(0, 5).permutations(2).return"
        );
    }

    #[test]
    fn passes_function_and_struct_lines_through() {
        let out = inline(PRELUDE);
        assert_eq!(out.lines().count(), PRELUDE.lines().count());
        assert!(out.contains("fn f(a: i32) -> i32 { a }"));
    }

    #[test]
    fn twin_of_a_variable_program_has_no_variable_references() {
        let src = format!("{PRELUDE}x = range(0, 5)\ny = x.map(f)\ny.return\nx.return");
        assert!(references_a_variable(&src));
        assert!(!references_a_variable(&inline(&src)));
    }
}

mod single_variable {
    use super::*;

    equivalence_cases! {
        zero_funs_return => "x = range(0, 5)\nx.return";
        #[ignore = "see #441"]
        streaming_var_zero_funs_return => "x = range(0, 5).map(f)\nx.return";
        streaming_var_streaming_consumer => "x = range(0, 5).map(f)\nx.map(g).return";
        #[ignore = "see #442"]
        filter_var_zero_funs_return => "x = range(0, 5).filter(keep)\nx.return";
        filter_var_streaming_consumer => "x = range(0, 5).filter(keep)\nx.map(f).return";
        #[ignore = "see #442"]
        filter_map_var_return => "x = range(0, 5).filter_map(f)\nx.return";
        #[ignore = "see #441"]
        permutations_var_zero_funs_return => "x = range(0, 5).permutations(2)\nx.return";
        #[ignore = "see #441"]
        permutations_var_streaming_consumer => "x = range(0, 5).permutations(2)\nx.map(f).return";
        #[ignore = "see #441"]
        permutations_var_filter_consumer => "x = range(0, 5).permutations(2)\nx.filter(keep).return";
        #[ignore = "see #441"]
        permutations_with_replacement_var_return => "x = range(0, 5).permutations_with_replacement(2)\nx.return";
        #[ignore = "see #441"]
        permutations_with_replacement_var_map => "x = range(0, 5).permutations_with_replacement(2)\nx.map(f).return";
        #[ignore = "see #441"]
        combinations_var_zero_funs_return => "x = range(0, 5).combinations(2)\nx.return";
        #[ignore = "see #441"]
        combinations_var_streaming_consumer => "x = range(0, 5).combinations(2)\nx.map(f).return";
        #[ignore = "see #441"]
        fold_var_zero_funs_return => "x = range(0, 5).fold(0, add)\nx.return";
        fold_var_streaming_consumer => "x = range(0, 5).fold(0, add)\nx.map(f).return";
        #[ignore = "see #441"]
        keep_first_n_var_zero_funs_return => "x = range(0, 100).keep_first_n(5, compare)\nx.return";
        keep_first_n_var_streaming_consumer => "x = range(0, 100).keep_first_n(5, compare)\nx.map(f).return";
        streaming_var_collecting_consumer => "x = range(0, 5).map(f)\nx.permutations(2).return";
        filter_var_collecting_consumer => "x = range(0, 5).filter(keep)\nx.permutations(2).return";
        collecting_var_collecting_consumer => "x = range(0, 5).permutations(2)\nx.combinations(2).return";
        fold_var_collecting_consumer => "x = range(0, 5).fold(0, add)\nx.permutations(2).return";
        combinations_var_collecting_consumer => "x = range(0, 6).combinations(2)\nx.permutations(2).return";
    }
}

mod chains {
    use super::*;

    equivalence_cases! {
        #[ignore = "see #441"]
        chain_of_two_streaming => "x = range(0, 5).map(f)\ny = x.map(g)\ny.return";
        #[ignore = "see #441"]
        chain_of_three_streaming => "x = range(0, 5).map(f)\ny = x.map(g)\nz = y.map(f)\nz.return";
        #[ignore = "see #441"]
        chain_of_three_zero_fun_consumer => "x = range(0, 5)\ny = x.permutations(2)\nz = y.map(f)\nz.return";
        #[ignore = "see #441"]
        chain_collecting_head => "x = range(0, 5).permutations(2)\ny = x.map(f)\ny.return";
        #[ignore = "see #441"]
        chain_collecting_middle => "x = range(0, 5)\ny = x.permutations(2)\nz = y.map(f)\nz.map(g).return";
        #[ignore = "see #441"]
        chain_collecting_tail => "x = range(0, 5)\ny = x.map(f)\nz = y.permutations(2)\nz.return";
        #[ignore = "see #441"]
        chain_zero_fun_head => "x = range(0, 5)\ny = x\nz = y.map(f)\nz.return";
        #[ignore = "see #441"]
        chain_zero_fun_link => "x = range(0, 5).permutations(2)\ny = x\ny.return";
        #[ignore = "see #441"]
        chain_zero_fun_link_streaming_consumer => "x = range(0, 5).permutations(2)\ny = x\ny.map(f).return";
        #[ignore = "see #441"]
        chain_of_four_with_fold => "x = range(0, 5).map(f)\ny = x.fold(0, add)\nz = y.map(g)\nw = z.map(f)\nw.return";
        #[ignore = "see #441"]
        chain_two_collecting_links => "x = range(0, 6)\ny = x.permutations(2)\nz = y.combinations(2)\nz.return";
        #[ignore = "see #441"]
        chain_consumer_zero_funs_collecting_head => "x = range(0, 5)\ny = x.permutations(2)\ny.return";
    }
}

mod fan_out {
    use super::*;

    equivalence_cases! {
        two_streaming_consumers => "x = range(0, 5)\nx.map(f).return\nx.map(g).return";
        three_streaming_consumers => "x = range(0, 5)\nx.map(f).return\nx.map(g).return\nx.filter(keep).return";
        zero_fun_and_streaming_consumer => "x = range(0, 5)\nx.return\nx.map(g).return";
        #[ignore = "see #441"]
        streaming_var_two_consumers => "x = range(0, 5).map(f)\nx.return\nx.map(g).return";
        #[ignore = "see #441"]
        collecting_var_two_streaming_consumers => "x = range(0, 5).permutations(2)\nx.map(f).return\nx.map(g).return";
        #[ignore = "see #441"]
        collecting_var_zero_fun_and_streaming_consumer => "x = range(0, 5).permutations(2)\nx.return\nx.map(g).return";
        collecting_consumer_and_streaming_consumer => "x = range(0, 5)\nx.permutations(2).return\nx.map(f).return";
        collecting_consumer_and_zero_fun_consumer => "x = range(0, 5)\nx.permutations(2).return\nx.return";
        fold_consumer_and_streaming_consumer => "x = range(0, 5)\nx.fold(0, add).return\nx.map(f).return";
        combinations_consumer_and_streaming_consumer => "x = range(0, 5)\nx.combinations(2).return\nx.filter(keep).return";
        collecting_var_with_collecting_and_streaming_consumers => "x = range(0, 5).map(f)\nx.permutations(2).return\nx.map(g).return";
        two_collecting_consumers => "x = range(0, 5)\nx.permutations(2).return\nx.combinations(2).return";
        #[ignore = "see #441"]
        same_consumer_twice => "x = range(0, 5).permutations(2)\nx.map(f).return\nx.map(f).return";
        #[ignore = "see #441"]
        same_zero_fun_consumer_twice => "x = range(0, 5).permutations(2)\nx.return\nx.return";
        same_collecting_consumer_twice => "x = range(0, 5)\nx.permutations(2).return\nx.permutations(2).return";
        same_streaming_consumer_three_times => "x = range(0, 5)\nx.map(f).return\nx.map(f).return\nx.map(f).return";
        unused_variable => "x = range(0, 5).permutations(2)";
        unused_chain => "x = range(0, 5)\ny = x.permutations(2)\nz = y.map(f)";
        used_and_unused_sibling => "x = range(0, 5)\ny = x.permutations(2)\nx.map(f).return";
    }
}

mod diamonds {
    use super::*;

    equivalence_cases! {
        #[ignore = "see #442"]
        diamond_streaming => "x = range(0, 5)\ny = x.map(f)\nz = x.filter(keep)\ny.return\nz.return";
        #[ignore = "see #441"]
        diamond_collecting_branch => "x = range(0, 5)\ny = x.permutations(2)\nz = x.map(f)\ny.return\nz.return";
        #[ignore = "see #441"]
        diamond_both_collecting => "x = range(0, 5)\ny = x.permutations(2)\nz = x.combinations(2)\ny.return\nz.return";
        #[ignore = "see #442"]
        diamond_collecting_root => "x = range(0, 5).permutations(2)\ny = x.map(f)\nz = x.filter(keep)\ny.return\nz.return";
        #[ignore = "see #441"]
        diamond_with_root_consumer => "x = range(0, 5)\ny = x.map(f)\nz = x.map(g)\nx.return\ny.return\nz.return";
        diamond_branches_rejoined_downstream => "x = range(0, 5)\ny = x.map(f)\nz = x.map(g)\ny.map(g).return\nz.map(f).return";
        #[ignore = "see #441"]
        fan_out_at_every_level => "x = range(0, 5)\ny = x.map(f)\nz = y.permutations(2)\nx.return\ny.return\nz.return\ny.map(g).return";
        #[ignore = "see #442"]
        diamond_over_collecting_chain => "x = range(0, 5)\ny = x.permutations(2)\na = y.map(f)\nb = y.filter(keep)\na.return\nb.return";
    }
}

mod outputs {
    use super::*;

    equivalence_cases! {
        #[ignore = "see #441"]
        write_file_streaming_var => "x = range(0, 3).map(f)\nx.write_file(\"out.csv\", csv)";
        write_file_zero_fun_consumer => "x = range(0, 3)\nx.write_file(\"out.csv\", csv)";
        #[ignore = "see #441"]
        write_file_collecting_var => "x = range(0, 3).permutations(2)\nx.write_file(\"out.csv\", csv)";
        write_file_collecting_consumer => "x = range(0, 3)\nx.permutations(2).write_file(\"out.csv\", csv)";
        #[ignore = "see #441"]
        return_and_write_file_consumers => "x = range(0, 3).map(f)\nx.return\nx.write_file(\"out.csv\", csv)";
        #[ignore = "see #441"]
        collecting_var_return_and_write_file => "x = range(0, 3).permutations(2)\nx.return\nx.map(f).write_file(\"out.csv\", csv)";
        two_write_file_consumers => "x = range(0, 3)\nx.map(f).write_file(\"a.csv\", csv)\nx.map(g).write_file(\"b.csv\", csv)";
        #[ignore = "see #441"]
        write_file_after_chain => "x = range(0, 3)\ny = x.permutations(2)\ny.map(f).write_file(\"out.csv\", csv)";
    }
}

mod sources {
    use super::*;

    equivalence_cases! {
        #[ignore = "see #441"]
        read_file_var_zero_funs_return => "x = read_file(\"d.csv\", csv, struct=Data).ok()\nx.return";
        read_file_var_streaming_consumer => "x = read_file(\"d.csv\", csv, struct=Data).ok()\nx.map(f).return";
        read_file_var_collecting_consumer => "x = read_file(\"d.csv\", csv, struct=Data).ok()\nx.permutations(2).return";
        #[ignore = "see #441"]
        read_file_collecting_var_zero_funs_return => "x = read_file(\"d.csv\", csv, struct=Data).permutations(2)\nx.return";
        #[ignore = "see #441"]
        read_file_var_chain => "x = read_file(\"d.csv\", csv, struct=Data).ok_or_panic()\ny = x.map(f)\ny.return";
        read_file_var_fan_out => "x = read_file(\"d.csv\", csv, struct=Data).ok()\nx.map(f).return\nx.permutations(2).return";
        #[ignore = "see #441"]
        read_file_var_write_file => "x = read_file(\"d.csv\", csv, struct=Data).ok_or_panic()\nx.write_file(\"o.csv\", csv)";
        select_all_var_zero_funs_return => "x = select_all(range(0, 10), range(0, 20))\nx.return";
        select_all_var_streaming_consumer => "x = select_all(range(0, 10), range(0, 20))\nx.map(f).return";
        select_all_var_collecting_consumer => "x = select_all(range(0, 3), range(0, 4))\nx.permutations(2).return";
        #[ignore = "see #441"]
        select_all_collecting_var_zero_funs_return => "x = select_all(range(0, 3), range(0, 4)).permutations(2)\nx.return";
        select_all_var_fan_out => "x = select_all(range(0, 10), range(0, 20))\nx.map(f).return\nx.filter(keep).return";
        zero_fun_link_over_select_all => "x = select_all(range(0, 3), range(0, 4))\ny = x\ny.return";
    }
}

mod scoping {
    use super::*;

    equivalence_cases! {
        undeclared_consumer => "nope.map(f).return";
        undeclared_zero_fun_consumer => "nope.return";
        #[ignore = "see #441"]
        undeclared_variable_source => "y = nope.map(f)\ny.return";
        undeclared_alongside_declared => "x = range(0, 5)\nx.return\nnope.map(f).return";
    }
}

mod redeclaration {
    use super::*;
    use marigold_grammar::marigold_check;

    fn assert_rejected(body: &str) {
        let src = format!("{PRELUDE}{body}");
        let diagnostics = marigold_check(&src);
        assert!(
            diagnostics.iter().any(|d| d.is_error()),
            "redeclaration must be a diagnostic error, got {diagnostics:?}\n{src}"
        );
    }

    macro_rules! rejected_cases {
        ($($(#[$meta:meta])* $name:ident => $body:expr;)*) => {
            $(
                $(#[$meta])*
                #[test]
                fn $name() {
                    assert_rejected($body);
                }
            )*
        };
    }

    rejected_cases! {
        #[ignore = "see #444"]
        redeclaration_streaming => "x = range(0, 5)\nx.return\nx = range(0, 7)\nx.return";
        #[ignore = "see #444"]
        redeclaration_to_collecting => "x = range(0, 5)\nx.return\nx = range(0, 7).permutations(2)\nx.return";
        #[ignore = "see #444"]
        redeclaration_from_collecting => "x = range(0, 5).permutations(2)\nx.return\nx = range(0, 7)\nx.map(f).return";
        #[ignore = "see #444"]
        self_referential_redeclaration => "x = range(0, 5)\nx = x.permutations(2)\nx.return";
        #[ignore = "see #444"]
        self_referential_redeclaration_streaming_consumer => "x = range(0, 5)\nx = x.map(f)\nx.map(g).return";
        #[ignore = "see #444"]
        redeclaration_in_chain => "x = range(0, 5)\ny = x.permutations(2)\nx = range(0, 9)\nz = y.map(f)\nz.return\nx.return";
        #[ignore = "see #444"]
        redeclaration_in_the_middle_of_consumers => "x = range(0, 5)\nx.return\nx = range(0, 7).permutations(2)\nx.return";
    }

    #[test]
    fn distinct_names_are_not_flagged() {
        let src = format!("{PRELUDE}x = range(0, 5)\ny = x.map(f)\nx.return\ny.return");
        assert!(!marigold_check(&src).iter().any(|d| d.is_error()));
    }
}

mod shadowing_semantics {
    use super::*;

    fn last_cardinality(body: &str) -> String {
        let src = format!("{PRELUDE}{body}");
        marigold_analyze(&src)
            .unwrap()
            .streams
            .last()
            .unwrap()
            .cardinality
            .to_string()
    }

    #[test]
    fn consumer_before_a_redeclaration_sees_the_last_declaration() {
        assert_eq!(
            last_cardinality("x = range(0, 5)\nx.map(f).return\nx = range(0, 100).filter(keep)\n"),
            "\u{2264}100"
        );
    }

    #[test]
    fn variable_from_variable_reads_the_preceding_declaration() {
        assert_eq!(
            last_cardinality(
                "x = range(0, 5)\ny = x.map(f)\nx = range(0, 100).filter(keep)\ny.return\n"
            ),
            "5"
        );
    }

    #[test]
    fn variable_from_variable_reads_its_own_previous_binding() {
        assert_eq!(
            last_cardinality("x = range(0, 5)\nx = x.map(f)\nx.return\n"),
            "5"
        );
    }

    #[test]
    fn variable_from_a_name_declared_only_later_is_an_undeclared_source() {
        assert_eq!(
            last_cardinality("y = x.map(f)\nx = range(0, 5)\ny.return\n"),
            "?"
        );
    }
}

mod absolute_contracts {
    use super::*;

    fn only_stream(body: &str) -> StreamComplexity {
        let r = marigold_analyze(&format!("{PRELUDE}{body}")).unwrap();
        assert_eq!(r.streams.len(), 1, "{body}");
        r.streams[0].clone()
    }

    #[ignore = "see #441"]
    #[test]
    fn named_collecting_var_reports_collects_input() {
        let s = only_stream("x = range(0, 5).permutations(2)\nx.map(f).return");
        assert!(s.collects_input);
    }

    #[ignore = "see #441"]
    #[test]
    fn named_collecting_var_zero_funs_reports_collects_input() {
        let s = only_stream("x = range(0, 5).permutations(2)\nx.return");
        assert!(s.collects_input);
    }

    #[ignore = "see #441"]
    #[test]
    fn collects_input_propagates_through_every_link_of_a_chain() {
        let s = only_stream(
            "x = range(0, 5)\ny = x.permutations(2)\nz = y.map(f)\nw = z.map(g)\nw.return",
        );
        assert!(s.collects_input);
    }

    #[ignore = "see #441"]
    #[test]
    fn zero_fun_consumer_adds_no_time_of_its_own() {
        let named = only_stream("x = range(0, 5).permutations(2)\nx.return");
        let inlined = only_stream("range(0, 5).permutations(2).return");
        assert_eq!(named.exact_time.to_string(), inlined.exact_time.to_string());
        assert_eq!(named.exact_time.to_string(), "O(1)");
    }

    #[test]
    fn variable_with_no_consumers_produces_no_stream() {
        let r = marigold_analyze(&format!("{PRELUDE}x = range(0, 5).permutations(2)")).unwrap();
        assert!(r.streams.is_empty());
    }

    #[test]
    fn undeclared_variable_is_unknown_and_not_collecting() {
        let s = only_stream("nope.map(f).return");
        assert_eq!(s.space_class, ComplexityClass::Unknown);
        assert_eq!(s.time_class, ComplexityClass::Unknown);
        assert!(!s.collects_input);
    }

    #[test]
    fn fan_out_counts_each_consumer_as_if_inline() {
        let named = marigold_analyze(&format!(
            "{PRELUDE}x = range(0, 5).map(f)\nx.map(g).return\nx.map(g).return"
        ))
        .unwrap();
        let single =
            marigold_analyze(&format!("{PRELUDE}range(0, 5).map(f).map(g).return")).unwrap();
        assert_eq!(named.streams.len(), 2);
        for s in &named.streams {
            assert_eq!(s.exact_time, single.streams[0].exact_time);
            assert_eq!(s.exact_space, single.streams[0].exact_space);
        }
        let mut doubled = single.program_exact_time.clone();
        doubled.merge(&single.program_exact_time);
        assert_eq!(named.program_exact_time, doubled);
    }
}

mod generated {
    use super::*;
    use proptest::prelude::*;

    fn source() -> impl Strategy<Value = String> {
        prop_oneof![
            Just("range(0, 5)".to_string()),
            Just("range(0, 8)".to_string()),
            Just("read_file(\"d.csv\", csv, struct=Data).ok()".to_string()),
            Just("select_all(range(0, 3), range(0, 4))".to_string()),
        ]
    }

    fn op() -> impl Strategy<Value = String> {
        prop_oneof![
            Just("map(f)".to_string()),
            Just("map(g)".to_string()),
            Just("filter(keep)".to_string()),
            Just("permutations(2)".to_string()),
            Just("combinations(2)".to_string()),
            Just("permutations_with_replacement(2)".to_string()),
            Just("fold(0, add)".to_string()),
        ]
    }

    fn ops(max: usize) -> impl Strategy<Value = Vec<String>> {
        proptest::collection::vec(op(), 0..=max)
    }

    fn chain(base: &str, ops: &[String]) -> String {
        ops.iter()
            .fold(base.to_string(), |acc, o| format!("{acc}.{o}"))
    }

    fn program() -> impl Strategy<Value = String> {
        (
            source(),
            ops(2),
            proptest::option::of(ops(2)),
            proptest::collection::vec((0usize..2, ops(2), any::<bool>()), 1..4),
        )
            .prop_map(|(src, x_ops, y_ops, consumers)| {
                let mut lines = vec![format!("x = {}", chain(&src, &x_ops))];
                let has_y = y_ops.is_some();
                if let Some(y_ops) = &y_ops {
                    lines.push(format!("y = {}", chain("x", y_ops)));
                }
                for (target, c_ops, writes) in consumers {
                    let var = if target == 1 && has_y { "y" } else { "x" };
                    let out = if writes {
                        "write_file(\"o.csv\", csv)"
                    } else {
                        "return"
                    };
                    lines.push(format!("{}.{out}", chain(var, &c_ops)));
                }
                lines.join("\n")
            })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(256))]
        #[ignore = "see #441"]
        #[test]
        fn random_named_programs_match_their_inlined_twin(body in program()) {
            let named_src = format!("{PRELUDE}{body}");
            let inline_src = inline(&named_src);
            prop_assert!(!references_a_variable(&inline_src));
            let named = marigold_analyze(&named_src);
            let inlined = marigold_analyze(&inline_src);
            prop_assert_eq!(named.is_ok(), inlined.is_ok());
            if let (Ok(n), Ok(t)) = (named, inlined) {
                let mut diffs = program_diffs(&n, &t);
                diffs.extend(node_diffs(&named_src, &inline_src));
                prop_assert!(diffs.is_empty(), "{}\n{}", named_src, diffs.join("\n"));
            }
        }
    }
}
