use marigold_grammar::source_map::{GeneratedProgram, SegmentKind as K};
use marigold_grammar::{marigold_generate, marigold_parse};

fn generate(src: &str) -> GeneratedProgram {
    let program = marigold_generate(src).unwrap_or_else(|e| panic!("{src:?}: {e}"));
    assert_eq!(program.rust, marigold_parse(src).unwrap(), "{src:?}");
    program
}

fn slices<'a>(src: &'a str, g: &'a GeneratedProgram) -> Vec<(K, &'a str, &'a str)> {
    g.map
        .segments()
        .iter()
        .map(|s| {
            (
                s.kind,
                &src[s.source.start..s.source.end],
                &g.rust[s.generated.start..s.generated.end],
            )
        })
        .collect()
}

#[test]
fn a_named_stream_with_a_collecting_step_is_deferred_and_mapped() {
    let src = "x = range(0, 5)\nx.permutations(2).return";
    let g = generate(src);
    assert!(g
        .rust
        .contains("{ let __marigold_receiver = x.get(); ::marigold::marigold_impl::futures::StreamExt::flatten("));
    let rows = slices(src, &g);
    let stream = rows.iter().find(|r| r.0 == K::ReturningStream).unwrap();
    assert_eq!(stream.1, "x.permutations(2).return");
    assert!(stream.2.contains("{ let __marigold_receiver = x.get();"));
    assert!(stream
        .2
        .contains("async move { __marigold_receiver.permutations(2)"));
    let input = rows.iter().find(|r| r.0 == K::InputVariable).unwrap();
    assert_eq!((input.1, input.2), ("x", "x.get()"));
    let step = rows.iter().find(|r| r.0 == K::StreamPermutations).unwrap();
    assert_eq!(step.1, "permutations(2)");
    assert!(step.2.starts_with("permutations(2)"));
}

#[test]
fn a_from_prior_declaration_with_a_collecting_step_is_pinned_and_mapped() {
    let src = "x = range(0, 5)\ny = x.combinations(2)\ny.return";
    let g = generate(src);
    let rows = slices(src, &g);
    let decl = rows
        .iter()
        .filter(|r| r.0 == K::StreamVariable)
        .nth(1)
        .unwrap();
    assert_eq!(decl.1, "y = x.combinations(2)");
    assert!(decl.2.starts_with("let mut y = "));
    assert!(decl
        .2
        .contains("{ let __marigold_receiver = x.get(); Box::pin("));
    let inputs: Vec<_> = rows.iter().filter(|r| r.0 == K::InputVariable).collect();
    assert_eq!((inputs[0].1, inputs[0].2), ("x", "x.get()"));
    let step = rows.iter().find(|r| r.0 == K::StreamCombinations).unwrap();
    assert_eq!(step.1, "combinations(2)");
}

#[test]
fn a_named_stream_without_a_collecting_step_is_not_deferred() {
    let src = "x = range(0, 5)\nx.map(f).return";
    let g = generate(src);
    assert!(!g.rust.contains("__marigold_receiver"));
    let rows = slices(src, &g);
    assert!(rows.contains(&(K::InputVariable, "x", "x.get()")));
}

#[test]
fn a_deferred_write_file_sink_keeps_both_output_regions() {
    let src = "x = range(0, 5)\nx.keep_first_n(3, cmp).write_file(\"o.csv\", csv)";
    let g = generate(src);
    let rows = slices(src, &g);
    let sinks: Vec<_> = rows.iter().filter(|r| r.0 == K::OutputWriteFile).collect();
    assert_eq!(sinks.len(), 2);
    assert!(sinks.iter().all(|r| r.1.starts_with("write_file(")));
    assert!(rows.contains(&(K::InputVariable, "x", "x.get()")));
}

#[test]
fn a_redeclared_stream_variable_is_the_same_error_for_parse_and_generate() {
    let src = "x = range(0, 1)\nx = range(1, 2)\nx.return";
    let parse = marigold_parse(src).unwrap_err().0;
    let generate = marigold_generate(src).unwrap_err().0;
    assert_eq!(parse, generate);
    assert!(parse.contains("declared more than once"), "{parse}");
}
