use marigold_grammar::complexity::{Cardinality, ComplexityClass};
use num_bigint::BigUint;
use num_traits::One;
use proptest::prelude::*;

/// Generate a valid identifier for use as a closure name.
fn arb_identifier() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("f".to_string()),
        Just("g".to_string()),
        Just("h".to_string()),
        Just("my_fn".to_string()),
    ]
}

/// Represents a stream function with metadata for invariant checking.
#[derive(Debug, Clone)]
struct StreamFn {
    text: String,
    is_collecting: bool,
    is_fold: bool,
}

/// Generate a single streaming (non-collecting) stream function.
fn arb_streaming_fn() -> impl Strategy<Value = StreamFn> {
    prop_oneof![
        arb_identifier().prop_map(|id| StreamFn {
            text: format!("map({id})"),
            is_collecting: false,
            is_fold: false,
        }),
        arb_identifier().prop_map(|id| StreamFn {
            text: format!("filter({id})"),
            is_collecting: false,
            is_fold: false,
        }),
        arb_identifier().prop_map(|id| StreamFn {
            text: format!("filter_map({id})"),
            is_collecting: false,
            is_fold: false,
        }),
    ]
}

/// Generate a collecting stream function.
fn arb_collecting_fn() -> impl Strategy<Value = StreamFn> {
    prop_oneof![
        (1u64..5).prop_map(|k| StreamFn {
            text: format!("permutations({k})"),
            is_collecting: true,
            is_fold: false,
        }),
        (1u64..5).prop_map(|k| StreamFn {
            text: format!("combinations({k})"),
            is_collecting: true,
            is_fold: false,
        }),
        (1u64..4).prop_map(|k| StreamFn {
            text: format!("permutations_with_replacement({k})"),
            is_collecting: true,
            is_fold: false,
        }),
    ]
}

/// Generate a fold stream function.
fn arb_fold_fn() -> impl Strategy<Value = StreamFn> {
    arb_identifier().prop_map(|id| StreamFn {
        text: format!("fold(0, {id})"),
        is_collecting: false,
        is_fold: true,
    })
}

/// Generate any stream function.
fn arb_stream_fn() -> impl Strategy<Value = StreamFn> {
    prop_oneof![
        6 => arb_streaming_fn(),
        3 => arb_collecting_fn(),
        1 => arb_fold_fn(),
    ]
}

/// Generate a chain of stream functions (0..4 functions).
fn arb_stream_fn_chain() -> impl Strategy<Value = Vec<StreamFn>> {
    proptest::collection::vec(arb_stream_fn(), 0..5)
}

/// Generate a range input function string with at least 2 elements.
fn arb_range_input() -> impl Strategy<Value = String> {
    (0u32..50, 2u32..100).prop_map(|(start, count)| {
        let end = start + count; // end - start >= 2, so cardinality >= 2
        format!("range({start}, {end})")
    })
}

/// Generate a select_all input with 2-3 ranges.
fn arb_select_all_input() -> impl Strategy<Value = String> {
    proptest::collection::vec(arb_range_input(), 2..4).prop_map(|ranges| {
        format!("select_all({})", ranges.join(", "))
    })
}

/// Generate an input function string.
fn arb_input() -> impl Strategy<Value = String> {
    prop_oneof![
        3 => arb_range_input(),
        1 => arb_select_all_input(),
    ]
}

/// Result of generating a program, with metadata for checking invariants.
#[derive(Debug, Clone)]
struct GeneratedProgram {
    source: String,
    /// Number of streams with output functions (return/write_file).
    expected_stream_count: usize,
    /// Per-stream: whether the output chain has collecting ops.
    /// Note: for named streams, this only reflects the output chain's ops,
    /// not the variable definition's ops, matching `collects_input` semantics.
    stream_output_has_collecting: Vec<bool>,
    /// Per-stream: whether the last op is a fold (after all other ops).
    stream_ends_with_fold: Vec<bool>,
    /// Per-stream: whether it's purely streaming ops (no collecting, no fold) in the full pipeline.
    stream_only_streaming: Vec<bool>,
}

/// Generate an unnamed stream program: `input.chain.return`
fn arb_unnamed_stream_program() -> impl Strategy<Value = GeneratedProgram> {
    (arb_input(), arb_stream_fn_chain()).prop_map(|(input, chain)| {
        let has_collecting = chain.iter().any(|f| f.is_collecting);
        let ends_with_fold = chain.last().map_or(false, |f| f.is_fold);
        let only_streaming = chain.iter().all(|f| !f.is_collecting && !f.is_fold);

        let chain_str = chain
            .iter()
            .map(|f| format!(".{}", f.text))
            .collect::<String>();
        let source = format!("{input}{chain_str}.return");

        GeneratedProgram {
            source,
            expected_stream_count: 1,
            stream_output_has_collecting: vec![has_collecting],
            stream_ends_with_fold: vec![ends_with_fold],
            stream_only_streaming: vec![only_streaming],
        }
    })
}

/// Generate a stream variable program: `var = input.chain\nvar.chain.return`
fn arb_stream_var_program() -> impl Strategy<Value = GeneratedProgram> {
    (
        arb_input(),
        arb_stream_fn_chain(),
        arb_stream_fn_chain(),
    )
        .prop_map(|(input, var_chain, out_chain)| {
            // collects_input only reflects the output chain's collecting ops for named streams
            let output_has_collecting = out_chain.iter().any(|f| f.is_collecting);
            let ends_with_fold = out_chain
                .last()
                .or(var_chain.last())
                .map_or(false, |f| f.is_fold);
            let only_streaming = var_chain
                .iter()
                .chain(out_chain.iter())
                .all(|f| !f.is_collecting && !f.is_fold);

            let var_chain_str = var_chain
                .iter()
                .map(|f| format!(".{}", f.text))
                .collect::<String>();
            let out_chain_str = out_chain
                .iter()
                .map(|f| format!(".{}", f.text))
                .collect::<String>();

            let source = format!("my_stream = {input}{var_chain_str}\nmy_stream{out_chain_str}.return");

            GeneratedProgram {
                source,
                expected_stream_count: 1,
                stream_output_has_collecting: vec![output_has_collecting],
                stream_ends_with_fold: vec![ends_with_fold],
                stream_only_streaming: vec![only_streaming],
            }
        })
}

/// Generate a multi-consumer program: `var = input.chain\nvar.chain1.return\nvar.chain2.return`
fn arb_multi_consumer_program() -> impl Strategy<Value = GeneratedProgram> {
    (
        arb_input(),
        arb_stream_fn_chain(),
        arb_stream_fn_chain(),
        arb_stream_fn_chain(),
    )
        .prop_map(|(input, var_chain, chain1, chain2)| {
            let var_chain_str = var_chain
                .iter()
                .map(|f| format!(".{}", f.text))
                .collect::<String>();
            let chain1_str = chain1
                .iter()
                .map(|f| format!(".{}", f.text))
                .collect::<String>();
            let chain2_str = chain2
                .iter()
                .map(|f| format!(".{}", f.text))
                .collect::<String>();

            // collects_input only reflects each output chain's own collecting ops
            let output_has_collecting_1 = chain1.iter().any(|f| f.is_collecting);
            let output_has_collecting_2 = chain2.iter().any(|f| f.is_collecting);
            let ends_fold_1 = chain1
                .last()
                .or(var_chain.last())
                .map_or(false, |f| f.is_fold);
            let ends_fold_2 = chain2
                .last()
                .or(var_chain.last())
                .map_or(false, |f| f.is_fold);
            let only_streaming_1 = var_chain
                .iter()
                .chain(chain1.iter())
                .all(|f| !f.is_collecting && !f.is_fold);
            let only_streaming_2 = var_chain
                .iter()
                .chain(chain2.iter())
                .all(|f| !f.is_collecting && !f.is_fold);

            let source = format!(
                "data = {input}{var_chain_str}\ndata{chain1_str}.return\ndata{chain2_str}.return"
            );

            GeneratedProgram {
                source,
                expected_stream_count: 2,
                stream_output_has_collecting: vec![output_has_collecting_1, output_has_collecting_2],
                stream_ends_with_fold: vec![ends_fold_1, ends_fold_2],
                stream_only_streaming: vec![only_streaming_1, only_streaming_2],
            }
        })
}

/// Generate any valid program shape.
fn arb_program() -> impl Strategy<Value = GeneratedProgram> {
    prop_oneof![
        4 => arb_unnamed_stream_program(),
        3 => arb_stream_var_program(),
        2 => arb_multi_consumer_program(),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(512))]

    /// Every generated program should analyze without errors or panics.
    #[test]
    fn analyze_never_panics(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source);
        prop_assert!(result.is_ok(), "Failed to analyze program:\n{}\nError: {:?}", prog.source, result.err());
    }

    /// The number of streams in the result matches expected count.
    #[test]
    fn stream_count_matches(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        prop_assert_eq!(
            result.streams.len(),
            prog.expected_stream_count,
            "Program:\n{}\nExpected {} streams, got {}",
            prog.source,
            prog.expected_stream_count,
            result.streams.len()
        );
    }

    /// For each stream, time_class == exact_time.simplified().
    #[test]
    fn time_class_equals_simplified_exact_time(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            prop_assert_eq!(
                stream.time_class.clone(),
                stream.exact_time.simplified(),
                "Stream {} time_class mismatch in program:\n{}",
                i,
                prog.source
            );
        }
    }

    /// program_time >= every stream's time_class.
    #[test]
    fn program_time_dominates_streams(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            prop_assert!(
                result.program_time >= stream.time_class,
                "program_time {:?} < stream {} time {:?} in program:\n{}",
                result.program_time,
                i,
                stream.time_class,
                prog.source
            );
        }
    }

    /// program_space >= every stream's space_class.
    #[test]
    fn program_space_dominates_streams(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            prop_assert!(
                result.program_space >= stream.space_class,
                "program_space {:?} < stream {} space {:?} in program:\n{}",
                result.program_space,
                i,
                stream.space_class,
                prog.source
            );
        }
    }

    /// Streams with only streaming ops (map, filter, filter_map) have O(1) space.
    #[test]
    fn streaming_only_has_o1_space(prog in arb_unnamed_stream_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            if prog.stream_only_streaming[i] {
                prop_assert_eq!(
                    stream.space_class.clone(),
                    ComplexityClass::O1,
                    "Streaming-only stream {} should have O(1) space, got {:?} in program:\n{}",
                    i,
                    stream.space_class.clone(),
                    prog.source
                );
            }
        }
    }

    /// collects_input matches the presence of collecting ops in the output chain.
    /// For named streams, collects_input only reflects the output chain's ops (not variable def).
    #[test]
    fn collects_input_matches_collecting_ops(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            if i < prog.stream_output_has_collecting.len() {
                prop_assert_eq!(
                    stream.collects_input,
                    prog.stream_output_has_collecting[i],
                    "Stream {} collects_input mismatch in program:\n{}",
                    i,
                    prog.source
                );
            }
        }
    }

    /// A stream ending with fold should have cardinality Exact(1).
    #[test]
    fn fold_produces_cardinality_one(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            if i < prog.stream_ends_with_fold.len() && prog.stream_ends_with_fold[i] {
                prop_assert_eq!(
                    stream.cardinality.clone(),
                    Cardinality::Exact(BigUint::one()),
                    "Stream {} ending with fold should have cardinality 1, got {:?} in program:\n{}",
                    i,
                    stream.cardinality.clone(),
                    prog.source
                );
            }
        }
    }

    /// program_cardinality >= every stream's cardinality.
    #[test]
    fn program_cardinality_dominates_streams(prog in arb_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for (i, stream) in result.streams.iter().enumerate() {
            prop_assert!(
                result.program_cardinality >= stream.cardinality,
                "program_cardinality {:?} < stream {} cardinality {:?} in program:\n{}",
                result.program_cardinality,
                i,
                stream.cardinality,
                prog.source
            );
        }
    }

    /// Range-based streams with at least 2 elements should have time >= O(n).
    /// (Ranges are generated with count >= 2, so cardinality >= 2.)
    #[test]
    fn range_programs_have_at_least_on_time(prog in arb_unnamed_stream_program()) {
        let result = marigold_grammar::marigold_analyze(&prog.source).unwrap();
        for stream in &result.streams {
            prop_assert!(
                stream.time_class >= ComplexityClass::ON,
                "Range-based stream should have time >= O(n), got {:?} in program:\n{}",
                stream.time_class,
                prog.source
            );
        }
    }
}
