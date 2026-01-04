//! Performance benchmarks comparing LALRPOP and Pest parser implementations.
//!
//! This benchmark suite validates that the Pest parser maintains performance
//! within 10% of the LALRPOP baseline (Phase 2 success criteria).
//!
//! # Running Benchmarks
//!
//! Run LALRPOP benchmarks (default):
//! ```bash
//! cargo bench --bench parser_bench
//! ```
//!
//! Run Pest benchmarks:
//! ```bash
//! cargo bench --bench parser_bench --features pest-parser
//! ```
//!
//! Compare both parsers:
//! ```bash
//! cargo bench --bench parser_bench && \
//! cargo bench --bench parser_bench --features pest-parser
//! ```
//!
//! # Benchmark Categories
//!
//! - **Simple**: Basic constructs (range, return)
//! - **Complex**: Structs, enums, functions with chaining
//! - **Real-world**: Examples from integration tests

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use marigold_grammar::parser::{LalrpopParser, MarigoldParser};

#[cfg(feature = "pest-parser")]
use marigold_grammar::parser::PestParser;

/// Benchmark simple range with return
fn bench_simple_range_return(c: &mut Criterion) {
    let input = "range(0, 100).return";

    c.bench_function("lalrpop_simple_range_return", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_simple_range_return", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark range with map and return
fn bench_range_map_return(c: &mut Criterion) {
    let input = r#"
        fn double(v: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            v * 2
        %%%MARIGOLD_FUNCTION_END%%%

        range(0, 100)
            .map(double)
            .return
    "#;

    c.bench_function("lalrpop_range_map_return", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_range_map_return", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark chained stream operations
fn bench_chained_operations(c: &mut Criterion) {
    let input = r#"
        range(0, 10)
            .permutations(2)
            .combinations(2)
            .return
    "#;

    c.bench_function("lalrpop_chained_operations", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_chained_operations", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark filter operation
fn bench_filter_operation(c: &mut Criterion) {
    let input = r#"
        fn is_odd_number(i: &i32) -> bool %%%MARIGOLD_FUNCTION_START%%%
            i % 2 == 1
        %%%MARIGOLD_FUNCTION_END%%%

        range(0, 100)
            .filter(is_odd_number)
            .return
    "#;

    c.bench_function("lalrpop_filter_operation", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_filter_operation", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark struct declaration
fn bench_struct_declaration(c: &mut Criterion) {
    let input = r#"
        struct Point {
            x: i32,
            y: i32,
        }

        range(0, 10).return
    "#;

    c.bench_function("lalrpop_struct_declaration", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_struct_declaration", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark enum declaration with string values
fn bench_enum_declaration(c: &mut Criterion) {
    let input = r#"
        enum Hull {
            Spherical = "spherical",
            Split = "split",
        }

        struct Ship {
            name: string_8,
            hull: Hull,
        }

        range(0, 5).return
    "#;

    c.bench_function("lalrpop_enum_declaration", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_enum_declaration", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark enum with default variant
fn bench_enum_default_variant(c: &mut Criterion) {
    let input = r#"
        enum Color {
            Red = "red",
            Green = "green",
            Blue = "blue",
            default Other,
        }

        range(0, 5).return
    "#;

    c.bench_function("lalrpop_enum_default_variant", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_enum_default_variant", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark stream variable declaration
fn bench_stream_variable(c: &mut Criterion) {
    let input = r#"
        x = range(0, 100)
    "#;

    c.bench_function("lalrpop_stream_variable", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_stream_variable", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark permutations with replacement
fn bench_permutations_with_replacement(c: &mut Criterion) {
    let input = r#"
        range(0, 10)
            .permutations_with_replacement(3)
            .return
    "#;

    c.bench_function("lalrpop_permutations_with_replacement", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_permutations_with_replacement", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark keep_first_n with external function
fn bench_keep_first_n(c: &mut Criterion) {
    let input = r#"
        fn sorter(a: &Vec<i32>, b: &Vec<i32>) -> Ordering %%%MARIGOLD_FUNCTION_START%%%
            a[0].cmp(&b[0])
        %%%MARIGOLD_FUNCTION_END%%%

        range(0, 20)
            .permutations(2)
            .keep_first_n(10, sorter)
            .return
    "#;

    c.bench_function("lalrpop_keep_first_n", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_keep_first_n", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark complex example from CSV integration test
fn bench_complex_csv_example(c: &mut Criterion) {
    let input = r#"
        enum Hull {
            Spherical = "spherical",
            Split = "split",
        }

        struct Vaisseau {
            class: string_8,
            hull: Hull,
        }

        fn is_spherical(v: &Vaisseau) -> bool %%%MARIGOLD_FUNCTION_START%%%
            match v.hull {
                Hull::Spherical => true,
                _ => false
            }
        %%%MARIGOLD_FUNCTION_END%%%

        range(0, 100)
            .filter(is_spherical)
            .return
    "#;

    c.bench_function("lalrpop_complex_csv_example", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_complex_csv_example", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark complex example with multiple streams
fn bench_multiple_streams(c: &mut Criterion) {
    let input = r#"
        enum Hull {
            Spherical = "spherical",
            Split = "split",
        }

        struct Vaisseau {
            class: string_8,
            hull: Hull,
        }

        range(0, 50)
            .return

        range(50, 100)
            .return
    "#;

    c.bench_function("lalrpop_multiple_streams", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_multiple_streams", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark write_file operation
fn bench_write_file(c: &mut Criterion) {
    let input = r#"
        range(0, 100).write_file("output.csv", csv)
    "#;

    c.bench_function("lalrpop_write_file", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_write_file", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark empty input (baseline)
fn bench_empty_input(c: &mut Criterion) {
    let input = "";

    c.bench_function("lalrpop_empty_input", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_empty_input", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark deeply nested function calls
fn bench_deeply_nested(c: &mut Criterion) {
    let input = r#"
        fn process_a(v: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%% v + 1 %%%MARIGOLD_FUNCTION_END%%%
        fn process_b(v: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%% v * 2 %%%MARIGOLD_FUNCTION_END%%%
        fn process_c(v: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%% v - 3 %%%MARIGOLD_FUNCTION_END%%%
        fn filter_even(v: &i32) -> bool %%%MARIGOLD_FUNCTION_START%%% v % 2 == 0 %%%MARIGOLD_FUNCTION_END%%%

        range(0, 100)
            .map(process_a)
            .map(process_b)
            .map(process_c)
            .filter(filter_even)
            .permutations(2)
            .combinations(2)
            .return
    "#;

    c.bench_function("lalrpop_deeply_nested", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_deeply_nested", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

/// Benchmark color palette picker example (real-world use case)
fn bench_color_palette_picker(c: &mut Criterion) {
    let input = r#"
        fn compare_contrast(a: &Vec<Vec<u8>>, b: &Vec<Vec<u8>>) -> Ordering %%%MARIGOLD_FUNCTION_START%%%
            // Comparison logic placeholder
            Ordering::Equal
        %%%MARIGOLD_FUNCTION_END%%%

        range(0, 255)
            .permutations_with_replacement(3)
            .combinations(5)
            .keep_first_n(20, compare_contrast)
            .return
    "#;

    c.bench_function("lalrpop_color_palette_picker", |b| {
        let parser = LalrpopParser::new();
        b.iter(|| {
            parser
                .parse(black_box(input))
                .expect("LALRPOP parse failed")
        });
    });

    #[cfg(feature = "pest-parser")]
    c.bench_function("pest_color_palette_picker", |b| {
        let parser = PestParser::new();
        b.iter(|| parser.parse(black_box(input)).expect("Pest parse failed"));
    });
}

// Group all benchmarks
criterion_group!(
    benches,
    bench_empty_input,
    bench_simple_range_return,
    bench_range_map_return,
    bench_chained_operations,
    bench_filter_operation,
    bench_struct_declaration,
    bench_enum_declaration,
    bench_enum_default_variant,
    bench_stream_variable,
    bench_permutations_with_replacement,
    bench_keep_first_n,
    bench_write_file,
    bench_complex_csv_example,
    bench_multiple_streams,
    bench_deeply_nested,
    bench_color_palette_picker
);

criterion_main!(benches);
