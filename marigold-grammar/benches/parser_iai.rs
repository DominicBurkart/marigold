use iai_callgrind::{library_benchmark, library_benchmark_group, main};
use marigold_grammar::parser::{MarigoldParser, MarigoldParseError, PestParser};
use std::hint::black_box;

const MULTIPLE_STREAMS: &str = r#"
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

const COMPLEX_CSV: &str = r#"
    enum Hull {
        Spherical = "spherical",
        Split = "split",
    }

    struct Vaisseau {
        class: string_8,
        hull: Hull,
    }

    fn is_spherical(v: &Vaisseau) -> bool { match v.hull { Hull::Spherical => true, _ => false } }

    range(0, 100)
        .filter(is_spherical)
        .return
"#;

const DEEPLY_NESTED: &str = r#"
    fn process_a(v: i32) -> i32 { v + 1 }
    fn process_b(v: i32) -> i32 { v * 2 }
    fn process_c(v: i32) -> i32 { v - 3 }
    fn filter_even(v: &i32) -> bool { v % 2 == 0 }

    range(0, 100)
        .map(process_a)
        .map(process_b)
        .map(process_c)
        .filter(filter_even)
        .permutations(2)
        .combinations(2)
        .return
"#;

const COLOR_PALETTE_PICKER: &str = r#"
    fn compare_contrast(a: &Vec<Vec<u8>>, b: &Vec<Vec<u8>>) -> Ordering { Ordering::Equal }

    range(0, 255)
        .permutations_with_replacement(3)
        .combinations(5)
        .keep_first_n(20, compare_contrast)
        .return
"#;

const PERMUTATIONS_WITH_REPLACEMENT: &str = r#"
    range(0, 10)
        .permutations_with_replacement(3)
        .return
"#;

const CHAINED_OPERATIONS: &str = r#"
    range(0, 10)
        .permutations(2)
        .combinations(2)
        .return
"#;

#[library_benchmark]
#[bench::multiple_streams(MULTIPLE_STREAMS)]
#[bench::complex_csv(COMPLEX_CSV)]
#[bench::deeply_nested(DEEPLY_NESTED)]
#[bench::color_palette_picker(COLOR_PALETTE_PICKER)]
#[bench::permutations_with_replacement(PERMUTATIONS_WITH_REPLACEMENT)]
#[bench::chained_operations(CHAINED_OPERATIONS)]
fn bench_parse(input: &str) -> Result<String, MarigoldParseError> {
    black_box(PestParser::new().parse(black_box(input)))
}

library_benchmark_group!(
    name = parser_group;
    benchmarks = bench_parse
);

main!(library_benchmark_groups = parser_group);
