//! Pins the Rust text `marigold_parse` emits, byte for byte.
//!
//! `marigold_generate` and `marigold_parse` share one code path, so comparing them with each other
//! (as `tests/source_map.rs` does) cannot notice a change to the emitted code. These snapshots were
//! captured from `marigold_parse` on `main` before the source map existed (commit `2343ee8`); a
//! change to code generation has to update them on purpose.
//!
//! The emitted code depends on features: `io` adds I/O code and `tokio` / `async-std` (which emit
//! the same code) switch stream functions to their async forms. There is one snapshot set per
//! combination the CI matrix runs: none, `tokio` or `async-std`, and `io` together with either.
//! `io` alone is not in the matrix and has no snapshots.

use marigold_grammar::{marigold_generate, marigold_parse};

fn check(name: &str, src: &str, expected: &str) {
    let parsed = marigold_parse(src).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(parsed, expected, "{name}: marigold_parse output changed");
    let generated = marigold_generate(src).unwrap();
    assert_eq!(
        generated.rust, expected,
        "{name}: marigold_generate output changed"
    );
}

macro_rules! snapshots {
    ($dir:literal: $($test:ident: $name:literal, $src:expr;)*) => {
        $(
            #[test]
            fn $test() {
                check(
                    $name,
                    $src,
                    include_str!(concat!("golden_rust/", $dir, "/", $name, ".rs")),
                );
            }
        )*
    };
}

macro_rules! programs {
    ($dir:literal) => {
        snapshots! { $dir:
            snapshot_g01_unnamed_returning: "g01_unnamed_returning", "range(0, 10).return";
            snapshot_g02_unnamed_nonreturning: "g02_unnamed_nonreturning", "range(0, 10).write_file(\"out.csv\", csv)";
            snapshot_g03_var_named_returning: "g03_var_named_returning", "x = range(0, 5)\nx.return";
            snapshot_g04_var_named_nonreturning: "g04_var_named_nonreturning", "x = range(0, 5).map(f)\nx.write_file(\"o.csv\", csv)";
            snapshot_g05_var_from_prior: "g05_var_from_prior", "x = range(0, 5)\ny = x.filter(g)\ny.return";
            snapshot_g06_struct_plain: "g06_struct_plain", "struct S { a: i32, b: string_10, c: Option<u8> }\nrange(0, 1).return";
            snapshot_g07_struct_bounded: "g07_struct_bounded", "struct P { v: int[0, 255], w: uint[1, 10] }\nrange(0, 1).return";
            snapshot_g08_enum_and_range: "g08_enum_and_range", "enum E { A, B = \"b\", default Z }\nrange(E).return";
            snapshot_g09_fn: "g09_fn", "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return";
            snapshot_g10_out_of_order: "g10_out_of_order", "range(0, 2).return\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nstruct S { a: u8 }\nenum E { A }\nx.write_file(\"o.csv\", csv)\nrange(E).return";
            snapshot_g11_struct_enum_struct_interleave: "g11_struct_enum_struct_interleave", "struct B { a: u8 }\nenum A { X }\nstruct C { a: u8 }";
            snapshot_g12_read_file: "g12_read_file", "struct R { a: i32 }\nread_file(\"data.csv\", csv, struct=R).ok().return";
            snapshot_g13_select_all: "g13_select_all", "select_all(range(0, 3), range(5, 8).map(f)).filter_map(g).return";
            snapshot_g14_multiline: "g14_multiline", "x = range(0, 5)\n  .map(f)\n  .filter(g)\nx\n  .return";
            snapshot_g15_two_of_each: "g15_two_of_each", "a = range(0, 2)\nb = range(2, 4)\na.return\nrange(9, 10).write_file(\"p.csv\", csv)\nb.return\nrange(7, 8).return\na.write_file(\"q.csv\", csv)\nrange(1, 2).write_file(\"r.csv\", csv)";
            snapshot_g16_unicode: "g16_unicode", "struct R { a: i32 }\nenum Mood { Happy = \"😀\", Sad = \"ünï\" }\nfn pick(x: &str) -> bool { /* é */ x == \"日本語}\" }\nread_file(\"dät€.csv\", csv, struct=R).ok().return\nrange(0, 1).write_file(\"日本語/ö.csv\", csv)";
            snapshot_g18_trailing_ws_between_vars: "g18_trailing_ws_between_vars", "  x = range(0, 5)   \n\n   \n\n y = x.map(f)   \t\ny.return   ";
            snapshot_g19_empty: "g19_empty", "";
            snapshot_g20_whitespace_only: "g20_whitespace_only", "   \n\t\n ";
            snapshot_g21_all_stream_fns: "g21_all_stream_fns", "range(0, =10).permutations(2).combinations(2).permutations_with_replacement(2).keep_first_n(3, cmp).fold(0, add).map(a).filter(b).filter_map(c).return";
            snapshot_g22_bounded_enum_ref: "g22_bounded_enum_ref", "enum Color { Red, Green }\nstruct P { c: int[0, Color.len()] }\nx = range(Color)\nx.return";
            snapshot_g23_only_declarations: "g23_only_declarations", "enum E { A }\nstruct S { a: u8 }\nfn f(x: i32) -> i32 { x }";
            snapshot_g24_stream_var_only: "g24_stream_var_only", "x = range(0, 3)";
            snapshot_g26_bounded_types_example: "g26_bounded_types_example", "        struct Sensor {\n            reading: int[-40, 85]\n        }\n\n        fn to_sensor(i: i32) -> Sensor {\n            Sensor { reading: i as i8 }\n        }\n\n        fn get_reading(s: Sensor) -> i8 {\n            s.reading\n        }\n\n        range(0, 3)\n            .map(to_sensor)\n            .map(get_reading)\n            .return\n";
        }
    };
}

#[cfg(not(any(feature = "io", feature = "tokio", feature = "async-std")))]
mod default_features {
    use super::*;
    programs!("default");
}

#[cfg(all(not(feature = "io"), any(feature = "tokio", feature = "async-std")))]
mod async_runtime {
    use super::*;
    programs!("async_runtime");
}

#[cfg(all(feature = "io", any(feature = "tokio", feature = "async-std")))]
mod io_and_async_runtime {
    use super::*;
    programs!("io_and_async_runtime");
}
