//! The program corpus whose default `analyze` output is pinned by `tests/golden_analyze`.
//!
//! Not a test target itself (cargo ignores `tests/<dir>/mod.rs`); include with
//! `mod analyze_corpus;`.
#![allow(dead_code)]

use std::fs;
use std::path::{Path, PathBuf};

pub const INLINE_PROGRAMS: &[(&str, &str)] = &[
    ("readme_hello_world", "range(0, 5).write_file(\"/dev/stdout\", csv)"),
    ("range_identifier_bound", "range(0, N).return"),
    (
        "read_file_unbound",
        "struct D { x: i32 }\nfn id(d: D) -> D { d }\nread_file(\"data.csv\", csv, struct=D).ok().map(id).return",
    ),
    (
        "read_file_permutations",
        "struct D { x: i32 }\nread_file(\"data.csv\", csv, struct=D).ok_or_panic().permutations(3).return",
    ),
    (
        "read_file_combinations_then_keep_first_n",
        "struct D { x: i32 }\nfn cmp(a: &[D; 2], b: &[D; 2]) -> Ordering { Ordering::Equal }\nread_file(\"data.csv\", csv, struct=D).ok_or_panic().combinations(2).keep_first_n(5, cmp).return",
    ),
    ("undeclared_variable", "ghost.map(f).return"),
    (
        "variable_from_read_file_two_consumers",
        "struct D { x: i32 }\nrows = read_file(\"data.csv\", csv, struct=D).ok_or_panic()\nrows.map(f).return\nrows.permutations(2).return",
    ),
    (
        "select_all_with_read_file",
        "struct D { x: i32 }\nselect_all(range(0, 5), read_file(\"d.csv\", csv, struct=D).ok_or_panic()).return",
    ),
    (
        "enum_range",
        "enum Color { Red, Green, Blue }\nrange(Color).return",
    ),
    ("empty_program", ""),
];

pub fn crate_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn marigold_files_in(dir: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", dir.display()))
        .map(|entry| entry.expect("directory entry").path())
        .filter(|path| {
            matches!(
                path.extension().and_then(|e| e.to_str()),
                Some("marigold") | Some("mg")
            )
        })
        .collect();
    files.sort();
    files
}

/// Every pinned program as `(golden name, source)`, in a fixed order.
pub fn corpus() -> Vec<(String, String)> {
    let mut programs = Vec::new();
    let sources = [
        ("grammar_tests", crate_root().join("tests/programs")),
        (
            "algorithms",
            crate_root().join("../examples/algorithms/programs"),
        ),
        ("examples", crate_root().join("../examples")),
        ("repository_root", crate_root().join("..")),
        ("emit", crate_root().join("../marigold/tests/golden/emit")),
    ];
    for (group, dir) in sources {
        for path in marigold_files_in(&dir) {
            let stem = path
                .file_stem()
                .and_then(|s| s.to_str())
                .expect("utf-8 file stem");
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("could not read {}: {e}", path.display()));
            let suffix = if path.extension().and_then(|e| e.to_str()) == Some("mg") {
                "_mg"
            } else {
                ""
            };
            programs.push((format!("{group}__{stem}{suffix}"), source));
        }
    }
    let mut remap: Vec<PathBuf> = fs::read_dir(crate_root().join("tests/fixtures/remap"))
        .expect("read remap fixtures")
        .map(|entry| {
            entry
                .expect("directory entry")
                .path()
                .join("program.marigold")
        })
        .filter(|path| path.is_file())
        .collect();
    remap.sort();
    for path in remap {
        let stem = path
            .parent()
            .and_then(|dir| dir.file_name())
            .and_then(|s| s.to_str())
            .expect("utf-8 fixture name");
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read {}: {e}", path.display()));
        programs.push((format!("remap__{stem}"), source));
    }
    for (name, source) in INLINE_PROGRAMS {
        programs.push((format!("inline__{name}"), (*source).to_string()));
    }
    programs
}
