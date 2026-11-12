use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use marigold_grammar::complexity::{NodeComplexity, ProgramComplexity};
use serde::Serialize;

const BLESS_VARIABLE: &str = "MARIGOLD_BLESS_ANALYZE_GOLDENS";

const INLINE_PROGRAMS: &[(&str, &str)] = &[
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

fn crate_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn golden_dir() -> PathBuf {
    crate_root().join("tests/golden_analyze")
}

#[derive(Serialize)]
struct Analyzed<'a> {
    analyze: &'a ProgramComplexity,
    nodes: &'a Vec<NodeComplexity>,
}

#[derive(Serialize)]
struct Rejected {
    analyze_error: String,
    nodes_error: String,
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

fn corpus() -> Vec<(String, String)> {
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

fn record(source: &str) -> String {
    let program = marigold_grammar::marigold_analyze(source);
    let nodes = marigold_grammar::marigold_stream_complexities(source);
    let mut text = match (program, nodes) {
        (Ok(program), Ok(nodes)) => serde_json::to_string_pretty(&Analyzed {
            analyze: &program,
            nodes: &nodes,
        }),
        (Err(analyze), Err(nodes)) => serde_json::to_string_pretty(&Rejected {
            analyze_error: analyze.to_string(),
            nodes_error: nodes.to_string(),
        }),
        (program, nodes) => panic!(
            "marigold_analyze and marigold_stream_complexities disagree on whether the program parses: {:?} / {:?}",
            program.map(|_| ()).map_err(|e| e.to_string()),
            nodes.map(|_| ()).map_err(|e| e.to_string()),
        ),
    }
    .expect("serialize golden");
    text.push('\n');
    text
}

#[test]
fn default_analyze_output_matches_the_committed_goldens() {
    let corpus = corpus();
    let dir = golden_dir();
    let bless = std::env::var_os(BLESS_VARIABLE).is_some();
    if bless {
        fs::create_dir_all(&dir).expect("create golden directory");
    }

    let mut expected_files = BTreeSet::new();
    let mut mismatches = Vec::new();
    for (name, source) in &corpus {
        let file_name = format!("{name}.json");
        let rendered = record(source);
        let path = dir.join(&file_name);
        if bless {
            fs::write(&path, &rendered).expect("write golden");
        } else {
            match fs::read_to_string(&path) {
                Ok(committed) if committed == rendered => {}
                Ok(_) => mismatches.push(format!("{file_name}: output differs from the golden")),
                Err(e) => mismatches.push(format!("{file_name}: missing golden ({e})")),
            }
        }
        expected_files.insert(file_name);
    }

    let present: BTreeSet<String> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", dir.display()))
        .map(|entry| {
            entry
                .expect("directory entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    let stale: Vec<&String> = present.difference(&expected_files).collect();

    if bless {
        for name in &stale {
            fs::remove_file(dir.join(name)).expect("remove stale golden");
        }
        return;
    }

    assert!(
        mismatches.is_empty() && stale.is_empty(),
        "default analyze output changed.\n{}\nstale goldens: {stale:?}\nThese goldens pin the exact JSON that marigold-lsp, the MCP adapter, the CLI and the README consume, field order included. First review `git diff marigold-grammar/tests/golden_analyze` and the code change that caused it; only if the change is deliberate, re-bless with `{BLESS_VARIABLE}=1 cargo test -p marigold-grammar --test analyze_default_golden` (this also deletes stale goldens), review the diff of every changed file and explain each one in the pull request. Do not bless to make this test pass.",
        mismatches.join("\n")
    );
}

#[test]
fn corpus_covers_every_pinned_program_directory() {
    let corpus = corpus();
    let names: BTreeSet<&str> = corpus.iter().map(|(name, _)| name.as_str()).collect();
    for expected in [
        "grammar_tests__card_read_file",
        "grammar_tests__color_palette",
        "algorithms__gcd",
        "algorithms__tsp_brute_force",
        "examples__integers",
        "inline__range_identifier_bound",
        "inline__readme_hello_world",
        "grammar_tests__chained_maps_mg",
        "repository_root__hello_world",
        "emit__g01_unnamed_returning",
        "remap__f01_undefined_fn",
    ] {
        assert!(names.contains(expected), "corpus is missing {expected}");
    }
    assert!(corpus.len() >= 107, "corpus shrank to {}", corpus.len());
}

#[test]
fn identifier_range_bound_is_a_parse_error_with_the_current_message() {
    let error = marigold_grammar::marigold_analyze("range(0, N).return")
        .map(|_| ())
        .expect_err("an identifier range bound does not parse");
    assert!(
        error.to_string().contains("Could not parse range end"),
        "unexpected error text: {error}"
    );
}
