use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use marigold_grammar::complexity::{NodeComplexity, ProgramComplexity};
use serde::Serialize;

mod analyze_corpus;

use analyze_corpus::{corpus, crate_root};

const BLESS_VARIABLE: &str = "MARIGOLD_BLESS_ANALYZE_GOLDENS";

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

struct GoldenCheck {
    mismatches: Vec<String>,
    stale: Vec<String>,
}

fn check_goldens(corpus: &[(String, String)], dir: &Path, bless: bool) -> GoldenCheck {
    if bless {
        fs::create_dir_all(dir).expect("create golden directory");
    }

    let mut expected_files = BTreeSet::new();
    let mut mismatches = Vec::new();
    for (name, source) in corpus {
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

    let present: BTreeSet<String> = fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", dir.display()))
        .map(|entry| {
            entry
                .expect("directory entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    let stale: Vec<String> = present.difference(&expected_files).cloned().collect();

    if bless {
        for name in &stale {
            fs::remove_file(dir.join(name)).expect("remove stale golden");
        }
    }
    GoldenCheck { mismatches, stale }
}

#[test]
fn default_analyze_output_matches_the_committed_goldens() {
    let bless = std::env::var_os(BLESS_VARIABLE).is_some();
    let GoldenCheck { mismatches, stale } = check_goldens(&corpus(), &golden_dir(), bless);
    if bless {
        return;
    }

    assert!(
        mismatches.is_empty() && stale.is_empty(),
        "default analyze output changed.\n{}\nstale goldens: {stale:?}\nThese goldens pin the exact JSON that marigold-lsp, the MCP adapter, the CLI and the README consume, field order included. First review `git diff marigold-grammar/tests/golden_analyze` and the code change that caused it; only if the change is deliberate, re-bless with `{BLESS_VARIABLE}=1 cargo test -p marigold-grammar --test analyze_default_golden` (this also deletes stale goldens), review the diff of every changed file and explain each one in the pull request. Do not bless to make this test pass.",
        mismatches.join("\n")
    );
}

#[test]
fn bless_mode_rewrites_goldens_and_deletes_stale_ones_while_check_mode_only_reports() {
    let dir = std::env::temp_dir().join(format!(
        "marigold-analyze-golden-bless-{}",
        std::process::id()
    ));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    let corpus = vec![("small".to_string(), "range(0, 3).return".to_string())];

    let missing = check_goldens(&corpus, &dir, false);
    assert_eq!(missing.mismatches.len(), 1);
    assert!(missing.mismatches[0].starts_with("small.json: missing golden"));

    fs::write(dir.join("small.json"), "outdated\n").unwrap();
    fs::write(dir.join("planted.json"), "{}\n").unwrap();
    let reported = check_goldens(&corpus, &dir, false);
    assert_eq!(
        reported.mismatches,
        vec!["small.json: output differs from the golden".to_string()]
    );
    assert_eq!(reported.stale, vec!["planted.json".to_string()]);
    assert!(
        dir.join("planted.json").exists(),
        "check mode deletes nothing"
    );
    assert_eq!(
        fs::read_to_string(dir.join("small.json")).unwrap(),
        "outdated\n",
        "check mode writes nothing"
    );

    let blessed = check_goldens(&corpus, &dir, true);
    assert_eq!(blessed.stale, vec!["planted.json".to_string()]);
    assert!(
        !dir.join("planted.json").exists(),
        "bless deletes stale goldens"
    );
    assert_eq!(
        fs::read_to_string(dir.join("small.json")).unwrap(),
        record("range(0, 3).return")
    );

    let clean = check_goldens(&corpus, &dir, false);
    assert!(clean.mismatches.is_empty() && clean.stale.is_empty());
    fs::remove_dir_all(&dir).unwrap();
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
