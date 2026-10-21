//! End-to-end tests for `marigold emit` (issue #400, task T3).
//!
//! Needs `--features cli` (same gate as `check_cli.rs`; CI's `cargo test --all-features` has it).
//! Style follows `check_cli.rs`: `env!("CARGO_BIN_EXE_marigold")`, temp files, `Output` asserts.
//!
//! Oracles used here:
//! * `marigold_grammar::marigold_parse`        -- `--format rust` must equal it byte for byte;
//! * `marigold_grammar::marigold_generate`     -- JSON offsets must equal the library's `SourceMap`;
//! * `marigold check`                          -- exit codes and error rendering must match it;
//! * `emit_schema` (shared with the grammar crate) -- strict schema + semantic validator;
//! * golden files in `tests/golden/emit/`      -- full JSON documents (`UPDATE_GOLDEN=1` rewrites).
#![cfg(feature = "cli")]

#[path = "../../marigold-grammar/tests/common/emit_schema.rs"]
mod emit_schema;

use emit_schema::{check_semantics, segment_text, validate_v1};
use marigold_grammar::{
    marigold_generate, marigold_parse, marigold_stream_complexities, marigold_symbols,
};
use serde_json::{json, Value};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

// ---------------------------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------------------------

fn marigold() -> Command {
    Command::new(env!("CARGO_BIN_EXE_marigold"))
}

fn temp_dir() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("marigold-emit-cli-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// A file in the per-process temp directory that is deleted when it goes out of scope (the
/// tests write up to 10 MiB each).
struct TempFile(PathBuf);

impl std::ops::Deref for TempFile {
    type Target = Path;
    fn deref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<Path> for TempFile {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

fn write_temp(name: &str, contents: impl AsRef<[u8]>) -> TempFile {
    let path = temp_dir().join(name);
    std::fs::write(&path, contents).unwrap();
    TempFile(path)
}

fn emit_file(args: &[&str], path: &Path) -> Output {
    marigold()
        .arg("emit")
        .args(args)
        .arg(path)
        .output()
        .unwrap()
}

fn emit_stdin(args: &[&str], input: &[u8]) -> Output {
    let mut child = marigold()
        .arg("emit")
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    let input = input.to_vec();
    let writer = std::thread::spawn(move || {
        let _ = stdin.write_all(&input);
    });
    let out = child.wait_with_output().unwrap();
    writer.join().unwrap();
    out
}

fn check_file(args: &[&str], path: &Path) -> Output {
    marigold()
        .arg("check")
        .args(args)
        .arg(path)
        .output()
        .unwrap()
}

fn stdout_json(out: &Output) -> Value {
    serde_json::from_slice(&out.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout is not JSON ({e}): {}",
            String::from_utf8_lossy(&out.stdout)
        )
    })
}

fn s(bytes: &[u8]) -> &str {
    std::str::from_utf8(bytes).unwrap()
}

fn golden_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden/emit")
}

/// `(file name, source text)` of every golden input, sorted.
fn golden_programs() -> Vec<(String, PathBuf, String)> {
    let mut v: Vec<_> = std::fs::read_dir(golden_dir())
        .expect("tests/golden/emit must exist")
        .flatten()
        .filter(|e| e.path().extension().is_some_and(|x| x == "marigold"))
        .map(|e| {
            let p = e.path();
            (
                e.file_name().to_string_lossy().into_owned(),
                p.clone(),
                std::fs::read_to_string(p).unwrap(),
            )
        })
        .collect();
    v.sort();
    v
}

/// The repo's other Marigold programs (same search path as the grammar crate's `corpus()`).
fn corpus() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = Vec::new();
    for dir in [
        root.join("marigold-grammar/tests/programs"),
        root.join("examples"),
        root.clone(),
    ] {
        for entry in std::fs::read_dir(dir).into_iter().flatten().flatten() {
            let path = entry.path();
            let ext = path.extension().and_then(|e| e.to_str());
            if path.is_file() && matches!(ext, Some("marigold") | Some("mg")) {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

/// Full battery on one parsed JSON document.
fn assert_document_ok(doc: &Value, src: &str) {
    validate_v1(doc).unwrap_or_else(|e| panic!("{e}"));
    check_semantics(doc).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(doc["source"], json!(src), "`source` is the input verbatim");
    assert_eq!(doc["rust"], json!(marigold_parse(src).unwrap()));
    assert_eq!(doc["features"], json!(expected_features()));
    let program = marigold_generate(src).unwrap();
    let segs = program.map.segments();
    let out = doc["segments"].as_array().unwrap();
    assert_eq!(out.len(), segs.len());
    for (i, (lib, j)) in segs.iter().zip(out).enumerate() {
        assert_eq!(j["source"]["start"], json!(lib.source.start));
        assert_eq!(j["source"]["end"], json!(lib.source.end));
        assert_eq!(j["generated"]["start"], json!(lib.generated.start));
        assert_eq!(j["generated"]["end"], json!(lib.generated.end));
        assert_eq!(j["expr_index"], json!(lib.expr_index));
        assert_eq!(j["name"], json!(lib.name));
        // Round trip using nothing but the JSON: re-slice both texts.
        let (stext, gtext) = (
            segment_text(doc, i, "source"),
            segment_text(doc, i, "generated"),
        );
        assert_eq!(stext, &src[lib.source.start..lib.source.end]);
        match j["kind"].as_str().unwrap() {
            "returning_stream" => {
                assert!(stext.ends_with("return"), "{stext:?}");
                assert!(gtext.starts_with("let returning_stream_"), "{gtext:?}");
            }
            "non_returning_stream" => {
                assert!(stext.contains(".write_file("), "{stext:?}");
                assert!(gtext.starts_with("let non_returning_stream_"), "{gtext:?}");
            }
            "stream_variable" => assert!(gtext.starts_with("let mut "), "{gtext:?}"),
            "stream_variable_runner" => assert!(
                gtext.starts_with("let stream_variable_runners_"),
                "{gtext:?}"
            ),
            "function" => assert!(gtext.starts_with("const fn "), "{gtext:?}"),
            "struct" | "enum" => assert!(
                gtext.contains("struct ") || gtext.contains("enum "),
                "{gtext:?}"
            ),
            _ => {} // T2 adds kinds: the schema validator and the goldens still cover them
        }
    }
    // Other public APIs that know (parts of) the same ranges.
    for n in marigold_stream_complexities(src).unwrap() {
        let hit = out
            .iter()
            .find(|j| {
                j["expr_index"] == json!(n.expr_index) && j["kind"] != "stream_variable_runner"
            })
            .unwrap();
        assert_eq!(hit["source"]["start"], json!(n.range.start));
        assert_eq!(hit["source"]["end"], json!(n.range.end));
        assert_eq!(hit["name"], json!(n.name));
    }
    for d in marigold_symbols(src).declarations {
        let hit = out
            .iter()
            .find(|j| j["expr_index"] == json!(d.expr_index))
            .unwrap();
        assert_eq!(hit["name"], json!(d.name));
        assert!(hit["source"]["start"].as_u64().unwrap() as usize <= d.range.start);
        assert!(hit["source"]["end"].as_u64().unwrap() as usize >= d.range.end);
    }
}

// ---------------------------------------------------------------------------------------------
// --format rust
// ---------------------------------------------------------------------------------------------

#[test]
fn rust_format_is_the_default_and_equals_marigold_parse_byte_for_byte() {
    for (name, path, src) in golden_programs() {
        let expected = marigold_parse(&src).unwrap();
        for args in [&[][..], &["--format", "rust"][..], &["--format=rust"][..]] {
            let out = emit_file(args, &path);
            assert_eq!(out.status.code(), Some(0), "{name}: {out:?}");
            assert_eq!(
                s(&out.stdout),
                expected,
                "{name} {args:?}: no extra bytes, not even a newline"
            );
            assert!(out.stderr.is_empty(), "{name}: {}", s(&out.stderr));
        }
    }
}

#[test]
fn rust_format_matches_marigold_parse_for_the_whole_repo_corpus() {
    let files = corpus();
    assert!(files.len() >= 30, "corpus too small: {}", files.len());
    let mut compared = 0;
    for path in files {
        let src = std::fs::read_to_string(&path).unwrap();
        let out = emit_file(&[], &path);
        match marigold_parse(&src) {
            Ok(expected) => {
                assert_eq!(out.status.code(), Some(0), "{}", path.display());
                assert_eq!(s(&out.stdout), expected, "{}", path.display());
                compared += 1;
            }
            Err(_) => assert_eq!(out.status.code(), Some(1), "{}", path.display()),
        }
    }
    assert!(compared >= 25, "only {compared} corpus programs compiled");
}

#[test]
fn rust_output_is_the_inner_text_of_what_m_expands_to() {
    // `m!(..)` expands to "{\n" + marigold_parse(tokens) + "\n}\n"; `emit` prints the middle part
    // for the file's own text. Pin the wrapper-free contract.
    let path = write_temp("inner.marigold", "range(0, 3).return");
    let out = emit_file(&[], &path);
    let text = s(&out.stdout);
    assert!(
        text.starts_with("async {\n    use ::marigold::marigold_impl::*;\n    "),
        "{text}"
    );
    assert!(text.ends_with("all_streams\n}\n"), "{text:?}");
    assert!(!text.starts_with('{'));
}

// ---------------------------------------------------------------------------------------------
// --format json
// ---------------------------------------------------------------------------------------------

#[test]
fn json_documents_validate_for_every_golden_program() {
    for (name, path, src) in golden_programs() {
        let out = emit_file(&["--format", "json"], &path);
        assert_eq!(out.status.code(), Some(0), "{name}: {out:?}");
        assert!(out.stderr.is_empty(), "{name}: {}", s(&out.stderr));
        assert_document_ok(&stdout_json(&out), &src);
    }
}

#[test]
fn json_documents_validate_for_the_whole_repo_corpus() {
    let mut n = 0;
    for path in corpus() {
        let src = std::fs::read_to_string(&path).unwrap();
        if marigold_parse(&src).is_err() {
            continue;
        }
        let out = emit_file(&["--format", "json"], &path);
        assert_eq!(out.status.code(), Some(0), "{}", path.display());
        assert_document_ok(&stdout_json(&out), &src);
        n += 1;
    }
    assert!(n >= 25, "only {n} corpus programs checked");
}

#[test]
fn json_rust_field_equals_the_rust_format_output() {
    let (_, path, _) = golden_programs()
        .into_iter()
        .find(|(n, _, _)| n.starts_with("g10"))
        .unwrap();
    let rust = emit_file(&["--format", "rust"], &path);
    let doc = stdout_json(&emit_file(&["--format", "json"], &path));
    assert_eq!(doc["rust"].as_str().unwrap(), s(&rust.stdout));
}

#[test]
fn json_is_pretty_printed_with_exactly_one_trailing_newline_and_fixed_key_order() {
    let path = write_temp("pretty.marigold", "x = range(0, 5)\nx.return");
    let out = emit_file(&["--format", "json"], &path);
    let text = s(&out.stdout);
    assert!(
        text.ends_with("}\n") && !text.ends_with("}\n\n"),
        "{:?}",
        &text[text.len().saturating_sub(10)..]
    );
    let features: String = expected_features()
        .iter()
        .map(|f| format!("\n    \"{f}\""))
        .collect::<Vec<_>>()
        .join(",");
    assert!(
        text.starts_with(&format!("{{\n  \"version\": 1,\n  \"features\": [{features}\n  ],\n  \"source\": \"x = range(0, 5)\\nx.return\",\n  \"rust\": \"async {{")),
        "{}",
        &text[..120]
    );
    assert!(text.contains("\n  \"segments\": [\n    {\n      \"kind\": \"stream_variable\",\n      \"name\": \"x\",\n      \"ident\": \"x\",\n      \"expr_index\": 0,\n      \"parent\": null,\n      \"source\": {\n        \"start\": 0,\n        \"end\": 15,\n        \"start_line\": 1,\n        \"start_column\": 1,\n        \"end_line\": 1,\n        \"end_column\": 16\n      },"), "{text}");
    assert_eq!(
        text.lines().filter(|l| l.starts_with("  \"")).count(),
        5,
        "exactly five top-level keys"
    );
}

#[test]
fn json_is_deterministic_and_independent_of_cwd_and_environment() {
    let path = write_temp(
        "det.marigold",
        "fn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nx.return",
    );
    let a = emit_file(&["--format", "json"], &path);
    let b = marigold()
        .current_dir("/")
        .env_clear()
        .args(["emit", "--format", "json"])
        .arg(&*path)
        .output()
        .unwrap();
    assert_eq!(a.status.code(), Some(0));
    assert_eq!(a.stdout, b.stdout);
    let c = emit_stdin(
        &["--format", "json"],
        b"fn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nx.return",
    );
    assert_eq!(
        a.stdout, c.stdout,
        "no path or other machine-specific data in the document"
    );
}

#[test]
fn json_columns_count_scalar_values_and_offsets_count_bytes() {
    let src = "range(0, 1).write_file(\"é😀.csv\", csv)\r\nrange(2, 3).return\r\n";
    let path = write_temp("unicode.marigold", src);
    let doc = stdout_json(&emit_file(&["--format", "json"], &path));
    assert_document_ok(&doc, src);
    let first = "range(0, 1).write_file(\"é😀.csv\", csv)";
    let seg = |n: u64| {
        doc["segments"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s["expr_index"] == json!(n))
            .unwrap()
            .clone()
    };
    assert_eq!(seg(0)["source"]["end"], json!(first.len()));
    assert_eq!(
        seg(0)["source"]["end_column"],
        json!(first.chars().count() + 1)
    );
    assert_ne!(first.len(), first.chars().count());
    assert_eq!(seg(1)["source"]["start"], json!(first.len() + 2));
    assert_eq!(
        (
            seg(1)["source"]["start_line"].as_u64(),
            seg(1)["source"]["start_column"].as_u64()
        ),
        (Some(2), Some(1))
    );
}

// ---------------------------------------------------------------------------------------------
// golden documents
// ---------------------------------------------------------------------------------------------

const GOLDEN_JSON: [&str; 8] = [
    "g01_unnamed_returning",
    "g03_var_named_returning",
    "g05_var_from_prior",
    "g09_fn",
    "g10_out_of_order",
    "g16_unicode",
    "g17_crlf",
    "g19_empty",
];

#[test]
fn golden_inputs_have_the_bytes_the_tests_rely_on() {
    let read =
        |n: &str| std::fs::read_to_string(golden_dir().join(format!("{n}.marigold"))).unwrap();
    assert!(
        read("g17_crlf").contains("\r\n") && !read("g17_crlf").replace("\r\n", "").contains('\r'),
        "g17 must keep its CRLF line endings"
    );
    assert!(read("g16_unicode").contains("日本語") && read("g16_unicode").contains("😀"));
    assert_eq!(read("g19_empty"), "");
    assert!(read("g18_trailing_ws_between_vars").contains("   \t\n"));
}

/// The generated Rust, and with it every golden document, depends on the features of the grammar
/// crate the binary links. `cli` enables `marigold-grammar/io` (the view `m!` has under
/// `marigold run`); `tokio` / `async-std` of this package switch stream functions to their async
/// forms. So there are two sets of golden documents, named like the snapshot sets of
/// `marigold-grammar/tests/generated_rust_golden.rs`, which also serve as an independent oracle
/// for the `rust` field of every golden document and for `--format rust`.
fn golden_variant() -> &'static str {
    if cfg!(any(feature = "tokio", feature = "async-std")) {
        "io_and_async_runtime"
    } else {
        "io"
    }
}

/// The grammar features the binary was built with, sorted: what `"features"` must list.
fn expected_features() -> Vec<&'static str> {
    let mut features = vec!["io"];
    if cfg!(feature = "async-std") {
        features.push("async-std");
    }
    if cfg!(feature = "tokio") {
        features.push("tokio");
    }
    features.sort();
    features
}

/// The features the golden documents of `variant` were written with. The runtime part is not
/// part of what a golden pins (`tokio` and `async-std` generate the same code), so the test
/// swaps it for the features of this build before comparing; `expected_features` pins those.
fn golden_features(variant: &str) -> Vec<&'static str> {
    match variant {
        "io" => vec!["io"],
        _ => vec!["io", "tokio"],
    }
}

/// `text` (a pretty-printed document) with its `"features"` replaced by `features`.
fn with_features(text: &str, features: &[&str]) -> String {
    let start = text.find("  \"features\": [").expect("a features key");
    let end = start + text[start..].find("],\n").expect("end of features") + 3;
    let list = if features.is_empty() {
        "  \"features\": [],\n".to_string()
    } else {
        let items: Vec<_> = features.iter().map(|f| format!("    \"{f}\"")).collect();
        format!("  \"features\": [\n{}\n  ],\n", items.join(",\n"))
    };
    format!("{}{list}{}", &text[..start], &text[end..])
}

fn snapshot_path(variant: &str, name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../marigold-grammar/tests/golden_rust")
        .join(variant)
        .join(format!("{name}.rs"))
}

#[test]
fn golden_json_documents_are_stable() {
    // Only the exact value `1` blesses, so a stray `UPDATE_GOLDEN=0` or `=` cannot rewrite them.
    let update = std::env::var("UPDATE_GOLDEN").as_deref() == Ok("1");
    let variant = golden_variant();
    for name in GOLDEN_JSON {
        let input = golden_dir().join(format!("{name}.marigold"));
        let golden = golden_dir().join(variant).join(format!("{name}.json"));
        let out = emit_file(&["--format", "json"], &input);
        assert_eq!(out.status.code(), Some(0), "{name}");
        if update {
            std::fs::create_dir_all(golden.parent().unwrap()).unwrap();
            std::fs::write(
                &golden,
                with_features(s(&out.stdout), &golden_features(variant)),
            )
            .unwrap();
            continue;
        }
        let written = std::fs::read_to_string(&golden)
            .unwrap_or_else(|_| panic!("missing {}; run with UPDATE_GOLDEN=1", golden.display()));
        let document: Value = serde_json::from_str(&written).unwrap();
        assert_eq!(
            document["features"],
            json!(golden_features(variant)),
            "{name}: golden written for other features"
        );
        let want = with_features(&written, &expected_features());
        assert!(
            s(&out.stdout) == want,
            "{name}: output differs from {}\nrerun with UPDATE_GOLDEN=1 if the change is intended and review the diff\n--- got ---\n{}",
            golden.display(),
            s(&out.stdout)
        );
        // a golden file must itself be a valid v1 document for its input ...
        let document: Value = serde_json::from_str(&want).unwrap();
        assert_document_ok(&document, &std::fs::read_to_string(&input).unwrap());
        // ... whose Rust is the snapshot captured from `main` before the source map existed
        let snapshot = snapshot_path(variant, name);
        assert_eq!(
            document["rust"],
            json!(std::fs::read_to_string(&snapshot).unwrap()),
            "{name}: golden `rust` differs from {}",
            snapshot.display()
        );
    }
    // Blessing is not a pass: the new files must be reviewed, then the tests rerun without it.
    assert!(
        !update,
        "golden documents rewritten for `{variant}`: review `git diff`, then rerun without UPDATE_GOLDEN"
    );
}

#[test]
fn rust_output_equals_the_snapshots_captured_from_main_for_this_feature_set() {
    // `marigold_parse` in this process and the binary share the grammar's features, so comparing
    // them cannot notice a binary built with the wrong ones (`cli` must enable `io`, the view of
    // the `m!` macro under `marigold run`). The snapshots were captured on main with those features.
    let variant = golden_variant();
    let mut compared = 0;
    for (name, path, _) in golden_programs() {
        let name = name.trim_end_matches(".marigold");
        let Ok(snapshot) = std::fs::read_to_string(snapshot_path(variant, name)) else {
            continue; // g25 is not one of the main snapshots
        };
        let out = emit_file(&[], &path);
        assert_eq!(out.status.code(), Some(0), "{name}");
        assert_eq!(s(&out.stdout), snapshot, "{name} ({variant})");
        compared += 1;
    }
    assert!(compared >= 25, "only {compared} programs compared");
}

#[test]
fn json_lists_the_grammar_features_the_rust_depends_on() {
    // Run under plain `--features cli` (`cargo test -p marigold --features cli --test emit_cli`):
    // the only configuration where `cli` itself must provide the `io` feature. CI's
    // `--all-features` enables `io` anyway.
    let path = write_temp("features.marigold", "range(0, 3).return");
    let doc = stdout_json(&emit_file(&["--format", "json"], &path));
    assert_eq!(doc["features"], json!(expected_features()));
    // `read_file` is where the `io` view differs from the featureless one: the generated code
    // derives `Deserialize` for the struct, as the `m!` macro does under `marigold run`
    let program = "struct R { a: i32 }\nread_file(\"data.csv\", csv, struct=R).ok().return";
    let io_view = emit_file(&[], &write_temp("io_view.marigold", program));
    assert_eq!(io_view.status.code(), Some(0));
    assert!(
        s(&io_view.stdout).contains("serde::Deserialize"),
        "emit must generate what `m!` generates with the io feature: {}",
        s(&io_view.stdout)
    );
}

// ---------------------------------------------------------------------------------------------
// stdin vs file
// ---------------------------------------------------------------------------------------------

#[test]
fn stdin_and_file_give_byte_identical_output_for_both_formats() {
    for (name, path, src) in golden_programs() {
        for args in [&["--format", "rust"][..], &["--format", "json"][..]] {
            let file = emit_file(args, &path);
            let stdin = emit_stdin(args, src.as_bytes());
            assert_eq!(file.status.code(), stdin.status.code(), "{name} {args:?}");
            assert_eq!(file.stdout, stdin.stdout, "{name} {args:?}");
            assert_eq!(file.stderr, stdin.stderr, "{name} {args:?}");
        }
    }
}

#[test]
fn stdin_errors_differ_from_file_errors_only_by_the_path_label() {
    let src = "x = range(0, 5)\nx.retur";
    let path = write_temp("label.marigold", src);
    let file = emit_file(&[], &path);
    let stdin = emit_stdin(&[], src.as_bytes());
    assert_eq!(file.status.code(), Some(1));
    assert_eq!(stdin.status.code(), Some(1));
    let normalised = s(&file.stderr).replace(&path.display().to_string(), "<stdin>");
    assert_eq!(normalised, s(&stdin.stderr));
    assert!(
        s(&stdin.stderr).starts_with("<stdin>:2:"),
        "{}",
        s(&stdin.stderr)
    );
}

#[test]
fn empty_stdin_is_a_valid_empty_program() {
    for args in [&[][..], &["--format", "json"][..]] {
        let out = emit_stdin(args, b"");
        assert_eq!(out.status.code(), Some(0), "{out:?}");
        if args.is_empty() {
            assert_eq!(s(&out.stdout), marigold_parse("").unwrap());
        } else {
            let doc = stdout_json(&out);
            validate_v1(&doc).unwrap();
            assert_eq!(doc["segments"], json!([]));
            assert_eq!(doc["source"], json!(""));
        }
    }
}

// ---------------------------------------------------------------------------------------------
// trimming: offsets are relative to the ORIGINAL file
// ---------------------------------------------------------------------------------------------

#[test]
fn emit_does_not_trim_so_offsets_index_the_original_file() {
    // `marigold run` calls `.trim()`; `emit` must not: offsets are valid for the file on disk.
    let program = "x = range(0, 5)\nx.return";
    let src = "\n\n  \t\r\n\u{20}".to_string() + program + " \r\n\r\n\t";
    let path = write_temp("padded.marigold", &src);
    let doc = stdout_json(&emit_file(&["--format", "json"], &path));
    assert_document_ok(&doc, &src);
    let lead = src.find("x = ").unwrap();
    assert!(lead > 0);
    let first = &doc["segments"][0]["source"];
    assert_eq!(first["start"], json!(lead));
    assert_eq!(
        first["start_line"],
        json!(4),
        "three newlines precede the program"
    );
    assert_eq!(
        first["start_column"],
        json!(2),
        "one space precedes it on that line"
    );
    assert_eq!(segment_text(&doc, 0, "source"), "x = range(0, 5)");
    // Same Rust as the trimmed program (what `run` would compile), different offsets.
    let trimmed = stdout_json(&emit_file(
        &["--format", "json"],
        &write_temp("trimmed.marigold", program),
    ));
    assert_eq!(doc["rust"], trimmed["rust"]);
    assert_eq!(
        emit_file(&[], &path).stdout,
        emit_file(&[], &write_temp("trimmed.marigold", program)).stdout
    );
    // and via stdin
    assert_eq!(
        emit_stdin(&["--format", "json"], src.as_bytes()).stdout,
        emit_file(&["--format", "json"], &path).stdout
    );
}

#[test]
fn characters_outside_the_grammar_around_a_program_are_a_syntax_error_for_emit_and_check() {
    // PINNED DIVERGENCE (see TESTSPEC-T3 section 5): `run` and `analyze` trim with `str::trim`,
    // which strips Unicode White_Space such as U+00A0 (no-break space) and U+000C (form feed),
    // but the grammar's WHITESPACE is only " \t\r\n". `emit` follows `check` (no trim), so
    // those two are accepted by `run` and rejected here. A byte order mark (U+FEFF) is not
    // White_Space: `run` rejects it too. If a later task makes `run` agree with `emit`,
    // update this test on purpose.
    for (name, src) in [
        ("nbsp", "\u{a0}range(0, 1).return"),
        ("bom", "\u{feff}range(0, 1).return"),
        ("trailing_ff", "range(0, 1).return\u{c}"),
    ] {
        let path = write_temp(&format!("{name}.marigold"), src);
        let emit = emit_file(&[], &path);
        let check = check_file(&[], &path);
        assert_eq!(emit.status.code(), Some(1), "{name}: {emit:?}");
        assert_eq!(check.status.code(), Some(1), "{name}");
        assert!(emit.stdout.is_empty());
        assert_eq!(emit.stderr, check.stdout, "{name}: same rendering as check");
    }
}

// ---------------------------------------------------------------------------------------------
// error paths and exit codes
// ---------------------------------------------------------------------------------------------

const BAD_PROGRAMS: [(&str, &str); 9] = [
    ("syntax_truncated", "range(0,"),
    ("syntax_typo", "range(0, 1).retur"),
    (
        "syntax_after_valid_chunk",
        "x = range(0,3)\nrange(0,1).retur",
    ),
    ("toplevel_comment", "// c\nrange(0,1).return"),
    ("unknown_enum", "range(Color).return"),
    (
        "unknown_enum_other_declared",
        "enum E { A }\nrange(Nope).return",
    ),
    (
        "bounds_inverted",
        "struct T { f: int[10, 5] }\nrange(0,1).return",
    ),
    ("fn_without_body", "fn f(x: i32) -> i32\nrange(0,1).return"),
    ("nul_byte", "range(0, 1).return\u{0}"),
];

#[test]
fn compile_errors_exit_one_with_empty_stdout_and_check_style_diagnostics_on_stderr() {
    for (name, src) in BAD_PROGRAMS {
        assert!(marigold_parse(src).is_err(), "{name} must be invalid");
        let path = write_temp(&format!("bad_{name}.marigold"), src);
        for format in ["rust", "json"] {
            let out = emit_file(&["--format", format], &path);
            assert_eq!(out.status.code(), Some(1), "{name} {format}: {out:?}");
            assert!(
                out.stdout.is_empty(),
                "{name} {format}: stdout must stay empty on failure"
            );
        }
        // human diagnostics: exactly the error lines `marigold check` prints for the file
        let human = emit_file(&["--format", "rust"], &path);
        let check = check_file(&[], &path);
        assert_eq!(check.status.code(), Some(1), "{name}");
        assert_eq!(s(&human.stderr), error_blocks(s(&check.stdout)), "{name}");
        assert!(
            s(&human.stderr).starts_with(&format!("{}:", path.display())),
            "{name}: {}",
            s(&human.stderr)
        );
        // json diagnostics: exactly `marigold check --format json` (same array, same key order)
        let json_err = emit_file(&["--format", "json"], &path);
        let check_json = check_file(&["--format", "json"], &path);
        let want: Vec<Value> = stdout_json(&check_json)
            .as_array()
            .unwrap()
            .iter()
            .filter(|d| d["severity"] == "error")
            .cloned()
            .collect();
        let got: Value = serde_json::from_slice(&json_err.stderr)
            .unwrap_or_else(|e| panic!("{name}: stderr is not JSON: {e}: {}", s(&json_err.stderr)));
        assert_eq!(got, json!(want), "{name}");
        if want.len() == stdout_json(&check_json).as_array().unwrap().len() {
            assert_eq!(
                s(&json_err.stderr),
                s(&check_json.stdout),
                "{name}: byte-identical to check, key order included"
            );
        }
    }
}

/// Keep the `error[..]` blocks of `marigold check` human output (a line plus its `  help:` lines).
fn error_blocks(check_stdout: &str) -> String {
    let mut out = String::new();
    let mut keep = false;
    for line in check_stdout.split_inclusive('\n') {
        if line.starts_with("  help:") {
            if keep {
                out.push_str(line);
            }
        } else {
            keep = line.contains(": error[");
            if keep {
                out.push_str(line);
            }
        }
    }
    out
}

#[test]
fn human_column_after_multibyte_text_counts_scalar_values() {
    // "é" is 2 bytes and "😀" 4 bytes / 2 UTF-16 units: the error is on the same line, after them,
    // so rendering the column in bytes or UTF-16 units gives a different number.
    let first = "range(0, 1).write_file(\"é😀.csv\", csv)";
    let src = format!("{first} range(Colour).return");
    assert_ne!(first.len(), first.chars().count());
    let col = first.chars().count() + 1 + 7; // a space, then `Colour` is the 7th char of `range(Colour)`
    let path = write_temp("multibyte_column.marigold", &src);
    let want = format!("{}:1:{col}: error[undefined-enum]: ", path.display());
    let emit = emit_file(&[], &path);
    assert!(s(&emit.stderr).starts_with(&want), "{}", s(&emit.stderr));
    assert!(emit.stdout.is_empty());
    let check = check_file(&[], &path);
    assert!(s(&check.stdout).starts_with(&want), "{}", s(&check.stdout));
}

#[test]
fn only_errors_are_printed_when_the_program_also_has_warnings() {
    // `check` prints a warning (1:5) and an error (2:7) for this program; `emit` only the error.
    let path = write_temp("warning_and_error.marigold", "a = a\nrange(Colour).return");
    let check = check_file(&[], &path);
    assert!(
        s(&check.stdout).contains(":1:5: warning["),
        "{}",
        s(&check.stdout)
    );
    let human = emit_file(&[], &path);
    assert_eq!(human.status.code(), Some(1));
    assert!(
        !s(&human.stderr).contains("warning["),
        "{}",
        s(&human.stderr)
    );
    assert!(
        s(&human.stderr).contains(":2:7: error[undefined-enum]"),
        "{}",
        s(&human.stderr)
    );
    let json = emit_file(&["--format", "json"], &path);
    let errors: Value = serde_json::from_slice(&json.stderr).unwrap();
    assert_eq!(errors.as_array().unwrap().len(), 1, "{errors}");
    assert_eq!(errors[0]["severity"], "error");
    assert!(json.stdout.is_empty());
}

#[test]
fn human_diagnostic_positions_are_line_and_scalar_column() {
    let src = "range(0, 1).return\n\"éé😀\" range(Colour).return";
    let path = write_temp("pos.marigold", src);
    let out = emit_file(&[], &path);
    assert_eq!(out.status.code(), Some(1));
    // pest reports the failure at the start of the unparsable chunk: line 2, column 1.
    assert!(
        s(&out.stderr).starts_with(&format!("{}:2:1: error[syntax-error]:", path.display())),
        "{}",
        s(&out.stderr)
    );
    let src = "range(0, 1).return\r\nrange(Colour).return\r\n";
    let out = emit_file(&[], &write_temp("pos_crlf.marigold", src));
    assert!(
        s(&out.stderr).contains(":2:7: error[undefined-enum]"),
        "{}",
        s(&out.stderr)
    );
}

#[test]
fn human_diagnostics_include_the_help_line_literally() {
    // `emit` and `check` share the human renderer, so comparing one with the other cannot
    // notice a line they both drop: pin the exact text.
    let path = write_temp("help.marigold", "range(Color).return");
    let want = format!(
        "{}:1:7: error[undefined-enum]: range(Color): 'Color' is not a declared enum in this program\n  help: no enums are declared in this program\n",
        path.display()
    );
    assert_eq!(s(&emit_file(&[], &path).stderr), want);
    assert_eq!(s(&check_file(&[], &path).stdout), want);
}

#[test]
fn exit_codes_always_agree_with_check_on_every_input_we_have() {
    let mut paths: Vec<PathBuf> = golden_programs().into_iter().map(|(_, p, _)| p).collect();
    paths.extend(corpus());
    let mut temps = Vec::new();
    for (name, src) in BAD_PROGRAMS {
        temps.push(write_temp(&format!("cmp_{name}.marigold"), src));
    }
    temps.push(write_temp("cmp_warn.marigold", "a = a\na.return"));
    temps.push(write_temp("cmp_info.marigold", "range(0, 1).map(f).return"));
    temps.push(write_temp("cmp_undefined_var.marigold", "xs.return"));
    paths.extend(temps.iter().map(|t| t.to_path_buf()));
    for path in paths {
        let emit = emit_file(&[], &path);
        let check = check_file(&[], &path);
        assert_eq!(
            emit.status.code(),
            check.status.code(),
            "{}",
            path.display()
        );
        assert_eq!(
            emit.stdout.is_empty(),
            emit.status.code() != Some(0)
                || marigold_parse(&std::fs::read_to_string(&path).unwrap())
                    .unwrap()
                    .is_empty()
        );
    }
}

#[test]
fn warnings_and_information_do_not_fail_emit_and_are_not_printed() {
    for (name, src) in [
        ("warn", "a = a\na.return"),
        ("info", "range(0, 1).map(f).return"),
        ("undeclared", "xs.return"),
    ] {
        let path = write_temp(&format!("quiet_{name}.marigold"), src);
        for format in ["rust", "json"] {
            let out = emit_file(&["--format", format], &path);
            assert_eq!(out.status.code(), Some(0), "{name} {format}: {out:?}");
            assert!(out.stderr.is_empty(), "{name} {format}: {}", s(&out.stderr));
        }
    }
}

#[test]
fn unreadable_input_exits_two_with_empty_stdout() {
    let dir = temp_dir();
    let invalid_utf8 = write_temp("latin1.marigold", b"range(0, 1).return \xff\xfe");
    for path in [
        PathBuf::from("/nonexistent/definitely-missing.marigold"),
        dir.clone(),
        invalid_utf8.to_path_buf(),
    ] {
        for format in ["rust", "json"] {
            let out = emit_file(&["--format", format], &path);
            assert_eq!(
                out.status.code(),
                Some(2),
                "{} {format}: {out:?}",
                path.display()
            );
            assert!(out.stdout.is_empty());
            assert!(
                s(&out.stderr).starts_with(&format!("error: could not read {}:", path.display())),
                "{}",
                s(&out.stderr)
            );
        }
    }
    let out = emit_stdin(&[], b"range(0, 1).return \xff");
    assert_eq!(out.status.code(), Some(2), "{out:?}");
    assert!(out.stdout.is_empty());
    assert!(
        s(&out.stderr).starts_with("error: could not read <stdin>:"),
        "{}",
        s(&out.stderr)
    );
}

#[test]
fn usage_errors_exit_two() {
    let out = marigold()
        .args(["emit", "--format", "xml"])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2), "{out:?}");
    assert!(out.stdout.is_empty());
    let err = s(&out.stderr);
    assert!(
        err.contains("xml") && err.contains("rust") && err.contains("json"),
        "{err}"
    );
    let out = marigold()
        .args(["emit", "a.marigold", "b.marigold"])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2), "{out:?}");
}

#[test]
fn help_documents_the_command_formats_and_units() {
    let top = marigold().arg("--help").output().unwrap();
    assert!(s(&top.stdout).contains("emit"), "{}", s(&top.stdout));
    let out = marigold().args(["emit", "--help"]).output().unwrap();
    assert_eq!(out.status.code(), Some(0));
    let help = s(&out.stdout);
    for needle in [
        "--format",
        "rust",
        "json",
        "byte offsets",
        "1-based",
        "scalar",
        "build features",
        "let _ = include!(\"gen.rs\").await;",
        "stderr",
        "TokenStream",
        "a file named `-`",
    ] {
        assert!(
            help.contains(needle),
            "`emit --help` should mention {needle:?}:\n{help}"
        );
    }
}

#[test]
fn dash_is_a_file_name_not_stdin() {
    for format in ["rust", "json"] {
        let out = marigold()
            .args(["emit", "--format", format, "-"])
            .stdin(Stdio::null())
            .output()
            .unwrap();
        assert_eq!(out.status.code(), Some(2), "{format}");
        assert!(out.stdout.is_empty());
        assert!(
            s(&out.stderr).starts_with("error: could not read -:"),
            "{}",
            s(&out.stderr)
        );
    }
}

#[cfg(target_os = "linux")]
#[test]
fn a_full_disk_is_exit_two_not_silent_success() {
    let path = write_temp("full.marigold", "range(0, 3).return");
    let out = marigold()
        .arg("emit")
        .arg(&*path)
        .stdout(
            std::fs::OpenOptions::new()
                .write(true)
                .open("/dev/full")
                .unwrap(),
        )
        .stderr(Stdio::piped())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2), "{out:?}");
    assert!(
        s(&out.stderr).contains("could not write to stdout"),
        "{}",
        s(&out.stderr)
    );
}

#[test]
fn input_of_exactly_the_check_limit_gets_the_diagnostics_check_gives() {
    // 10 MiB exactly: `check` still parses it, so `emit` must report the same (real) position, not
    // its fallback for larger inputs.
    const MAX: usize = 10 * 1024 * 1024;
    let mut src = " ".repeat(MAX - "range(0,".len());
    src.push_str("range(0,");
    assert_eq!(src.len(), MAX);
    let path = write_temp("exactly_max.marigold", &src);
    let check = check_file(&["--format", "json"], &path);
    let checked = stdout_json(&check);
    assert_ne!(checked[0]["code"], "input-too-large");
    let emit = emit_file(&["--format", "json"], &path);
    assert_eq!(emit.status.code(), Some(1));
    let emitted: Value = serde_json::from_slice(&emit.stderr).unwrap();
    assert_eq!(emitted, checked);
}

#[test]
fn oversized_valid_input_is_emitted_although_check_rejects_it() {
    // marigold_generate has no size limit (it mirrors marigold_parse); only `check` is capped.
    let path = write_temp("huge_ws.marigold", " ".repeat(10 * 1024 * 1024 + 1));
    let out = emit_file(&[], &path);
    assert_eq!(out.status.code(), Some(0), "{:?}", out.status);
    assert_eq!(s(&out.stdout), marigold_parse("").unwrap());
    assert_eq!(check_file(&[], &path).status.code(), Some(1));
}

#[test]
fn oversized_invalid_input_still_exits_one_and_prints_nothing_on_stdout() {
    // `check` would answer `input-too-large` here, which would be a lie for `emit`; `emit` falls
    // back to the parse error message. (The message embeds the whole line: ~2x the input.)
    let path = write_temp(
        "huge_bad.marigold",
        format!("{}range(0,", " ".repeat(10 * 1024 * 1024 + 1)),
    );
    let out = emit_file(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
    let err: Value = serde_json::from_slice(&out.stderr).unwrap();
    assert_eq!(err[0]["severity"], "error");
    assert_ne!(err[0]["code"], "input-too-large");
    assert!(err[0]["message"].as_str().unwrap().contains("Parse error"));
    // the synthetic diagnostic has the shape and key order of `check`'s
    assert_eq!(err.as_array().unwrap().len(), 1);
    assert_eq!(err[0]["code"], "syntax-error");
    let end = 10 * 1024 * 1024 + 1 + "range(0,".len();
    assert_eq!(err[0]["range"], json!({"start": end, "end": end}));
    assert_eq!(err[0].as_object().unwrap().len(), 4);
    assert!(
        s(&out.stderr).starts_with(&format!("[\n  {{\n    \"range\": {{\n      \"start\": {end},\n      \"end\": {end}\n    }},\n    \"severity\": \"error\",\n    \"code\": \"syntax-error\",\n    \"message\": \"Parse error")),
        "layout of the diagnostic differs from `check --format json`"
    );

    // human format: one line with the usual prefix, at the position of the failure
    let out = emit_file(&[], &path);
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
    assert!(
        s(&out.stderr).starts_with(&format!(
            "{}:1:{}: error[syntax-error]: Parse error",
            path.display(),
            10 * 1024 * 1024 + 1 + "range(0,".len() + 1
        )),
        "{}",
        &s(&out.stderr)[..200.min(out.stderr.len())]
    );
    assert!(s(&out.stderr).ends_with('\n'));
}

#[test]
fn oversized_invalid_input_has_a_short_escaped_message() {
    // The raw parse error embeds the offending line twice (23 MB for this file) and whatever
    // control characters it holds: an escape sequence in the input would reach the terminal.
    let tail = "\nrange(0,\u{1b}]0;pwned\u{7}\u{1b}[31mred";
    let src = format!("{}{tail}", " ".repeat(10 * 1024 * 1024 + 1));
    let path = write_temp("huge_escape.marigold", &src);
    for format in ["rust", "json"] {
        let out = emit_file(&["--format", format], &path);
        assert_eq!(out.status.code(), Some(1), "{format}");
        assert!(out.stdout.is_empty(), "{format}");
        assert!(
            out.stderr.len() < 4096,
            "{format}: {} bytes of diagnostics",
            out.stderr.len()
        );
        assert!(
            !out.stderr.iter().any(|&b| b == 0x1b || b == 0x07),
            "{format}: control bytes reached stderr: {:?}",
            s(&out.stderr)
        );
        assert!(out.stderr.ends_with(b"\n"), "{format}");
    }
    // the position is the line:column pest names, as an offset into the file
    let json = emit_file(&["--format", "json"], &path);
    let diagnostics: Value = serde_json::from_slice(&json.stderr).unwrap();
    let human = emit_file(&[], &path);
    let head = s(&human.stderr)
        .split(": error[syntax-error]")
        .next()
        .unwrap();
    let mut parts = head.rsplitn(3, ':');
    let (column, line): (usize, usize) = (
        parts.next().unwrap().parse().unwrap(),
        parts.next().unwrap().parse().unwrap(),
    );
    assert_eq!(line, 2, "{head}");
    let line_start = src.find('\n').unwrap() + 1;
    let offset = line_start
        + src[line_start..]
            .char_indices()
            .nth(column - 1)
            .map_or(src.len() - line_start, |(i, _)| i);
    assert_eq!(diagnostics[0]["range"]["start"], json!(offset));
    assert_eq!(diagnostics[0]["range"]["end"], json!(offset));
}

// ---------------------------------------------------------------------------------------------
// isolation and robustness
// ---------------------------------------------------------------------------------------------

#[test]
fn emit_has_no_side_effects_and_never_touches_the_marigold_cache() {
    let home = std::env::temp_dir().join(format!("marigold-emit-home-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&home);
    std::fs::create_dir_all(&home).unwrap();
    let cwd = std::env::temp_dir().join(format!("marigold-emit-cwd-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&cwd);
    std::fs::create_dir_all(&cwd).unwrap();
    let input = cwd.join("in.marigold");
    std::fs::write(&input, "range(0, 3).return").unwrap();
    let before = std::fs::metadata(&input).unwrap().modified().unwrap();

    for format in ["rust", "json"] {
        for broken in [false, true] {
            let mut cmd = marigold();
            cmd.current_dir(&cwd)
                .env("HOME", &home)
                .args(["emit", "--format", format]);
            cmd.arg(if broken {
                "missing.marigold"
            } else {
                "in.marigold"
            });
            let out = cmd.output().unwrap();
            assert_eq!(out.status.code(), Some(if broken { 2 } else { 0 }));
        }
    }
    let ls = |d: &Path| -> Vec<String> {
        let mut v: Vec<_> = std::fs::read_dir(d)
            .unwrap()
            .flatten()
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .collect();
        v.sort();
        v
    };
    assert_eq!(
        ls(&home),
        Vec::<String>::new(),
        "emit must not create ~/.marigold"
    );
    assert_eq!(ls(&cwd), vec!["in.marigold".to_string()]);
    assert_eq!(
        std::fs::metadata(&input).unwrap().modified().unwrap(),
        before
    );
    assert_eq!(
        std::fs::read_to_string(&input).unwrap(),
        "range(0, 3).return"
    );
    let _ = std::fs::remove_dir_all(&home);
    let _ = std::fs::remove_dir_all(&cwd);
}

#[test]
fn relative_paths_resolve_against_the_working_directory() {
    let dir = temp_dir().join("relative");
    std::fs::create_dir_all(&dir).unwrap();
    let program = TempFile(dir.join("p.marigold"));
    std::fs::write(&*program, "range(0, 3).return").unwrap();
    let out = marigold()
        .current_dir(&dir)
        .args(["emit", "p.marigold"])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(
        s(&out.stdout),
        marigold_parse("range(0, 3).return").unwrap()
    );
    let err = marigold()
        .current_dir(&dir)
        .args(["emit", "bad/../nope.marigold"])
        .output()
        .unwrap();
    assert!(
        s(&err.stderr).starts_with("error: could not read bad/../nope.marigold:"),
        "{}",
        s(&err.stderr)
    );
}

#[test]
fn a_closed_stdout_pipe_is_not_a_panic() {
    // `marigold emit big.marigold | head`: Rust ignores SIGPIPE, so a `println!`-style writer panics
    // with "failed printing to stdout" and exits 101. The output (>64 KiB) cannot fit the pipe.
    let src: String = (0..3000)
        .map(|i| format!("x{i} = range(0, {i})\nx{i}.return\n"))
        .collect();
    let path = write_temp("big_pipe.marigold", &src);
    for format in ["rust", "json"] {
        let mut child = marigold()
            .args(["emit", "--format", format])
            .arg(&*path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        drop(child.stdout.take());
        let out = child.wait_with_output().unwrap();
        assert!(
            !s(&out.stderr).contains("panicked"),
            "{format}: {}",
            s(&out.stderr)
        );
        assert_eq!(out.status.code(), Some(0), "{format}: {out:?}");
    }
}

#[test]
fn large_programs_round_trip_and_stay_fast() {
    use std::time::{Duration, Instant};
    let src: String = (0..3000)
        .map(|i| format!("x{i} = range(0, {i})\nx{i}.return\nstruct S{i} {{ a: u8 }}\n"))
        .collect();
    let path = write_temp("large.marigold", &src);
    let t = Instant::now();
    let out = emit_file(&["--format", "json"], &path);
    assert!(t.elapsed() < Duration::from_secs(30), "{:?}", t.elapsed());
    assert_eq!(out.status.code(), Some(0));
    let doc = stdout_json(&out);
    validate_v1(&doc).unwrap();
    check_semantics(&doc).unwrap();
    // per i: declaration + input + runner, stream + input variable, struct + field
    assert_eq!(doc["segments"].as_array().unwrap().len(), 7 * 3000);
}

#[test]
fn check_is_unchanged_by_the_emit_refactor() {
    // The detailed oracle is the baseline script (`compare_check_baseline.sh`); this is the cheap
    // in-repo canary for the shared read_source / renderer code path.
    let path = write_temp(
        "canary.marigold",
        "range(0, 1).return\nrange(Colour).return",
    );
    let out = check_file(&[], &path);
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        s(&out.stdout),
        format!(
            "{}:2:7: error[undefined-enum]: range(Colour): 'Colour' is not a declared enum in this program\n  help: no enums are declared in this program\n",
            path.display()
        )
    );
    assert!(out.stderr.is_empty());
}
