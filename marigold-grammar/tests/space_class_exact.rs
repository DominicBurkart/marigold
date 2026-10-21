use marigold_grammar::complexity::ProgramComplexity;
use marigold_grammar::{marigold_analyze, marigold_stream_complexities};
use std::path::{Path, PathBuf};

const F: &str = "fn f(a: i32) -> i32 { a }\nfn g(a: i32) -> i32 { a }\n";

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(rd) = std::fs::read_dir(dir) else {
        return;
    };
    for e in rd.flatten() {
        let p = e.path();
        if p.is_dir() {
            if p.file_name()
                .is_some_and(|n| n == "target" || n == "node_modules")
            {
                continue;
            }
            collect(&p, out);
        } else if p.extension().is_some_and(|x| x == "marigold" || x == "mg") {
            out.push(p);
        }
    }
}

fn assert_invariants(label: &str, p: &ProgramComplexity) {
    for (i, s) in p.streams.iter().enumerate() {
        assert_eq!(
            s.space_class,
            s.exact_space.simplified(),
            "{label}: stream {i} ({}) space_class {} != exact_space {} simplified",
            s.description,
            s.space_class,
            s.exact_space
        );
    }
    let max = p
        .streams
        .iter()
        .map(|s| s.space_class.clone())
        .fold(marigold_grammar::complexity::ComplexityClass::O1, |a, b| {
            a.max(b)
        });
    assert_eq!(
        p.program_space, max,
        "{label}: program_space != max of streams"
    );
    assert_eq!(
        p.program_space,
        p.program_exact_space.simplified(),
        "{label}: program_space != program_exact_space.simplified()"
    );
}

fn assert_nodes(label: &str, src: &str) {
    if let Ok(nodes) = marigold_stream_complexities(src) {
        for n in nodes {
            assert_eq!(
                n.complexity.space_class,
                n.complexity.exact_space.simplified(),
                "{label}: node {:?} space_class != exact_space simplified",
                n.name
            );
        }
    }
}

#[test]
fn named_and_inline_stream_agree() {
    let a = marigold_analyze(&format!("{F}x = range(0, 5).map(f)\nx.return")).unwrap();
    let b = marigold_analyze(&format!("{F}range(0, 5).map(f).return")).unwrap();
    assert_eq!(
        a.streams.last().unwrap().space_class,
        b.streams[0].space_class
    );
    assert_eq!(
        a.streams.last().unwrap().exact_space,
        b.streams[0].exact_space
    );
    assert_eq!(a.program_space, b.program_space);
    assert_eq!(a.program_exact_space, b.program_exact_space);
}

#[test]
fn stream_variable_programs_satisfy_invariants() {
    let programs = [
        "x = range(0, 5).map(f)\nx.return",
        "x = range(0, 5).map(f)\ny = x.map(g)\ny.return",
        "x = range(0, 5).map(f)\ny = x.map(g)\nz = y.map(f)\nz.return",
        "x = range(0, 5)\nx.map(f).return\nx.map(g).return",
        "x = range(0, 5).map(f)\nx.return\nx.map(g).return",
        "x = range(0, 5)\nx.permutations(2).return",
        "x = range(0, 5).map(f)\nx.permutations(2).return",
        "x = range(0, 5).map(f)\ny = x.permutations(2)\ny.return",
        "x = range(0, 5)\ny = x.map(f)\ny.permutations(2).return\ny.return",
        "x = range(0, 5)\nx.return",
    ];
    for p in programs {
        let src = format!("{F}{p}");
        let r = marigold_analyze(&src).unwrap_or_else(|e| panic!("{p}: {e}"));
        assert_invariants(p, &r);
        assert_nodes(p, &src);
    }
}

#[test]
fn all_fixture_programs_satisfy_invariants() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = Vec::new();
    collect(&root.join("marigold-grammar/tests/programs"), &mut files);
    collect(&root.join("examples"), &mut files);
    files.push(root.join("hello_world.marigold"));
    let mut covered = 0;
    for f in &files {
        let Ok(src) = std::fs::read_to_string(f) else {
            continue;
        };
        let Ok(r) = marigold_analyze(&src) else {
            continue;
        };
        assert_invariants(&f.display().to_string(), &r);
        assert_nodes(&f.display().to_string(), &src);
        covered += 1;
    }
    eprintln!("covered {covered} of {} fixture programs", files.len());
    assert!(covered >= 20, "only {covered} fixtures analyzed");
}
