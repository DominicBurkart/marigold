use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let programs = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("programs");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap());
    println!("cargo:rerun-if-changed=programs");
    println!("cargo:rerun-if-changed=build.rs");
    for entry in fs::read_dir(&programs).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) != Some("marigold") {
            continue;
        }
        let source = fs::read_to_string(&path).unwrap();
        let generated = marigold_grammar::marigold_parse(&source)
            .unwrap_or_else(|e| panic!("{}: {e}", path.display()));
        let name = path.file_stem().unwrap().to_str().unwrap();
        fs::write(
            out.join(format!("{name}.rs")),
            format!("{{\n{generated}\n}}\n"),
        )
        .unwrap();
    }
}
