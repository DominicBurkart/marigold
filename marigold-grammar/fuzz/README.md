# `marigold-grammar` fuzzing

Continuous fuzz targets for the Marigold parser and analyzer, built on
[`cargo-fuzz`](https://github.com/rust-fuzz/cargo-fuzz) /
[`libfuzzer-sys`](https://crates.io/crates/libfuzzer-sys).

## Install

Fuzzing requires a nightly toolchain and `cargo-fuzz`:

```bash
rustup toolchain install nightly
cargo install cargo-fuzz
```

## Run a target

From the `marigold-grammar/` directory:

```bash
# Parser fuzzer: feeds arbitrary bytes to PestParser::parse_program
cargo +nightly fuzz run fuzz_parse

# Analyzer fuzzer: feeds arbitrary bytes to marigold_analyze
cargo +nightly fuzz run fuzz_analyze
```

Each target loads the tracked seed corpus under `fuzz/corpus/<target>/`
before starting, then writes additional inputs it discovers under the
same directory. Crashes land in `fuzz/artifacts/<target>/`.

Time-box a run with `-max_total_time`:

```bash
cargo +nightly fuzz run fuzz_parse -- -max_total_time=60
```

## Seed corpus

Seed inputs for every target live in `fuzz/corpus/<target>/` and are
tracked in git so every developer (and CI) starts from the same
baseline. `fuzz/.gitignore` explicitly does **not** ignore `corpus/`.

To add a new seed, drop the file into the target's corpus directory and
commit it:

```bash
cp my_new_input.marigold fuzz/corpus/fuzz_parse/
git add fuzz/corpus/fuzz_parse/my_new_input.marigold
```

## Reproducing a crash

`cargo-fuzz` prints the path to the offending input on crash. Replay it
against the target with:

```bash
cargo +nightly fuzz run fuzz_parse fuzz/artifacts/fuzz_parse/crash-<hash>
```

Crashes should be minimised and copied into the seed corpus (or, for
sensitive inputs, attached to the GitHub issue) before fixing.
