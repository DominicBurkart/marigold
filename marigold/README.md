# Marigold

[![crates.io](https://img.shields.io/crates/v/marigold.svg)](https://crates.io/crates/marigold)
[![docs.rs](https://img.shields.io/docsrs/marigold.svg)](https://docs.rs/marigold)
[![website](https://img.shields.io/badge/-website-blue)](https://dominic.computer/marigold)
[![lines of code](https://raw.githubusercontent.com/DominicBurkart/marigold/main/development_metadata/badges/lines_of_code.svg)](https://github.com/DominicBurkart/marigold)
[![contributors](https://raw.githubusercontent.com/DominicBurkart/marigold/main/development_metadata/badges/contributors.svg)](https://github.com/DominicBurkart/marigold/graphs/contributors)
[![bench](https://github.com/DominicBurkart/marigold/workflows/bench/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/bench.yaml)
[![codecov](https://codecov.io/gh/DominicBurkart/nanna-coder/graph/badge.svg?token=47S7xzd3Ny)](https://codecov.io/gh/DominicBurkart/nanna-coder)
[![tests](https://github.com/DominicBurkart/marigold/workflows/tests/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/tests.yaml)
[![style](https://github.com/DominicBurkart/marigold/workflows/style/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/style.yaml)
[![wasm](https://github.com/DominicBurkart/marigold/workflows/wasm/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/wasm.yaml)
[![last commit](https://img.shields.io/github/last-commit/dominicburkart/marigold)](https://github.com/DominicBurkart/marigold)

Marigold is an imperative, domain-specific language for data pipelining and
analysis using async streams. It can be used as a standalone language or within
Rust programs.

```rust
use marigold::m;

let odd_digits = m!(
  fn is_odd(i: i32) -> bool {
    i.wrapping_rem(2) == 1
  }

  range(0, 10)
    .filter(is_odd)
    .return
).await.collect::<Vec<_>>().await;

println!("{:?}", odd_digits); // [1, 3, 5, 7, 9]
```

## Quickstart

### Install

```sh
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/DominicBurkart/marigold/main/install.sh | sh
```

Or via cargo:

```sh
cargo install marigold -F cli
```

Or from source:

```sh
git clone https://github.com/DominicBurkart/marigold.git
cd marigold
cargo install --path marigold -F cli
```

### Hello World

Create `hello_world.marigold`:

```marigold
range(0, 5).write_file("/dev/stdout", csv)
```

Run it:

```sh
marigold run hello_world.marigold
```

Output:

```text
0
1
2
3
4
```

### Static Analysis

Marigold can statically analyze your program's complexity:

```sh
marigold analyze hello_world.marigold
```

```json
{
  "streams": [
    {
      "description": "input.write_file(...)",
      "cardinality": "5",
      "time_class": "O(1)",
      "exact_time": "O(1)",
      "space_class": "O(1)",
      "exact_space": "O(1)",
      "collects_input": false
    }
  ],
  "program_time": "O(1)",
  "program_exact_time": "O(1)",
  "program_space": "O(1)",
  "program_exact_space": "O(1)",
  "program_cardinality": "5"
}
```

### Editor and Agent Integration

Marigold ships an optional language server, `marigold-lsp`, that reports
diagnostics for `.marigold` files and supports go-to-definition, references,
rename, symbols and hover (with a complexity estimate). None of it is needed to
build or run Marigold programs.

Install the server from a clone of this repository:

```sh
cargo install --path marigold-lsp
```

Then connect your tool using the matching directory under
[integrations/](https://github.com/DominicBurkart/marigold/tree/main/integrations):
Claude Code (plugin), opencode (`lsp` config), Codex (MCP server and
`AGENTS.md`) and pi (extension). `marigold-lsp mcp` serves the same checks,
symbols and complexity estimates as MCP tools for agents without an LSP client.

For tools that can only run shell commands, build the CLI with `-F cli` and run:

```sh
marigold check hello_world.marigold
marigold check --format json hello_world.marigold
```

Diagnostics have three levels. Errors make the program invalid and the command
exit 1. Warnings flag likely mistakes, such as a stream variable read before it
is declared. Information covers names that may be Rust items in scope inside
`m!()`, such as an undeclared function, and can be ignored in that case.
Warnings and information leave the exit code at 0, so read them as well.

Opt-in telemetry annotations (observed inputs, runs and errors per stream
node) are described in
[integrations/telemetry](https://github.com/DominicBurkart/marigold/tree/main/integrations/telemetry).

## Runtimes

By default, Marigold works in a single future and can work with any runtime.

The `tokio` and `async-std` features allow Marigold to spawn additional tasks,
enabling parallelism for multithreaded runtimes.

Marigold supports async tracing, e.g. with tokio-console.

## Platforms

Marigold's CI builds against aarch64, arm, WASM, and x86 targets, and builds
the x86 target in mac and windows environments.
