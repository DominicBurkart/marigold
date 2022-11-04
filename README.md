# Marigold

[![website](https://img.shields.io/badge/-website-blue)](https://dominic.computer/marigold)
[![crates.io](https://img.shields.io/crates/v/marigold.svg)](https://crates.io/crates/marigold)
[![docs.rs](https://img.shields.io/docsrs/marigold.svg)](https://docs.rs/marigold)
[![lines of code](https://raw.githubusercontent.com/DominicBurkart/marigold/main/development_metadata/badges/lines_of_code.svg)](https://github.com/DominicBurkart/marigold)
[![contributors](https://raw.githubusercontent.com/DominicBurkart/marigold/main/development_metadata/badges/contributors.svg)](https://github.com/DominicBurkart/marigold/graphs/contributors)
[![bench](https://github.com/DominicBurkart/marigold/workflows/bench/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/bench.yaml)
[![tests](https://github.com/DominicBurkart/marigold/workflows/tests/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/tests.yaml)
[![style](https://github.com/DominicBurkart/marigold/workflows/style/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/style.yaml)
[![wasm](https://github.com/DominicBurkart/marigold/workflows/wasm/badge.svg)](https://github.com/DominicBurkart/marigold/actions/workflows/wasm.yaml)
[![last commit](https://img.shields.io/github/last-commit/dominicburkart/marigold)](https://github.com/DominicBurkart/marigold)

Marigold is a domain-specific language for streaming data pipelining and
analysis. Marigold compiles to asynchronous Rust, and can be accessed in a
macro:

```rust
use marigold::m;

let odd_digits = m!(
  fn is_odd(i: &i32) -> bool {
    i.wrapping_rem(2) == 1
  }

  range(0, 10)
    .filter(is_odd)
    .return
).await.collect::<Vec<_>>();

println!("{:?}", odd_digits); // [1, 3, 5, 7, 9]
```

## Runtimes

By default, Marigold works in a single future and can work with any runtime.

The `tokio` and `async-std` features allow Marigold to spawn additional tasks,
enabling parallelism for multithreaded runtimes.

Marigold supports async tracing, e.g. with tokio-console.

## Platforms

Marigold's CI builds against aarch64, arm, WASM, and x86 targets, and builds
the x86 target in mac and windows environments.
