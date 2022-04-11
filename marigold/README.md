# marigold

[![crates.io](https://img.shields.io/crates/v/marigold.svg)](https://crates.io/crates/marigold)
[![docs.rs](https://img.shields.io/docsrs/marigold.svg)](https://docs.rs/marigold)
[![tests](https://github.com/DominicBurkart/marigold/actions/workflows/tests.yaml/badge.svg?branch=main&event=push)](https://github.com/DominicBurkart/marigold/actions/workflows/tests.yaml)
[![style](https://github.com/DominicBurkart/marigold/actions/workflows/pre-commit.yaml/badge.svg?branch=main&event=push)](https://github.com/DominicBurkart/marigold/actions/workflows/pre-commit.yaml)
[![wasm](https://github.com/DominicBurkart/marigold/actions/workflows/wasm.yaml/badge.svg?branch=main&event=push)](https://github.com/DominicBurkart/marigold/actions/workflows/wasm.yaml)
[![last commit](https://img.shields.io/github/last-commit/dominicburkart/marigold)](https://github.com/DominicBurkart/marigold)

(WIP) Marigold is a domain-specific language for data pipelining and analysis.
Marigold compiles to asynchronous Rust, and can be accessed in a macro:

```rust
use marigold::m;

let is_odd = |i: &i32| i % 2 == 1;

let odd_digits = m!(
  range(0, 10)
    .filter(is_odd)
    .to_vec()
    .return
).await;

println!("{:?}", odd_digits); // [1, 3, 5, 7, 9]
```
