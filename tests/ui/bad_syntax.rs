// This file is a trybuild compile-fail fixture.
// It verifies that invalid Marigold DSL syntax surfaces as a `compile_error!`
// diagnostic rather than a proc-macro panic.
fn main() {
    // "bad syntax" is valid Rust tokens but not valid Marigold grammar.
    let _ = marigold::m!(bad syntax);
}
