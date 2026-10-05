---
name: marigold
description: Write, fix, and analyze Marigold (.marigold) stream-processing programs. Use when creating or editing .marigold files or when marigold diagnostics appear after an edit.
---

# Marigold

Marigold is a streaming DSL that compiles to async Rust. A program is a
sequence of declarations (`struct`, `enum`, `fn`) and streams such as
`range(0, 10).map(f).filter(g).return`.

## Diagnostics

After you edit a `.marigold` file, the language server reports diagnostics on
your next turn. Fix every error and review every warning before finishing.

Warnings do not make a file invalid, but they usually point at a mistake:

- `undefined-stream-variable`: a stream variable is used but not declared in
  this program.
- `undefined-fn`: a function named in `map`, `filter`, `fold` and the like is
  not declared with `fn` in this program.
- `undefined-struct`: `struct=T` names a struct that is not declared.

Read the warnings, not just the ok or error counts: `marigold check` exits 0
and `marigold_check` returns `ok: true` even when warnings remain. A warning
can be a false alarm when the name is defined in Rust code that surrounds the
program. Otherwise fix it, and do not treat a result with no errors as clean
while warnings remain.

## Complexity

Run `marigold analyze <file>` to get JSON with each stream's cardinality and
time/space complexity before choosing between `permutations`, `combinations`,
and filters on large inputs.
