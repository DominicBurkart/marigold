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
your next turn. Fix every error, fix every warning, and read every information
diagnostic before finishing.

Warning: does not make a file invalid, but is almost always a real mistake.

- `undefined-stream-variable`: a stream variable is read before the line that
  declares it, or by its own declaration.

Information: the name may be a Rust item in scope inside `m!()`, because fns and
structs normally live in the Rust code around the program.

- `undefined-stream-variable`: a stream variable is used but not declared in
  this program. It may be a Rust binding with a `get()` method returning a
  stream.
- `undefined-fn`: a function named in `map`, `filter`, `fold` and the like is
  not declared with `fn` in this program.
- `undefined-struct`: `struct=T` names a struct that is not declared.
- `resolver-diagnostics-truncated`: more than 50 undefined-name diagnostics
  were found; only the first 50 are reported. Fix those and check again.

Ignore these information diagnostics only when the name is a Rust item
in scope inside `m!()`; otherwise fix it. `marigold check` exits 0 for
warnings and information and 1 on any error, and, if the MCP server is
registered, `marigold_check` returns `ok: true` with `error_count`,
`warning_count` and `info_count`, so read the diagnostics, not just the ok or
error counts.

## Complexity

Run `marigold analyze <file>` to get JSON with each stream's cardinality and
time/space complexity before choosing between `permutations`, `combinations`,
and filters on large inputs.

## Generated Rust and source map

Run `marigold emit <file>` to print the Rust code the `m!` macro generates for
a program, and `marigold emit --format json <file>` for a JSON document that
maps each top-level Marigold expression to the generated code (byte offsets,
1-based lines, columns in Unicode scalar values, the `source` and `rust` texts
embedded). If the program does not compile, stdout stays empty and the errors
go to stderr as `marigold check` prints them. Offsets refer to the file as it
is on disk, not to the text `m!()` sees. The output of the `cargo install
marigold -F cli` build matches `marigold run`; a build with `-F cli,tokio`
generates different code.
