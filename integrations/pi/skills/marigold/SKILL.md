---
name: marigold
description: Write, fix, and analyze Marigold (.marigold) stream-processing programs. Use when creating or editing .marigold files or when marigold diagnostics appear after an edit.
---

# Marigold

Marigold is a streaming DSL that compiles to async Rust. A program is a
sequence of declarations (`struct`, `enum`, `fn`) and streams such as
`range(0, 10).map(f).filter(g).return`.

## Feedback loop

- After every `edit` or `write` to a `.marigold` file, diagnostics are
  appended to the tool result as `path:line:col: severity[code]: message`.
  Fix every error and review every warning before moving on.
- Call the `marigold_check` tool to re-check a file without editing it.
- `help:` lines under a diagnostic list valid alternatives (for example
  the enums declared in the program).

## Diagnostic codes

- `syntax-error`: the text does not match the grammar; the location is
  where parsing stopped.
- `undefined-enum`: `range(Name)` names an enum that is not declared in
  the program.
- `bounds-violation`: an `int[min, max]` or `uint[min, max]` field has
  min > max, or a negative min for `uint`.
- `undefined-type`: a bound expression such as `Name.len()` refers to an
  unknown enum or field.
- `cyclic-bound`: bound expressions refer to each other in a cycle.
- `undefined-stream-variable` (warning): a stream variable is read before
  the line that declares it, or by its own declaration; read only variables
  declared above.
- `input-too-large`: the file is over the 10 MiB limit and was not
  checked.

Warnings do not make a file invalid, but they usually point at a mistake.
Do not treat a result with no errors as clean while warnings remain.

Information diagnostics mean the name is not declared in this program:

- `undefined-stream-variable` (information): a stream variable is used but
  not declared in this program. It may be a Rust binding with a `get()`
  method returning a stream.
- `undefined-fn`: a function named in `map`, `filter`, `fold` and the like
  is not declared with `fn` in this program.
- `undefined-struct`: `struct=T` names a struct that is not declared.
- `resolver-diagnostics-truncated`: more than 50 undefined-name
  diagnostics were found; only the first 50 are reported.

An information diagnostic can be a false alarm when the name is a Rust
item in scope around the program. Otherwise fix it.

## Complexity

Run `marigold analyze <file>` to get JSON with each stream's cardinality
and time/space complexity before choosing between `permutations`,
`combinations`, and filters on large inputs.
