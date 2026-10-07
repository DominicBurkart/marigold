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
  appended to the tool result as `path:line:col: error[code]: message`.
  Fix every error before moving on.
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

## Complexity

Run `marigold analyze <file>` to get JSON with each stream's cardinality
and time/space complexity before choosing between `permutations`,
`combinations`, and filters on large inputs.
