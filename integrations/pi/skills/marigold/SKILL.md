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

## Navigation tools

All tools take only `.marigold` files. `line` and `column` are 1-based;
columns count UTF-16 code units, the same as the `path:line:col`
locations in diagnostics, so a location printed by one tool can be passed
straight to another. Every tool says explicitly when nothing was found.

- `marigold_symbols` (`path`): outline of a file. Start here to find the
  declarations in an unfamiliar file.
- `marigold_workspace_symbols` (`query`): find a function, struct, enum or
  stream variable by name across every `.marigold` file under the working
  directory.
- `marigold_definition` (`path`, `line`, `column`): jump from a use to its
  declaration.
- `marigold_references` (`path`, `line`, `column`, optional
  `include_declaration`, default true): every use of a symbol. Run it
  before changing a signature or deleting a declaration.
- `marigold_hover` (`path`, `line`, `column`): the declaration or stream
  at the position plus its complexity (cardinality, time, space, whether
  it collects input). Use it to compare stream shapes before choosing
  between `permutations`, `combinations` and filters.
- `marigold_rename` (`path`, `line`, `column`, `new_name`, optional
  `apply`): rename a symbol everywhere in the file.

## Renaming

Prefer `marigold_rename` over search and replace, because it only touches
the real symbol.

1. Call it without `apply`. It lists the proposed edits per file and
   changes nothing.
2. Review the edits. Name clashes and invalid identifiers come back as
   errors with the server's message.
3. Call it again with `apply: true`. The edits are written only if the
   file is inside the working directory and unchanged since it was read;
   if it changed, re-run the preview. The result ends with fresh
   diagnostics for the renamed file, which you must read.

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

Use `marigold_hover` for a quick complexity line, or run
`marigold analyze <file>` to get JSON with each stream's cardinality and
time/space complexity before choosing between `permutations`,
`combinations`, and filters on large inputs.
