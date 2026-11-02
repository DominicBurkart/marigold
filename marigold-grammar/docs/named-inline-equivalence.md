# Named and inline streams are equivalent

Status: specification. The analysis rules are implemented (#449) and
redeclaration is rejected (#444). Open issues are listed at the end.

## Principle

A stream variable is a name for a pipeline. For every program, replacing each
use of a declared variable with the variable's definition must not change
anything a user or tool can observe, apart from the items listed under "May
differ".

```marigold
x = range(0, 5).permutations(2)
x.map(f).return
```

is equivalent to

```marigold
range(0, 5).permutations(2).map(f).return
```

The same holds through chains (`y = x.map(g)` is `y = <definition of
x>.map(g)`), for every number of consumers, for `return` and `write_file`
consumers, and for consumers with no functions (`x.return`).

## Must be identical

- **Output items**: The multiset of items returned by the program. Each consumer
  receives the full contents of the variable.
- **Order within one consumer**: Preserved. Each consumer sees the variable's
  items in the order the source and steps produce them.
- **Termination**: A program that terminates inline terminates when named,
  including when a consumer is slow, a consumer is a collecting step
  (`permutations`, `combinations`, `permutations_with_replacement`,
  `keep_first_n`, `fold`), or the output stream is dropped early.
- **`cardinality`**: Per stream and for `program_cardinality`.
- **`time_class` and `exact_time`**: Per stream and for `program_time` and
  `program_exact_time`. A zero-function consumer adds no work of its own.
- **`space_class` and `exact_space`**: Per stream and for `program_space` and
  `program_exact_space`, with `space_class == exact_space.simplified()`.
- **`collects_input`**: True when any step on the path from the source to the
  consumer collects, including steps in the variable's definition and in
  variables it is derived from.
- **Declaration nodes**: The node reported for `y = x.f` equals the node for `y
  = <definition of x>.f`.
- **Hover and the `marigold_complexity` MCP tool**: The complexity text for a
  consumer is identical to the text for its inline twin. Only the displayed
  source line differs.
- **Diagnostics**: Same severity and code for the same problem.
- **What compiles**: Anything that compiles inline compiles when named.

## May differ

- Interleaving of items between different returning streams. Separate consumers
  are merged with `select_all`, in an unspecified order. This also holds for two
  inline streams, so tests compare multisets across consumers and sequences
  within a consumer.
- Wall time and scheduling. A shared variable computes its prefix once and feeds
  each consumer through a one-slot channel, so the slowest consumer paces the
  others (back-pressure). It can be faster than the sum of the inline forms and
  can be slower than the fastest one.
- `description` strings, which name the variable (`x.return` against
  `input.permutations(2).return`).
- Source ranges, node ordering by source position, and the displayed source
  text.
- Work done by the runtime after every consumer has gone away, as long as it
  ends when the source ends.

## Accounting for several consumers

Analysis is done as if inline: each consumer is charged the full cost of the
variable's definition plus its own steps. The program totals therefore count a
shared prefix once per consumer. The runtime computes the prefix once, so the
reported time is an upper bound for the shared case and equals the inline figure
by definition. Decided: program totals are not deduplicated for shared prefixes.
This keeps the rule local (a stream's figures depend on its own text after
substitution) and the totals independent of how the program was written.

Space is the same: each consumer reports the space of its own expanded pipeline.
The channels between a variable and its consumers hold one item each, so `k`
consumers add `k` items, which is a program constant and is not charged.
Collecting steps are charged as they are inline.

A variable with no consumers has no stream, so it adds nothing to the totals.
Its declaration node still reports the variable's own pipeline.

## Redeclaration

Declaring the same variable name twice is an error. `marigold check` reports
`redeclared-stream-variable` with severity error on each later declaration, and
code generation rejects the program, so the macro fails at expansion. The
generated Rust binds each variable to a name and runs it once, so a second
declaration cannot work (#444).

If the language later allows shadowing, each consumer must see the nearest
declaration above it and codegen must give each declaration a unique binding.
The equivalence tests then become sequential: the twin is built by substituting
the definition in scope at each line.

## Undeclared variables

A reference to a name with no declaration in the program is an external source:
a Rust binding in scope inside `m!()` that has a `get()` method returning a
stream. The analyzer reports unknown cardinality, time and space (`O(?)`) for
it, `collects_input: false`, and `marigold check` reports an information-level
diagnostic. The inline twin of such a program is the program itself. The same
unknown-ness must apply to `y = nope.f` and to consumers of `y`, with the same
`exact_time` as a stream that starts at an unknown source.

For these streams `exact_space` carries the unknown, as `exact_time` does, so
`space_class == exact_space.simplified()` holds with no exemption (#448).

## Implementation guidance

The analysis continues one fold over the steps of a pipeline: a variable stores
the state after its own steps, and a consumer or derived variable continues from
it, so equality holds by construction and each step is applied once (#449). For
the runtime, `MultiConsumerStream` should not require `Copy` items or a `Send`
source (#445).

## Pending: hover and MCP test

With #441 fixed, a test can be added in `marigold-lsp` that builds
`complexity_line` for each consumer node of a named program and for its inline
twin and asserts the strings are equal, for at least a collecting variable
consumed with zero functions, a chain, and a fan-out. It is not part of the
analysis change; before #441 it would have pinned the wrong behaviour
(`collects input: no` for a consumer of a collecting variable).

## Tests

`marigold-grammar/tests/named_inline_equivalence.rs` generates the inline twin
of each named program with a textual substitution helper and compares every
field above. `tests/tests/named_inline_runtime.rs` compares output multisets and
per-consumer order at run time. A case blocked by an open issue is `#[ignore]`
with the issue number and must be un-ignored by the fix.

## Open issues

- #445: named streams require `Copy` items and a `Send` source.

## Analyzer semantics for redeclared names

`marigold_analyze` does not run the checker, so it still analyzes a program that
declares a name twice. Code generation emits structs and enums, then functions,
then every variable declaration in source order (`let mut x = ...`, so a later
declaration shadows an earlier one), then all streams. The analyzer follows the
same order and the tests in `shadowing_semantics` pin it:

- A variable declared from another variable reads the most recent preceding
  declaration of that name, including its own previous binding (`x = x.map(f)`).
- A stream consumer sees every variable declaration, so it uses the last
  declaration of its variable wherever that sits in the program, including a
  declaration that appears after the consumer.
- A variable declared from a name that is declared only later is an undeclared
  source with unknown figures, and the checker warns that the variable is read
  before it is declared.

These rules apply only to programs analyzed directly. `marigold check` and code
generation reject such a program (#444).
