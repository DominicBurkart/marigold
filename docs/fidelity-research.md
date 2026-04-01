# Technical Gaps for Proving Fidelity of `marigold analyze`

## What `marigold analyze` Claims

`marigold analyze` takes a marigold program and produces a `ProgramComplexity` JSON object claiming:

- **Per-stream**: description, cardinality (exact/bounded/unknown), time complexity (class + exact), space complexity (class + exact), whether the stream collects input
- **Program-level**: dominant time, space, exact variants, and cardinality across all streams

The analyzer pipeline is: Pest PEG parse → AST build → complexity analysis. The complexity pass walks the AST, propagates cardinality through stream functions, and accumulates time/space work per step.

## Current Verification State

The existing test suite provides three layers:

1. **Property-based tests** (`proptest_analyze.rs`, 512 cases): verify internal consistency invariants (e.g., `time_class == exact_time.simplified()`, program dominates streams, streaming-only implies O(1) space).
2. **E2E tests** (`e2e_complexity.rs`, `e2e_cardinality.rs`): hand-written programs with expected outputs.
3. **Bisect test** (`bisect_complexity.rs`): demonstrates regression detection.

These are solid engineering practices. The gaps below are about what they *cannot* catch.

---

## Gap 1: No Ground-Truth Oracle for Complexity Claims

**The problem.** The property tests verify that the analyzer is *self-consistent* (e.g., the simplified class matches the exact complexity). They do not verify that the claims are *correct* — that a program declared O(n) actually runs in O(n) time.

**Why it matters.** A systematic error in `step_work_class` or `propagate_cardinality` would produce self-consistent but wrong results. For example, if `Combinations(k)` were misclassified as `OPolynomial(k)` instead of `OCombinatorial(k)`, every property test would still pass because exact and simplified would agree on the same wrong answer.

**What would close it.**

- **Empirical oracle testing.** For programs with known-cardinality inputs (ranges), actually execute the compiled program, count emitted items, and measure wall-clock scaling. Compare observed cardinality against predicted. Fit observed time to complexity curves and assert the predicted class is plausible. This is the single highest-value addition — it connects analysis claims to runtime reality.
- **Reference implementation.** Write a naive interpreter for the marigold AST that counts operations per item. Use it as an independent oracle: analyze a program with both the complexity pass and the counting interpreter, then assert agreement.

---

## Gap 2: Opaque User Functions

**The problem.** `fn` declarations contain arbitrary Rust bodies. The analyzer treats every user function as O(1) work per invocation. A user function containing a loop, allocation, or recursion violates this assumption silently.

**Why it matters.** The analyzer's time/space claims become lower bounds rather than accurate descriptions when user functions do non-trivial work. The output doesn't surface this assumption.

**What would close it.**

- **Declare the assumption explicitly** in the output JSON (e.g., `"assumes_o1_user_fns": true`).
- **Lightweight Rust body analysis.** Since fn bodies are embedded Rust, use `syn` to parse them and flag loops, recursion, `.collect()`, `Vec::new()`, and allocation patterns. Report a warning or escalate the function's complexity contribution.
- **User annotations.** Allow `#[complexity(O(n))]` attributes on fn declarations so the analyzer can incorporate user-declared costs.

---

## Gap 3: Filter Cardinality is Unbounded

**The problem.** `filter` and `filter_map` wrap cardinality in `Symbolic::Filtered(inner)`, meaning "at most `inner`." The analyzer has no way to tighten this bound because filter predicates are opaque Rust closures.

**Why it matters.** A pipeline `range(0, 1000000).filter(f).permutations(3)` could produce anywhere from 0 to ~10^18 items. The analyzer reports `Bounded(Filtered(...))`, which is technically correct but not useful for predicting runtime.

**What would close it.**

- **Selectivity annotations.** Allow `filter(f, selectivity=0.01)` so the user can declare expected pass-through rate. The analyzer then produces tighter cardinality bounds.
- **Domain-restricted filters.** For bounded types (`int[0, 255]`), the type system already knows the domain size. If a filter is on a bounded field, the cardinality upper bound is the domain cardinality — tighter than the input cardinality.

---

## Gap 4: No Formal Semantics Specification

**The problem.** The meaning of each stream function is defined implicitly by its code-generation output (`nodes.rs` codegen methods) and its complexity model (`complexity.rs`). There is no independent specification document or formal semantics from which both are derived.

**Why it matters.** Without a shared source of truth, the code generator and the analyzer can disagree. The codegen might implement `keep_first_n` one way while the complexity pass models it differently. Bugs in either direction are invisible until a user notices wrong output or wrong analysis.

**What would close it.**

- **Denotational semantics document.** For each `StreamFunctionKind`, define:
  - the mathematical function on multisets/sequences it computes
  - its cardinality transformation rule
  - its time and space cost model
  This becomes the oracle both codegen and analysis are tested against.
- **Executable specification in a proof assistant.** Encode the semantics in Lean 4 or Coq. Prove that the complexity analysis rules are sound with respect to the denotational semantics. This is heavy but would provide the strongest guarantee.
- **Lightweight alternative: declarative rule table.** Define a single `StreamFunctionSemantics` struct with fields for cardinality_rule, time_rule, space_rule, and codegen_template. Both the analyzer and the code generator derive behavior from this shared table, making disagreement structurally impossible.

---

## Gap 5: Property Test Generator Coverage

**The problem.** The proptest generators (`proptest_analyze.rs`) cover a useful but limited space:
- Inputs are only `range` (never `read_file_csv` or `select_all` with >2 streams)
- Stream chains are 0–4 functions long
- No nested stream variable references (e.g., `v2 = v1.map(f)` chains are generated, but deeper DAGs are not)
- No struct/enum declarations or bounded types appear in generated programs
- User fn declarations are never generated

**Why it matters.** Bugs tend to hide in feature interactions. The property tests don't exercise `select_all` cardinality summation, bounded-type-aware analysis, or programs mixing type declarations with stream pipelines.

**What would close it.**

- **Expand generators** to include `read_file_csv` inputs (unknown cardinality paths), `select_all` with varying arity, struct/enum declarations before streams, and fn declarations referenced in map/filter closures.
- **Grammar-based fuzzing.** Use the Pest grammar itself to drive a fuzzer (e.g., via `cargo-fuzz` or `afl`). This generates syntactically valid programs that the hand-written generators might never produce, catching panics and assertion failures in the analyzer.
- **Differential fuzzing.** Feed the same generated program to both `marigold analyze` and a reference interpreter (see Gap 1), then compare outputs.

---

## Gap 6: No Cross-Validation Between Codegen and Analysis

**The problem.** The codebase has two consumers of the AST: the code generator (producing Rust/Tokio stream code) and the analyzer (producing complexity claims). They are tested independently. Nothing verifies that the generated code actually exhibits the predicted complexity.

**Why it matters.** The code generator is the ground truth of what the program *does*. The analyzer is a model of what it *should do*. If these drift apart, the analyzer lies.

**What would close it.**

- **End-to-end property: analyze then run.** For generated test programs with constant-range inputs:
  1. Run `marigold analyze` → get predicted cardinality.
  2. Compile and run the program → count actual output items.
  3. Assert predicted cardinality matches actual count (for `Exact` cardinality) or actual count falls within bounds (for `Bounded`).
- **Complexity scaling test.** Run the same program at input sizes n=100, n=1000, n=10000. Measure time. Assert the growth rate is consistent with the predicted class (linear, quadratic, etc.) using a simple curve-fitting check.

---

## Gap 7: `ExactComplexity` Arithmetic is Ad Hoc

**The problem.** `ExactComplexity` is a `BTreeMap<ComplexityClass, u64>` where the key is the complexity class and the value is a coefficient. The `simplified()` method picks the dominant term. The `add_work` method increments coefficients. There are no algebraic laws verified — no tests that addition is associative, that simplification is idempotent, or that the ordering on `ComplexityClass` is a total order consistent with asymptotic dominance.

**Why it matters.** A subtle ordering bug (e.g., `OCombinatorial(3) < OPolynomial(4)` when it should be `>`) would cause wrong simplification for every program using both operations.

**What would close it.**

- **Property tests on the algebra:**
  - `simplified(simplified(x)) == simplified(x)` (idempotence)
  - `a + b == b + a` (commutativity)
  - For all class pairs, `a >= b` iff `a` asymptotically dominates `b`
  - `ExactComplexity::new().simplified() == O1`
- **Derive `Ord` from a canonical ordering** rather than hand-implementing `PartialOrd`. The current `PartialOrd` impl on `ComplexityClass` should be tested against known asymptotic relationships at specific crossover points.

---

## Gap 8: No Totality Check on the AST Walk

**The problem.** `analyze_program()` pattern-matches on `TypedExpression` variants. If a new variant is added to the AST (e.g., a new language feature), the analyzer silently ignores it unless the match arm is exhaustive. Rust's exhaustiveness checking helps here, but only if the match is on the enum directly — `if let` chains or filtered iterations can miss variants.

**Why it matters.** A Dev agent adding a new AST node might not realize the analyzer also needs updating. The program would compile, tests would pass (because no existing test uses the new node), and the analyzer would produce incomplete results.

**What would close it.**

- **Ensure the main analysis match is exhaustive** (it appears to be, since it matches on `TypedExpression` directly — verify this remains true as the language evolves).
- **Add a compile-time test** that the number of `TypedExpression` variants equals an expected constant. When a new variant is added, the test fails, forcing the developer to update the analyzer.
- **Lint rule or CI check** that every `TypedExpression` variant appears in at least one analyzer test case.

---

## Approaches for Dev Agent Oracles

The above gaps suggest three categories of tooling that would help Dev agents working on marigold:

### Static Analysis Tooling

| Tool | Purpose | Closes Gap |
|------|---------|------------|
| `syn`-based fn body scanner | Flag non-O(1) patterns in user functions | 2 |
| AST variant coverage checker | Ensure analyzer handles all node types | 8 |
| Complexity algebra property tests | Verify `ExactComplexity` / `ComplexityClass` laws | 7 |

### Formal Methods

| Approach | Purpose | Closes Gap |
|----------|---------|------------|
| Denotational semantics doc | Single source of truth for stream function behavior | 4 |
| Lean 4 / Coq encoding of cardinality rules | Machine-checked soundness of propagation rules | 1, 4 |
| Declarative rule table (shared codegen + analysis source) | Structural impossibility of codegen/analysis disagreement | 4, 6 |

### Declarative / Oracle-Based Testing

| Approach | Purpose | Closes Gap |
|----------|---------|------------|
| Empirical cardinality oracle (run program, count items) | Ground-truth validation of cardinality claims | 1, 6 |
| Complexity scaling oracle (run at multiple n, fit curve) | Ground-truth validation of time complexity claims | 1, 6 |
| Grammar-based fuzzing (from `.pest`) | Discover panics and edge cases | 5 |
| Differential fuzzing (analyzer vs. reference interpreter) | Find semantic disagreements | 1, 5, 6 |
| Expanded proptest generators | Cover `read_file_csv`, `select_all`, bounded types | 5 |

### Priority Ranking for Dev Agent Utility

1. **Empirical cardinality oracle** — highest value, moderate effort. Directly validates correctness for exact-cardinality programs. A Dev agent can run this after any change to the analyzer and immediately see if cardinality claims broke.
2. **Declarative rule table** — high value, moderate effort. Eliminates an entire class of bugs (codegen/analysis disagreement) by construction. A Dev agent modifying a stream function only needs to update one place.
3. **Expanded proptest generators** — high value, low effort. The existing proptest infrastructure is excellent; widening coverage is incremental work.
4. **Complexity algebra property tests** — moderate value, low effort. Small addition to existing test suite.
5. **Grammar-based fuzzing** — moderate value, moderate effort. Catches edge cases that structured generators miss.
6. **Formal semantics** — highest correctness guarantee, highest effort. Worth pursuing for the core cardinality propagation rules first, leaving codegen formalization for later.
