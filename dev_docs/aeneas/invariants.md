# Aeneas invariants

This file states, in English, the formal claims the Lean proofs in
[`proofs/CsvCore.lean`](proofs/CsvCore.lean) encode. Any change to
`marigold-impl/src/csv_core.rs` that affects these claims must also update
this file.

## MVP claim: roundtrip

For all `r: Row`:

```text
decode_row(encode_row(&r)) == Some(r)
```

In English: Marigold's CSV row encoder is a left-inverse of its decoder.
Encoding a row and then decoding the result recovers the original row
bit-for-bit, with no field reordering, truncation, or escape loss.

## Supporting claim: length lower bound

`encode_row` produces a byte string whose length is at least:

```text
max(1, sum(field_lens) + (n_fields - 1))
```

That is, the encoded length is predictable from the input: one byte per
field content plus one separator byte between each pair of adjacent
fields, with a floor of one byte for the empty-row case. Any future claim
that depends on output size can rely on this bound.

## Status

As of this commit, the Lean proof of the MVP claim is stubbed with
`sorry`. The claim is enforced empirically by the proptest in
`marigold-impl/src/csv_core.rs`, which exercises the roundtrip on
randomly generated rows.

An agent reading this file must not assume the theorem holds until the
`sorry` is discharged. Discharging it is tracked as a separate follow-up
issue. Until then, treat the invariants as high-confidence but not yet
machine-checked.
