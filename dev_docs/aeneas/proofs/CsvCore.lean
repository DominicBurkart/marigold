/-!
# CSV Row Codec: Round-Trip Theorem (MVP stub)

This file states the round-trip identity for the CSV row codec defined
in `marigold-impl/src/csv_core.rs`. When Aeneas extraction is wired up,
this module will `import` the generated definitions and state the
theorem against them directly. For the MVP stub we define a Lean-side
model that mirrors the Rust code and state the theorem there.

Once the CI job runs `charon cargo --preset=aeneas` and
`aeneas -backend lean ... -o generated/`, replace the definitions below
with `import Generated.MarigoldImpl.CsvCore` (or whatever module the
extraction produces).

The theorem is currently `sorry`-stubbed. The empirical round-trip
property is enforced by the proptest in `marigold-impl/src/csv_core.rs`.
See `dev_docs/aeneas/invariants.md` for the formal statement in English.
-/

namespace CsvCore

/-- A CSV row: an ordered list of field byte strings. -/
structure Row where
  fields : List (List UInt8)
  deriving Repr, DecidableEq

def FIELD_SEP : UInt8 := 44  -- b','
def ROW_TERM  : UInt8 := 10  -- b'\n'

/-- Join a list of field byte strings with the field separator. -/
def joinFields : List (List UInt8) → List UInt8
  | []        => []
  | [f]       => f
  | f :: rest => f ++ [FIELD_SEP] ++ joinFields rest

/-- Encode a row using the RFC-4180 subset: fields joined by commas,
    terminated by LF. -/
def encodeRow (r : Row) : List UInt8 :=
  joinFields r.fields ++ [ROW_TERM]

/-- Decode a row. Returns `none` if the input is not well-formed
    (empty, missing terminator, or contains an LF before the terminator).
    MVP stub; Aeneas-generated definition will replace this. -/
def decodeRow (_bytes : List UInt8) : Option Row :=
  none

/-- **Round-trip identity (MVP).** For every `Row r`, decoding the
    encoded form recovers `r` exactly. Currently stubbed with `sorry`.
    The proptest in `marigold-impl/src/csv_core.rs::tests::roundtrip`
    provides empirical confidence until this proof is discharged.

    TODO: discharge once Aeneas-generated definitions are available. -/
theorem roundtrip (r : Row) : decodeRow (encodeRow r) = some r := by
  sorry

/-- **Length predictability.** The encoded length is
    `(sum of field lengths) + (n - 1) separators + 1 terminator`, for a
    row with `n ≥ 1` fields. MVP stub. -/
theorem encoded_length (r : Row) (h : r.fields.length ≥ 1) :
    (encodeRow r).length =
      (r.fields.foldl (fun acc f => acc + f.length) 0) + (r.fields.length - 1) + 1 := by
  sorry

end CsvCore
