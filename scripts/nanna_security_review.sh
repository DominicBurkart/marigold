#!/bin/bash
set -uo pipefail

NANNA_BIN="${NANNA_BIN:-nanna}"
MODEL="${NANNA_REVIEW_MODEL:-gemma4:e4b}"
CHUNK_BYTES="${NANNA_REVIEW_CHUNK_BYTES:-6000}"
MAX_CHUNKS="${NANNA_REVIEW_MAX_CHUNKS:-10}"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/nanna_security_review.XXXXXX")" || {
  echo "nanna security review: cannot create temp dir" >&2
  exit 1
}
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/diff"

if ! grep -q '[^[:space:]]' "$WORK/diff"; then
  echo "nanna security review: empty diff, nothing to review"
  exit 0
fi

SECRET_PATTERNS=(
  '-----BEGIN [A-Z ]*PRIVATE KEY-----'
  'AKIA[0-9A-Z]{16}'
  'ASIA[0-9A-Z]{16}'
  'gh[pousr]_[A-Za-z0-9]{36,}'
  'github_pat_[A-Za-z0-9_]{22,}'
  'xox[abprs]-[A-Za-z0-9-]{10,}'
  'sk-[A-Za-z0-9_-]{20,}'
  'AIza[0-9A-Za-z_-]{35}'
  'glpat-[A-Za-z0-9_-]{20,}'
  'npm_[A-Za-z0-9]{36}'
  'eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}'
)

grep -E '^\+' "$WORK/diff" | grep -v '^+++' > "$WORK/added" || true
for pattern in "${SECRET_PATTERNS[@]}"; do
  if grep -Eq -e "$pattern" "$WORK/added"; then
    echo "nanna security review: pre-scan matched a secret pattern in added lines (pattern: $pattern). Remove the secret before committing."
    exit 1
  fi
done

if ! command -v "$NANNA_BIN" > /dev/null 2>&1; then
  echo "nanna security review: nanna binary not found (NANNA_BIN=$NANNA_BIN). Install nanna or set NANNA_BIN; blocking the commit."
  exit 1
fi

mkdir "$WORK/chunks"
split -C "$CHUNK_BYTES" -d -a 4 "$WORK/diff" "$WORK/chunks/c"
TOTAL=$(find "$WORK/chunks" -type f | wc -l)
if (( TOTAL > MAX_CHUNKS )); then
  echo "nanna security review: diff too large to review: needs $TOTAL chunks of $CHUNK_BYTES bytes, limit is $MAX_CHUNKS chunks. Split the commit into smaller commits."
  exit 1
fi

PROMPT_HEAD="You are a security engineer. Review the code being committed to determine if it can be committed/pushed. Does this commit leak any secrets, tokens, sensitive internals, or PII? Your response MUST start with exactly 'VERDICT: SAFE' or 'VERDICT: UNSAFE'. If UNSAFE, follow with a list of at most 5 short bullets, one line each, naming the problems to fix before the commit can be completed."

STATUS=0
INDEX=0
for chunk in "$WORK"/chunks/c*; do
  INDEX=$((INDEX + 1))
  OUT="$WORK/out.$INDEX"
  ERR="$WORK/err.$INDEX"
  PROMPT="$PROMPT_HEAD

Diff part $INDEX of $TOTAL:

$(cat "$chunk")"
  if ! "$NANNA_BIN" chat -m "$MODEL" --temperature 0 -p "$PROMPT" > "$OUT" 2> "$ERR" < /dev/null; then
    echo "nanna security review: nanna chat failed for model $MODEL (is ollama running and the model pulled?). Blocking the commit."
    cat "$ERR" >&2
    exit 1
  fi
  sed -e 's/^Assistant: //' "$OUT"
  if grep -Eq '^[[:space:]]*VERDICT: UNSAFE' "$OUT"; then
    echo "nanna security review: part $INDEX/$TOTAL reported VERDICT: UNSAFE."
    STATUS=1
  elif grep -Eq '^[[:space:]]*(Assistant: )?VERDICT: SAFE' "$OUT"; then
    :
  else
    echo "nanna security review: no VERDICT line in model output for part $INDEX/$TOTAL; failing closed."
    STATUS=1
  fi
  if grep -Eq 'VERDICT: SAFE' "$OUT" && grep -Eq 'VERDICT: UNSAFE' "$OUT"; then
    echo "nanna security review: contradictory verdicts for part $INDEX/$TOTAL; failing closed."
    STATUS=1
  fi
done

if (( STATUS != 0 )); then
  echo "Security issues found above. Please fix before committing."
fi
exit "$STATUS"
