#!/usr/bin/env bash
set -uo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
fixtures=$root/scripts/fixtures/nanna_agents
check=$root/scripts/check_nanna_agents.sh
failures=0

expect() {
  local want=$1 label=$2
  shift 2
  "$@" >/dev/null 2>&1
  local got=$?
  if { [ "$want" = zero ] && [ "$got" -eq 0 ]; } || { [ "$want" = nonzero ] && [ "$got" -ne 0 ]; }; then
    echo "ok: $label exits $want"
  else
    echo "FAIL: $label expected $want, got $got" >&2
    failures=$((failures + 1))
  fi
}

expect zero "narrowed card" env NANNA_REPO="$fixtures/ok" "$check"
expect nonzero "widened card" env NANNA_REPO="$fixtures/widened" "$check"
expect nonzero "name and file mismatch" env NANNA_REPO="$fixtures/mismatch" "$check"

host=$(mktemp -d)
trap 'rm -rf "$host"' EXIT
mkdir -p "$host/agents"
cp -r "$root/.nanna/catalog"/. "$host/agents/"
expect zero "matching host catalog" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"
echo "drift" >> "$host/agents/reviewer.toml"
expect nonzero "drifted host catalog" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"
exit "$failures"
