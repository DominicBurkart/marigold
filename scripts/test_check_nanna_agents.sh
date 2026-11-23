#!/usr/bin/env bash
set -uo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
fixtures=$root/scripts/fixtures/nanna_agents
check=$root/scripts/check_nanna_agents.sh
failures=0

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

absent=$tmp/absent
host=$tmp/host
mkdir -p "$host/agents" "$tmp/empty-repo/.nanna/agents"
cp -r "$root/.nanna/catalog"/. "$host/agents/"

expect() {
  local want=$1 pattern=$2 label=$3
  shift 3
  local out got
  out=$("$@" 2>&1)
  got=$?
  if [ "$got" -ne "$want" ]; then
    echo "FAIL: $label expected exit $want, got $got" >&2
    failures=$((failures + 1))
  elif [ -n "$pattern" ] && ! printf '%s\n' "$out" | grep -q -- "$pattern"; then
    echo "FAIL: $label exit $got but output lacks '$pattern'" >&2
    failures=$((failures + 1))
  else
    echo "ok: $label exits $want"
  fi
}

expect 0 "" "narrowed card, no host catalog" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$fixtures/ok" "$check"
expect 1 "widens .scope.paths" "widened paths" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$fixtures/widened-paths" "$check"
expect 1 "widens .scope.max_effect" "widened max_effect" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$fixtures/widened-effect" "$check"
expect 1 "widens .scope.tools" "widened tools" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$fixtures/widened-tools" "$check"
expect 1 "differs from its file name" "name and file mismatch" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$fixtures/mismatch" "$check"
expect 1 "no cards in" "missing agents dir" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$tmp/empty-repo" "$check"
expect 0 "" "narrowed card against matching host catalog" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

sed -i 's/^max_iterations = .*/max_iterations = 300/' "$host/agents/reviewer.toml"
expect 0 "warning: .* differs" "drifted but compatible host catalog warns only" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

sed -i 's/^tools = .*/tools = ["list_directory"]/' "$host/agents/reviewer.toml"
expect 1 "host catalog .* rejects the repo cards" "repo catalog passes but host base is narrower" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

exit "$failures"
