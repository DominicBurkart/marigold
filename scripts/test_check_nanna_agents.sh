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
mkdir -p "$host/agents" "$tmp/empty-repo/.nanna/agents" "$tmp/no-agents-repo"
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
expect 1 "no cards in" "empty agents dir" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$tmp/empty-repo" "$check"
expect 1 "no cards in" "absent agents dir" env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$tmp/no-agents-repo" "$check"
expect 0 "" "narrowed card against matching host catalog" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

sed -i 's/^max_iterations = .*/max_iterations = 300/' "$host/agents/reviewer.toml"
expect 0 "warning: .* differs" "drifted but compatible host catalog warns only" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

sed -i 's/^tools = .*/tools = ["list_directory"]/' "$host/agents/reviewer.toml"
expect 1 "host catalog .* rejects the repo cards" "repo catalog passes but host base is narrower" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"
expect 1 "widens .scope.tools" "host base is narrower, cause reported" env NANNA_CONFIG_DIR="$host" NANNA_REPO="$fixtures/ok" "$check"

mkdir -p "$tmp/xdg/nanna"
cp -r "$host/agents" "$tmp/xdg/nanna/agents"
expect 1 "xdg/nanna/agents rejects the repo cards" "empty NANNA_CONFIG_DIR falls back to XDG_CONFIG_HOME" env NANNA_CONFIG_DIR= XDG_CONFIG_HOME="$tmp/xdg" NANNA_REPO="$fixtures/ok" "$check"

mutate() {
  local file=$1 expr=$2
  rm -rf "$tmp/m"
  mkdir -p "$tmp/m/repo/.nanna"
  cp -r "$root/.nanna/agents" "$tmp/m/repo/.nanna/agents"
  cp -r "$root/.nanna/catalog" "$tmp/m/cat"
  sed -i "$expr" "$tmp/m/cat/$file" "$tmp/m/repo/.nanna/agents/$file"
}
mutated() {
  env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$tmp/m/repo" NANNA_CATALOG_DIR="$tmp/m/cat" "$check"
}
mutated_elsewhere() {
  bash -c 'cd / && exec "$@"' _ env NANNA_CONFIG_DIR="$absent" NANNA_REPO="$tmp/m/repo" NANNA_CATALOG_DIR="$tmp/m/cat" "$check"
}

rm -rf "$tmp/m"
mkdir -p "$tmp/m/repo/.nanna"
cp -r "$root/.nanna/agents" "$tmp/m/repo/.nanna/agents"
cp -r "$root/.nanna/catalog" "$tmp/m/cat"
expect 0 "" "unmodified family passes" mutated
expect 0 "" "unmodified family passes from another working directory" mutated_elsewhere

mutate marigold-research-grammar.toml 's/^tools = \[/tools = ["cargo_check", /'
expect 1 "execution or forge tool" "researcher with a cargo tool" mutated
expect 1 "execution or forge tool" "researcher with a cargo tool, other working directory" mutated_elsewhere

mutate marigold-research-grammar.toml 's|^paths = .*|paths = ["docs/research/other.txt"]|'
expect 1 "other than docs/research/grammar.txt" "researcher with a wrong write path" mutated
expect 1 "other than docs/research/grammar.txt" "researcher with a wrong write path, other working directory" mutated_elsewhere

mutate marigold-research-lead.toml 's|^paths = .*|paths = ["docs/**"]|'
expect 1 "other than docs/research/\*\*" "lead with a wrong glob" mutated
expect 1 "other than docs/research/\*\*" "lead with a wrong glob, other working directory" mutated_elsewhere

mutate marigold-review-scope.toml 's/^tools = \[/tools = ["write_file", /'
expect 1 "execution, write or forge tool" "reviewer with a write tool, other working directory" mutated_elsewhere

mutate marigold-research-grammar.toml 's/^max_effect = .*/max_effect = "none"/'
expect 1 "ceiling is not workspace" "researcher with ceiling none" mutated

exit "$failures"
