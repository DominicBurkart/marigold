#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

nanna_bin=${NANNA_BIN:-nanna}
command -v "$nanna_bin" >/dev/null 2>&1 || {
  echo "check_nanna_agents: nanna binary not found (set NANNA_BIN)" >&2
  exit 2
}

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/agents"
cp -r .nanna/catalog/. "$scratch/agents/"

listing=$(NANNA_CONFIG_DIR="$scratch" "$nanna_bin" agents list --repo "$PWD")
echo "$listing"

status=0
for file in .nanna/agents/*.toml; do
  name=$(sed -n 's/^name = "\(.*\)"$/\1/p' "$file" | head -1)
  if ! printf '%s\n' "$listing" | awk -v n="$name" '$1 == n { found = 1 } END { exit !found }'; then
    echo "check_nanna_agents: $name from $file missing from the listing" >&2
    status=1
  fi
done

exec_tools='cargo_|run_command|github_|git_commit|git_push|write_file|kani|lean|aeneas'
for file in .nanna/catalog/marigold-review-*.toml .nanna/agents/marigold-review-*.toml .nanna/catalog/marigold-research-*.toml .nanna/agents/marigold-research-*.toml; do
  [ -e "$file" ] || continue
  tools=$(sed -n 's/^tools = //p' "$file")
  if printf '%s\n' "$tools" | grep -Eq "$exec_tools"; then
    echo "check_nanna_agents: $file lists an execution, write or forge tool" >&2
    status=1
  fi
  grep -q '^max_effect = "none"$' "$file" || { echo "check_nanna_agents: $file ceiling is not none" >&2; status=1; }
  grep -q '^paths = \[\]$' "$file" || { echo "check_nanna_agents: $file has write paths" >&2; status=1; }
done

for file in .nanna/catalog/marigold-*.toml .nanna/agents/marigold-*.toml; do
  [ -e "$file" ] || continue
  name=$(sed -n 's/^name = "\(.*\)"$/\1/p' "$file" | head -1)
  [ "$name" = "$(basename "$file" .toml)" ] || { echo "check_nanna_agents: $file name mismatch" >&2; status=1; }
  if sed -n 's/^tools = //p' "$file" | grep -Eq 'github_|git_commit|git_push|run_command'; then
    echo "check_nanna_agents: $file lists a network, forge or shell tool" >&2
    status=1
  fi
  if sed -n 's/^paths = //p' "$file" | grep -Eq '\.nanna|inchworm|Cargo\.|\.github'; then
    echo "check_nanna_agents: $file may write configuration or manifests" >&2
    status=1
  fi
done
exit "$status"
