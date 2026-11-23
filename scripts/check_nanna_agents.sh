#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
repo=${NANNA_REPO:-$root}
catalog=${NANNA_CATALOG_DIR:-$root/.nanna/catalog}

if [ -n "${NANNA_CONFIG_DIR:-}" ]; then
  host_config=$NANNA_CONFIG_DIR
elif [ -n "${XDG_CONFIG_HOME:-}" ]; then
  host_config=$XDG_CONFIG_HOME/nanna
else
  host_config=${HOME:-}/.config/nanna
fi

nanna_bin=${NANNA_BIN:-nanna}
command -v "$nanna_bin" >/dev/null 2>&1 || {
  echo "check_nanna_agents: nanna binary not found (set NANNA_BIN)" >&2
  exit 2
}
command -v python3 >/dev/null 2>&1 || {
  echo "check_nanna_agents: python3 not found" >&2
  exit 2
}

shopt -s nullglob
cards=("$repo"/.nanna/agents/*.toml)
shopt -u nullglob
if [ "${#cards[@]}" -eq 0 ]; then
  echo "check_nanna_agents: no cards in $repo/.nanna/agents" >&2
  exit 1
fi

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/agents"
cp -r "$catalog"/. "$scratch/agents/"

status=0

listing_failed=0
listing=$(NANNA_CONFIG_DIR="$scratch" "$nanna_bin" agents list --repo "$repo") || {
  status=1
  listing_failed=1
}
echo "$listing"

if [ -d "$host_config/agents" ]; then
  echo "check_nanna_agents: checking against host catalog $host_config/agents" >&2
  if ! NANNA_CONFIG_DIR="$host_config" "$nanna_bin" agents list --repo "$repo" >/dev/null; then
    echo "check_nanna_agents: host catalog $host_config/agents rejects the repo cards" >&2
    status=1
  fi
  if ! diff -r "$catalog" "$host_config/agents" >/dev/null 2>&1; then
    echo "check_nanna_agents: warning: $catalog differs from $host_config/agents" >&2
  fi
else
  echo "check_nanna_agents: no host catalog at $host_config/agents; only the repo catalog was checked" >&2
fi

for file in "${cards[@]}"; do
  name=$(python3 -I -c '
import sys, tomllib
with open(sys.argv[1], "rb") as f:
    print(tomllib.load(f)["identity"]["name"])
' "$file") || {
    echo "check_nanna_agents: $file has no readable [identity] name" >&2
    status=1
    continue
  }
  if [ "$name.toml" != "$(basename "$file")" ]; then
    echo "check_nanna_agents: $file declares name $name which differs from its file name" >&2
    status=1
  fi
  if [ "$listing_failed" -eq 0 ] && ! printf '%s\n' "$listing" | awk -v n="$name" '$1 == n { found = 1 } END { exit !found }'; then
    echo "check_nanna_agents: $name from $file missing from the listing" >&2
    status=1
  fi
done

exec_tools='cargo_|run_command|github_|git_commit|git_push|write_file|kani|lean|aeneas'
for file in .nanna/catalog/marigold-review-*.toml .nanna/agents/marigold-review-*.toml; do
  [ -e "$file" ] || continue
  tools=$(sed -n 's/^tools = //p' "$file")
  if printf '%s\n' "$tools" | grep -Eq "$exec_tools"; then
    echo "check_nanna_agents: $file lists an execution, write or forge tool" >&2
    status=1
  fi
  grep -q '^max_effect = "none"$' "$file" || { echo "check_nanna_agents: $file ceiling is not none" >&2; status=1; }
  grep -q '^paths = \[\]$' "$file" || { echo "check_nanna_agents: $file has write paths" >&2; status=1; }
done

research_exec_tools='cargo_|run_command|github_|git_commit|git_push|kani|lean|aeneas'
for file in .nanna/catalog/marigold-research-*.toml .nanna/agents/marigold-research-*.toml; do
  [ -e "$file" ] || continue
  tools=$(sed -n 's/^tools = //p' "$file")
  if printf '%s\n' "$tools" | grep -Eq "$research_exec_tools"; then
    echo "check_nanna_agents: $file lists an execution or forge tool" >&2
    status=1
  fi
  grep -q '^max_effect = "workspace"$' "$file" || { echo "check_nanna_agents: $file ceiling is not workspace" >&2; status=1; }
  name=$(basename "$file" .toml)
  if [ "$name" = "marigold-research-lead" ]; then
    want='docs/research/**'
  else
    want="docs/research/${name#marigold-research-}.txt"
  fi
  grep -qxF "paths = [\"$want\"]" "$file" || { echo "check_nanna_agents: $file may write something other than $want" >&2; status=1; }
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
