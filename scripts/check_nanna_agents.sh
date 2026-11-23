#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
repo=${NANNA_REPO:-$root}
catalog=${NANNA_CATALOG_DIR:-$root/.nanna/catalog}
host_config=${NANNA_CONFIG_DIR:-}

nanna_bin=${NANNA_BIN:-nanna}
command -v "$nanna_bin" >/dev/null 2>&1 || {
  echo "check_nanna_agents: nanna binary not found (set NANNA_BIN)" >&2
  exit 2
}
command -v python3 >/dev/null 2>&1 || {
  echo "check_nanna_agents: python3 not found" >&2
  exit 2
}

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/agents"
cp -r "$catalog"/. "$scratch/agents/"

status=0

if [ -n "$host_config" ] && [ -d "$host_config/agents" ]; then
  if ! diff -r "$catalog" "$host_config/agents" >/dev/null; then
    echo "check_nanna_agents: $catalog differs from $host_config/agents" >&2
    diff -r "$catalog" "$host_config/agents" >&2 || true
    status=1
  fi
fi

listing=$(NANNA_CONFIG_DIR="$scratch" "$nanna_bin" agents list --repo "$repo") || status=1
echo "$listing"

for file in "$repo"/.nanna/agents/*.toml; do
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
    echo "check_nanna_agents: $file declares name $name" >&2
    status=1
  fi
  if ! printf '%s\n' "$listing" | awk -v n="$name" '$1 == n { found = 1 } END { exit !found }'; then
    echo "check_nanna_agents: $name from $file missing from the listing" >&2
    status=1
  fi
done
exit "$status"
