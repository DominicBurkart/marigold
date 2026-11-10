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
exit "$status"
