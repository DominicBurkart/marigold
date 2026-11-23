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
exit "$status"
