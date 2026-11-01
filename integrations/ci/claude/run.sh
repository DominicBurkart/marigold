#!/usr/bin/env bash
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO=$(cd "$HERE/../../.." && pwd)
VERSION=${HARNESS_VERSION:-$(tr -d '[:space:]' <"$HERE/VERSION")}
CF_HASH=$(sha256sum "$HERE/Containerfile" | cut -c1-8)
IMAGE=localhost/marigold-ci-claude:$VERSION-$CF_HASH
RUN_ID=$$
FAILED=0
CONTAINERS=()
VOLUMES=()

: "${MARIGOLD_LSP_BIN:?set MARIGOLD_LSP_BIN to a built marigold-lsp binary}"
[ -x "$MARIGOLD_LSP_BIN" ] || { echo "not executable: $MARIGOLD_LSP_BIN" >&2; exit 2; }
MARIGOLD_LSP_BIN=$(readlink -f "$MARIGOLD_LSP_BIN")
command -v podman >/dev/null || { echo "podman is required" >&2; exit 2; }

cleanup() {
  [ "${KEEP:-0}" = 1 ] && {
    echo "KEEP=1: leaving containers [${CONTAINERS[*]:-}] and volumes [${VOLUMES[*]:-}]"
    return
  }
  local c v
  for c in "${CONTAINERS[@]:-}"; do [ -n "$c" ] && podman rm -f "$c" >/dev/null 2>&1; done
  for v in "${VOLUMES[@]:-}"; do [ -n "$v" ] && podman volume rm -f "$v" >/dev/null 2>&1; done
  return 0
}
trap cleanup EXIT
trap 'exit 130' INT TERM

if ! podman image exists "$IMAGE"; then
  echo "building $IMAGE"
  podman build -q -t "$IMAGE" --build-arg "CLAUDE_CODE_VERSION=$VERSION" \
    -f "$HERE/Containerfile" "$HERE" >/dev/null || { echo "image build failed" >&2; exit 2; }
fi

declare -A RESULT
run_scenario() {
  local scenario=$1 name=marigold-ci-claude-$RUN_ID-$1 work
  CONTAINERS+=("$name")
  if [ "${KEEP:-0}" = 1 ]; then
    VOLUMES+=("$name")
    work=(-v "$name:/work")
  else
    work=(--tmpfs /work:rw,exec,mode=0755)
  fi
  echo "=== scenario: $scenario ==="
  podman run --name "$name" --security-opt label=disable \
    -v "$REPO:/repo:ro" \
    -v "$MARIGOLD_LSP_BIN:/usr/local/bin/marigold-lsp:ro" \
    "${work[@]}" \
    "$IMAGE" bash /repo/integrations/ci/claude/in-container.sh "$scenario"
  local rc=$?
  if [ "$rc" -eq 0 ]; then RESULT[$scenario]=PASS; else RESULT[$scenario]=FAIL; FAILED=1; fi
}

run_scenario broken
run_scenario valid

echo
echo "=== summary (Claude Code $VERSION) ==="
echo "${RESULT[broken]}  broken file: later request contains undefined-fn"
echo "${RESULT[valid]}  valid file (control): undefined-fn absent"
if [ "$FAILED" -eq 0 ]; then echo "PASS"; else echo "FAIL"; fi
exit "$FAILED"
