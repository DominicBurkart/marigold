#!/usr/bin/env bash
set -euo pipefail

CI_DIR=${CI_DIR:-/repo/integrations/ci}
WORK=${WORK:-/work}

start_mock() {
  local protocol=$1 script=$2 log=$3
  node "$CI_DIR/mock-model/mock-model.mjs" --protocol "$protocol" \
    --script "$script" --log "$log" --port 0 \
    --var "cwd=$PROJECT" --var "file=$PROJECT/hello_world.marigold" \
    >"$WORK/mock.out" 2>"$WORK/mock.err" &
  MOCK_PID=$!
  local i port=""
  for i in $(seq 1 100); do
    port=$(sed -n 's/^PORT=//p' "$WORK/mock.out" 2>/dev/null | head -n1)
    [ -n "$port" ] && break
    kill -0 "$MOCK_PID" 2>/dev/null || { cat "$WORK/mock.err" >&2; return 1; }
    sleep 0.1
  done
  [ -n "$port" ] || { echo "mock server did not report a port" >&2; return 1; }
  MOCK_PORT=$port
}

stop_mock() {
  [ -n "${MOCK_PID:-}" ] && kill "$MOCK_PID" 2>/dev/null || true
}

run_assertions() {
  local log=$1 scenario=$2
  case "$scenario" in
    broken)
      node "$CI_DIR/assert/assert-diagnostic.mjs" "$log" --after 1 \
        --contains undefined-fn
      ;;
    valid)
      node "$CI_DIR/assert/assert-diagnostic.mjs" "$log" --after 1 \
        --contains hello_world.marigold --absent undefined-fn
      ;;
    *)
      echo "unknown scenario $scenario" >&2
      return 2
      ;;
  esac
}
