#!/usr/bin/env bash
set -euo pipefail

SCENARIO=${1:?usage: in-container.sh broken|valid}
CI_DIR=/repo/integrations/ci
WORK=/work
PROJECT=$WORK/project
LOG=$WORK/requests-$SCENARIO.jsonl

. "$CI_DIR/lib/in-container.sh"

mkdir -p "$PROJECT" "$HOME"
trap stop_mock EXIT
MOCK_TURN_DELAY_MS=${MOCK_TURN_DELAY_MS:-3000}
start_mock anthropic "$CI_DIR/scripts/$SCENARIO.json" "$LOG"

cd "$PROJECT"
set +e
timeout "${CLAUDE_TIMEOUT:-180}" env \
  ANTHROPIC_BASE_URL="http://127.0.0.1:$MOCK_PORT" \
  ANTHROPIC_API_KEY=sk-ant-api03-dummy-key-for-offline-ci \
  DISABLE_TELEMETRY=1 \
  DISABLE_ERROR_REPORTING=1 \
  DISABLE_AUTOUPDATER=1 \
  CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1 \
  claude -p "Create hello_world.marigold, then check it." \
    --plugin-dir /repo/integrations/claude \
    --allowedTools "Edit,Write,Read" \
    --output-format text \
    >"$WORK/claude-$SCENARIO.out" 2>"$WORK/claude-$SCENARIO.err"
claude_rc=$?
set -e

echo "claude exit code: $claude_rc"
echo "model requests: $(wc -l <"$LOG") (index:turn $(node -e '
const l=require("fs").readFileSync(process.argv[1],"utf8").split("\n").filter(Boolean).map(JSON.parse);
console.log(l.map(e=>e.index+":"+(e.turn??"aux")).join(" "))' "$LOG"))"

rc=0
run_assertions "$LOG" "$SCENARIO" || rc=$?
if [ "$rc" -ne 0 ]; then
  echo "--- claude stdout ---"; head -c 2000 "$WORK/claude-$SCENARIO.out"
  echo "--- claude stderr ---"; head -c 2000 "$WORK/claude-$SCENARIO.err"
fi
exit "$rc"
