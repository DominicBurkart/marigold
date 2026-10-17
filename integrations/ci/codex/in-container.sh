#!/usr/bin/env bash
set -euo pipefail

SCENARIO=${1:?usage: in-container.sh broken|valid|error}
CI_DIR=/repo/integrations/ci
WORK=/work
PROJECT=$WORK/project
LOG=$WORK/requests-$SCENARIO.jsonl
ASSERT="node $CI_DIR/assert/assert-diagnostic.mjs"

. "$CI_DIR/lib/in-container.sh"

mkdir -p "$PROJECT" "$HOME" "$CODEX_HOME"
trap stop_mock EXIT
start_mock openai-responses "$CI_DIR/scripts/codex-$SCENARIO.json" "$LOG"

{
  cat <<CONF
model = "mock-model"
model_provider = "mock"
approval_policy = "never"
sandbox_mode = "danger-full-access"
check_for_update_on_startup = false

[analytics]
enabled = false

[feedback]
enabled = false

[model_providers.mock]
name = "Mock"
base_url = "http://127.0.0.1:$MOCK_PORT/v1"
env_key = "MOCK_API_KEY"
wire_api = "responses"
requires_openai_auth = false
request_max_retries = 0
stream_max_retries = 0

CONF
  cat /repo/integrations/codex/config.toml
} >"$CODEX_HOME/config.toml"

cp /repo/integrations/codex/AGENTS.md "$PROJECT/AGENTS.md"

export MOCK_API_KEY=dummy-key-for-offline-ci
cd "$PROJECT"

codex mcp list >"$WORK/mcp-list-$SCENARIO.txt" 2>&1
cat "$WORK/mcp-list-$SCENARIO.txt"

set +e
timeout "${CODEX_TIMEOUT:-180}" codex exec --skip-git-repo-check --json \
  "Create hello_world.marigold, then check it." \
  >"$WORK/codex-$SCENARIO.out" 2>"$WORK/codex-$SCENARIO.err" </dev/null
codex_rc=$?
set -e

echo "codex exit code: $codex_rc"
echo "model requests: $(wc -l <"$LOG") (index:turn $(node -e '
const l=require("fs").readFileSync(process.argv[1],"utf8").split("\n").filter(Boolean).map(JSON.parse);
console.log(l.map(e=>e.index+":"+(e.turn??"aux")).join(" "))' "$LOG"))"

rc=0
step() { "$@" || rc=1; }

step grep -Eq '^marigold +marigold-lsp +mcp .*enabled' "$WORK/mcp-list-$SCENARIO.txt"
step $ASSERT "$LOG" --after 0 --contains "# AGENTS.md instructions for $PROJECT"
step $ASSERT "$LOG" --after 0 --contains "MCP server on that file before you finish"
step $ASSERT "$LOG" --after 0 --contains "mcp__marigold"

case "$SCENARIO" in
  broken)
    step $ASSERT "$LOG" --after 2 --outputs-only --contains '"code":"undefined-fn"'
    step $ASSERT "$LOG" --after 2 --outputs-only --contains '"severity":"information"'
    ;;
  valid)
    step $ASSERT "$LOG" --after 2 --outputs-only --contains '"ok":true' \
      --absent undefined-fn
    ;;
  error)
    step $ASSERT "$LOG" --after 2 --outputs-only --contains '"code":"undefined-enum"'
    step $ASSERT "$LOG" --after 2 --outputs-only --contains '"severity":"error"'
    ;;
  *)
    echo "unknown scenario $SCENARIO" >&2
    rc=2
    ;;
esac

if [ "$rc" -ne 0 ]; then
  echo "--- codex stdout ---"; head -c 2000 "$WORK/codex-$SCENARIO.out"
  echo "--- codex stderr ---"; head -c 2000 "$WORK/codex-$SCENARIO.err"
fi
exit "$rc"
