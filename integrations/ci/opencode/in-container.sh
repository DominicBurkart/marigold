#!/usr/bin/env bash
set -euo pipefail

SCENARIO=${1:?usage: in-container.sh error|info|warning|mixed|valid}
CI_DIR=/repo/integrations/ci
WORK=/work
PROJECT=$WORK/project
LOG=$WORK/requests-$SCENARIO.jsonl

. "$CI_DIR/lib/in-container.sh"

mkdir -p "$PROJECT" "$HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME" "$XDG_STATE_HOME"
trap stop_mock EXIT
start_mock openai-chat "$CI_DIR/scripts/opencode-$SCENARIO.json" "$LOG"

cp /repo/integrations/opencode/AGENTS.md "$PROJECT/AGENTS.md"
node -e '
const fs = require("fs");
const shipped = JSON.parse(fs.readFileSync("/repo/integrations/opencode/opencode.json", "utf8"));
const cfg = {
  $schema: shipped.$schema,
  lsp: shipped.lsp,
  autoupdate: false,
  share: "disabled",
  model: "mock/mock-model",
  permission: { edit: "allow", bash: "deny", webfetch: "deny" },
  provider: {
    mock: {
      npm: "@ai-sdk/openai-compatible",
      name: "Mock",
      options: { baseURL: "http://127.0.0.1:" + process.argv[2] + "/v1", apiKey: "sk-dummy-offline-ci" },
      models: { "mock-model": { name: "Mock Model", tool_call: true, limit: { context: 128000, output: 8192 } } },
    },
  },
};
fs.writeFileSync(process.argv[1], JSON.stringify(cfg, null, 2) + "\n");
' "$PROJECT/opencode.json" "$MOCK_PORT"

cd "$PROJECT"
git init -q .
set +e
timeout "${OPENCODE_TIMEOUT:-240}" env \
  OPENCODE_DISABLE_AUTOUPDATE=1 \
  OPENCODE_DISABLE_MODELS_FETCH=1 \
  OPENCODE_DISABLE_LSP_DOWNLOAD=1 \
  opencode run --print-logs --log-level DEBUG --model mock/mock-model \
    "Create hello_world.marigold, then check it." \
    >"$WORK/opencode-$SCENARIO.out" 2>"$WORK/opencode-$SCENARIO.err"
oc_rc=$?
set -e

echo "opencode exit code: $oc_rc"
echo "model requests: $(wc -l <"$LOG") (index:turn $(node -e '
const l=require("fs").readFileSync(process.argv[1],"utf8").split("\n").filter(Boolean).map(JSON.parse);
console.log(l.map(e=>e.index+":"+(e.turn??"aux")).join(" "))' "$LOG"))"

assert() { node "$CI_DIR/assert/assert-diagnostic.mjs" "$LOG" "$@"; }

ERROR_MSG="is not a declared enum"
INFO_MSG="undefined_fn"
WARNING_MSG="is read before it is declared"
SCOPE="--scope tool-results --after 1"
rc=0
case "$SCENARIO" in
  error) assert $SCOPE --contains "$ERROR_MSG" || rc=$? ;;
  info) assert $SCOPE --contains "Wrote file successfully" --absent "$INFO_MSG" || rc=$? ;;
  warning) assert $SCOPE --contains "Wrote file successfully" --absent "$WARNING_MSG" || rc=$? ;;
  mixed)
    assert $SCOPE --contains "$ERROR_MSG" --absent "$INFO_MSG" || rc=$?
    assert $SCOPE --contains "$ERROR_MSG" --absent "$WARNING_MSG" || rc=$?
    ;;
  valid) assert $SCOPE --contains "Wrote file successfully" --absent "$ERROR_MSG" || rc=$? ;;
  *) echo "unknown scenario $SCENARIO" >&2; rc=2 ;;
esac
if [ "$rc" -ne 0 ]; then
  echo "--- opencode stdout ---"; head -c 3000 "$WORK/opencode-$SCENARIO.out"
  echo "--- opencode stderr ---"; tail -c 4000 "$WORK/opencode-$SCENARIO.err"
fi
exit "$rc"
