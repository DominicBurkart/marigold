#!/usr/bin/env bash
set -euo pipefail

SCENARIO=${1:?usage: in-container.sh error|info|warning|mixed|valid|edit-error|edit-info|mcp-info|mcp-warning|mcp-valid}
CI_DIR=/repo/integrations/ci
WORK=/work
PROJECT=$WORK/project
LOG=$WORK/requests-$SCENARIO.jsonl

. "$CI_DIR/lib/in-container.sh"

mkdir -p "$PROJECT" "$HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME" "$XDG_STATE_HOME"
trap stop_mock EXIT
start_mock openai-chat "$CI_DIR/scripts/opencode-$SCENARIO.json" "$LOG"

cp /repo/integrations/opencode/AGENTS.md "$PROJECT/AGENTS.md"
OPENCODE_MARIGOLD_MCP=${OPENCODE_MARIGOLD_MCP:-0} node -e '
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
if (process.env.OPENCODE_MARIGOLD_MCP === "1") {
  cfg.mcp = { marigold: { type: "local", command: ["marigold-lsp", "mcp"], enabled: true } };
}
fs.writeFileSync(process.argv[1], JSON.stringify(cfg, null, 2) + "\n");
' "$PROJECT/opencode.json" "$MOCK_PORT"

cd "$PROJECT"
git init -q .
if [ "${OPENCODE_MARIGOLD_MCP:-0}" = 1 ]; then
  timeout 60 opencode mcp list >"$WORK/mcp-list-$SCENARIO.txt" 2>&1 || true
  cat "$WORK/mcp-list-$SCENARIO.txt"
fi
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
MCP_TOOL=marigold_marigold_check
SCOPE="--scope tool-results --after 1"
EDIT_SCOPE="--scope tool-results --after 2"
MCP_TOOL_ARGS="--scope tool-results --after 2"
rc=0
case "$SCENARIO" in
  error) assert $SCOPE --contains "$ERROR_MSG" || rc=$? ;;
  info) assert $SCOPE --contains "Wrote file successfully" --absent "$INFO_MSG" || rc=$? ;;
  warning) assert $SCOPE --contains "Wrote file successfully" --absent "$WARNING_MSG" || rc=$? ;;
  mixed)
    assert $SCOPE --contains "$ERROR_MSG" --absent "$INFO_MSG" || rc=$?
    assert $SCOPE --contains "$ERROR_MSG" --absent "$WARNING_MSG" || rc=$?
    ;;
  edit-error) assert $EDIT_SCOPE --contains "$ERROR_MSG" || rc=$? ;;
  edit-info) assert $EDIT_SCOPE --contains "Edit applied successfully" --absent "$INFO_MSG" || rc=$? ;;
  valid) assert $SCOPE --contains "Wrote file successfully" --absent "$ERROR_MSG" || rc=$? ;;
  mcp-info | mcp-warning | mcp-valid)
    [ "${OPENCODE_MARIGOLD_MCP:-0}" = 1 ] || { echo "$SCENARIO needs OPENCODE_MARIGOLD_MCP=1" >&2; exit 2; }
    grep -q "marigold" "$WORK/mcp-list-$SCENARIO.txt" || { echo "opencode mcp list does not show marigold" >&2; rc=1; }
    node -e '
const l = require("fs").readFileSync(process.argv[1], "utf8").split("\n").filter(Boolean).map(JSON.parse);
const names = (l.find((e) => e.turn === 1).request.tools || []).map((t) => t.function.name);
console.log("tools offered: " + names.join(" "));
process.exit(names.includes(process.argv[2]) ? 0 : 1);
' "$LOG" "$MCP_TOOL" || { echo "tools array lacks $MCP_TOOL" >&2; rc=1; }
    case "$SCENARIO" in
      mcp-info)
        assert $MCP_TOOL_ARGS --contains "information[undefined-fn]" || rc=$?
        assert $MCP_TOOL_ARGS --contains "0 errors, 0 warnings, 1 information" || rc=$?
        ;;
      mcp-warning)
        assert $MCP_TOOL_ARGS --contains "warning[undefined-stream-variable]" || rc=$?
        assert $MCP_TOOL_ARGS --contains "is read before it is declared" || rc=$?
        ;;
      mcp-valid)
        assert $MCP_TOOL_ARGS --contains "no diagnostics in" --absent "undefined-fn" || rc=$?
        assert $MCP_TOOL_ARGS --contains "no diagnostics in" --absent "information[" || rc=$?
        assert $MCP_TOOL_ARGS --contains "no diagnostics in" --absent "warning[" || rc=$?
        ;;
    esac
    ;;
  *) echo "unknown scenario $SCENARIO" >&2; rc=2 ;;
esac
if [ "$rc" -ne 0 ]; then
  echo "--- opencode stdout ---"; head -c 3000 "$WORK/opencode-$SCENARIO.out"
  echo "--- opencode stderr ---"; tail -c 4000 "$WORK/opencode-$SCENARIO.err"
fi
exit "$rc"
