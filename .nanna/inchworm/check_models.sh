#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."
host=${OLLAMA_HOST_URL:-http://localhost:11434}
probe=${PROBE:-1}
timeout_secs=${LOAD_TIMEOUT_SECS:-900}
num_ctx=$(sed -n 's/^PARAMETER num_ctx \([0-9]*\)$/\1/p' .nanna/inchworm/leanstral/Modelfile)
: "${num_ctx:?check_models: no num_ctx in leanstral/Modelfile}"

models=$(python3 -I - .nanna/catalog/marigold-*.toml <<'PY'
import sys, tomllib
seen = set()
for path in sys.argv[1:]:
    with open(path, "rb") as f:
        seen.add(tomllib.load(f)["identity"]["model"])
print("\n".join(sorted(seen)))
PY
)
status=0
for model in $models; do
  if ! curl -fsS -m 10 "$host/api/show" -d "{\"model\":\"$model\"}" >/dev/null 2>&1; then
    echo "check_models: $model is not served by $host" >&2
    status=1
    continue
  fi
  if [ "$probe" = 1 ]; then
    rc=0
    reply=$(curl -fsS -m "$timeout_secs" "$host/api/chat" -d "{\"model\":\"$model\",\"stream\":false,\"options\":{\"num_predict\":8,\"num_ctx\":$num_ctx},\"messages\":[{\"role\":\"user\",\"content\":\"Reply with the word ok.\"}]}" 2>/dev/null) || rc=$?
    if [ "$rc" = 28 ]; then
      echo "check_models: $model did not load within ${timeout_secs}s (timeout)" >&2
      status=1
      continue
    elif [ "$rc" != 0 ]; then
      echo "check_models: $model request failed (curl exit $rc)" >&2
      status=1
      continue
    fi
    if ! printf '%s' "$reply" | python3 -I -c 'import json,sys; d=json.load(sys.stdin); sys.exit(0 if d["message"]["content"].strip() else 1)' 2>/dev/null; then
      echo "check_models: $model did not generate" >&2
      status=1
      continue
    fi
  fi
  echo "check_models: $model ok"
done
exit "$status"
