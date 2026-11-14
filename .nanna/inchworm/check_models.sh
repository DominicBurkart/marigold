#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."
host=${OLLAMA_HOST_URL:-http://localhost:11434}
probe=${PROBE:-1}

models=$(sed -n 's/^model = "\(.*\)"$/\1/p' .nanna/catalog/marigold-*.toml | sort -u)
status=0
for model in $models; do
  if ! curl -fsS -m 10 "$host/api/show" -d "{\"model\":\"$model\"}" >/dev/null 2>&1; then
    echo "check_models: $model is not served by $host" >&2
    status=1
    continue
  fi
  if [ "$probe" = 1 ]; then
    reply=$(curl -fsS -m 900 "$host/api/chat" -d "{\"model\":\"$model\",\"stream\":false,\"options\":{\"num_predict\":8,\"num_ctx\":1024},\"messages\":[{\"role\":\"user\",\"content\":\"Reply with the word ok.\"}]}" 2>/dev/null || true)
    if ! printf '%s' "$reply" | python3 -I -c 'import json,sys; d=json.load(sys.stdin); sys.exit(0 if d["message"]["content"].strip() else 1)' 2>/dev/null; then
      echo "check_models: $model did not generate" >&2
      status=1
      continue
    fi
  fi
  echo "check_models: $model ok"
done
exit "$status"
