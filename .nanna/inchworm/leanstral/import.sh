#!/usr/bin/env bash
set -euo pipefail

gguf=${1:?usage: import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf}
host=${OLLAMA_HOST_URL:-http://localhost:11434}
name=${LEANSTRAL_MODEL:-leanstral-whitedwarf}
expected=1058b79167ac2f3c8872f64546d751fddfaf689204ca3ce30508f260e146afa0

sum=$(sha256sum "$gguf" | cut -d' ' -f1)
[ "$sum" = "$expected" ] || { echo "import: checksum mismatch ($sum)" >&2; exit 1; }

curl -fsS -X POST "$host/api/blobs/sha256:$sum" --data-binary @"$gguf" >/dev/null

payload=$(python3 -I - "$sum" "$name" "$(dirname "$0")/Modelfile" <<'PY'
import json, re, sys
digest, name, modelfile = sys.argv[1:4]
text = open(modelfile).read()
template = re.search(r'TEMPLATE """(.*?)"""', text, re.S).group(1)
print(json.dumps({
    "model": name,
    "files": {"Leanstral-WhiteDwarf-seed-v3.gguf": "sha256:" + digest},
    "template": template,
    "parameters": {"temperature": 1.0, "num_ctx": 8192, "stop": ["</s>"]},
    "stream": False,
}))
PY
)
curl -fsS "$host/api/create" -d "$payload"
