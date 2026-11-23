#!/usr/bin/env bash
set -euo pipefail

gguf=${1:?usage: import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf}
host=${OLLAMA_HOST_URL:-http://localhost:11434}
name=${LEANSTRAL_MODEL:-leanstral-whitedwarf}
expected=${LEANSTRAL_SHA256:-1058b79167ac2f3c8872f64546d751fddfaf689204ca3ce30508f260e146afa0}

sum=$(sha256sum "$gguf" | cut -d' ' -f1)
[ "$sum" = "$expected" ] || { echo "import: checksum mismatch ($sum)" >&2; exit 1; }

code=$(curl -sS -o /dev/null -w '%{http_code}' -I "$host/api/blobs/sha256:$sum" || true)
if [ "$code" = 200 ]; then
  echo "import: blob sha256:$sum already present, skipping upload" >&2
else
  curl -fsS -T "$gguf" -X POST "$host/api/blobs/sha256:$sum" >/dev/null
fi

payload=$(python3 -I - "$sum" "$name" "$(dirname "$0")/Modelfile" "$(basename "$gguf")" <<'PY'
import json, re, sys
digest, name, modelfile, filename = sys.argv[1:5]
text = open(modelfile).read()
template = re.search(r'TEMPLATE """(.*?)"""', text, re.S).group(1)
text = re.sub(r'TEMPLATE """.*?"""', "", text, flags=re.S)
parameters = {}
for line in text.splitlines():
    m = re.match(r'PARAMETER\s+(\S+)\s+(.+?)\s*$', line)
    if not m:
        continue
    key, raw = m.groups()
    if len(raw) >= 2 and raw[0] == raw[-1] == '"':
        value = raw[1:-1]
    else:
        try:
            value = int(raw)
        except ValueError:
            value = float(raw)
    if key == "stop":
        parameters.setdefault("stop", []).append(value)
    else:
        parameters[key] = value
print(json.dumps({
    "model": name,
    "files": {filename: "sha256:" + digest},
    "template": template,
    "parameters": parameters,
    "stream": False,
}))
PY
)

response=$(curl -fsS "$host/api/create" -d "$payload")
printf '%s\n' "$response"
printf '%s' "$response" | python3 -I -c '
import json, sys
failed = False
for line in sys.stdin.read().splitlines():
    if line.strip():
        entry = json.loads(line)
        if "error" in entry:
            print("import: ollama error: " + str(entry["error"]), file=sys.stderr)
            failed = True
sys.exit(1 if failed else 0)
' || exit 1
