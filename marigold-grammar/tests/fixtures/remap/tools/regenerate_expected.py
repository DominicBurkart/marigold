#!/usr/bin/env python3
"""Rewrite `expected.jsonl` in every fixture directory from the independent oracle (`oracle.py`).

`oracle.py` implements the mapping rules of `src/remap.rs` a second time, in Python, from the
written rules and never from the Rust code; `tests/remap.rs::n22_...` compares the Rust output
with the frozen result. Run it only after a fixture changed or a rule was deliberately changed
in both implementations:

    python3 -I marigold-grammar/tests/fixtures/remap/tools/regenerate_expected.py
"""
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
ORACLE = os.path.join(HERE, "oracle.py")
KEYS = ("diag", "role", "precision", "reason", "range", "slice", "start_lc", "end_lc",
        "segment", "via_macro", "label", "suggestion")

total = 0
for name in sorted(os.listdir(ROOT)):
    d = os.path.join(ROOT, name)
    if not os.path.exists(os.path.join(d, "bare.jsonl")):
        continue
    out = subprocess.run([sys.executable, "-I", ORACLE, d], capture_output=True, text=True, check=True).stdout
    rows = []
    for line in out.splitlines():
        v = json.loads(line)
        if v.get("role") == "none":
            v["label"] = None
            v["suggestion"] = None
        rows.append({k: v.get(k) for k in KEYS})
    rows.sort(key=lambda r: json.dumps(r, sort_keys=True, ensure_ascii=False))
    with open(os.path.join(d, "expected.jsonl"), "w", encoding="utf8", newline="\n") as f:
        for r in rows:
            f.write(json.dumps(r, sort_keys=True, ensure_ascii=False) + "\n")
    total += len(rows)
print("spans", total)
