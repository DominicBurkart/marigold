#!/usr/bin/env python3
"""Independent (Python) oracle for the T4 remap rules. Usage: oracle.py FIXTURE_DIR -> JSON lines, one per rustc span (flattened, depth-first).
Written from the spec, NOT from the Rust reference implementation.
Segments in map.json are listed outer before inner, so the last segment containing a byte is the innermost.
A segment whose generated text equals its Marigold text is verbatim as a whole, whatever its kind;
a function segment is also verbatim over its common suffix."""
import json, sys
d = sys.argv[1]
src = open(f"{d}/program.marigold", "rb").read()
rust = open(f"{d}/generated.rs.txt", "rb").read()
segs = json.load(open(f"{d}/map.json"))["segments"]
lay = json.load(open(f"{d}/layout.json"))
prefix = lay["prefix"].encode()
genfile = lay["generated_file"]
base = len(prefix)

def is_boundary(b, i):
    return i == len(b) or (i < len(b) and (b[i] & 0xC0) != 0x80)

def lc(b, off):
    pre = b[:off]
    line = pre.count(b"\n") + 1
    ls = pre.rfind(b"\n") + 1
    return line, len(b[ls:off].decode("utf8")) + 1

def file_pos(off):
    l, c = lc(rust, off)
    if l == 1: c += len(prefix.rsplit(b"\n", 1)[-1].decode("utf8"))
    return l + prefix.count(b"\n"), c

def seg_ok(s):
    a, b = s["source"]["start"], s["source"]["end"]
    return a <= b <= len(src) and is_boundary(src, a) and is_boundary(src, b)

def verbatim(s):
    if not seg_ok(s): return 0
    g = rust[s["generated"]["start"]:s["generated"]["end"]]; t = src[s["source"]["start"]:s["source"]["end"]]
    if s["kind"] != "function" and g != t: return 0
    v = 0
    while v < min(len(g), len(t)) and g[-1 - v] == t[-1 - v]: v += 1
    while v and not (is_boundary(rust, s["generated"]["end"] - v) and is_boundary(src, s["source"]["end"] - v)): v -= 1
    return v

def covering(a, b):
    return [i for i, s in enumerate(segs) if s["generated"]["start"] < b and s["generated"]["end"] > a]

def innermost(p):
    c = [i for i, s in enumerate(segs) if s["generated"]["start"] <= p < s["generated"]["end"]]
    return c[-1] if c else None

def cls(a, b):
    if a == b:
        l = innermost(a); r = innermost(a - 1) if a > 0 else None
        i = l if l is not None else r
        if i is None: return ("unmapped", "glue", None, None)
        return seg_class(i, a, b)
    hits = covering(a, b)
    if not hits: return ("unmapped", "glue", None, None)
    if len({segs[i]["expr_index"] for i in hits}) > 1: return ("unmapped", "multiple_expressions", None, None)
    cont = [i for i in hits if segs[i]["generated"]["start"] <= a and b <= segs[i]["generated"]["end"]]
    if cont: return seg_class(cont[-1], a, b)
    w = hits[0]
    for i in hits:
        if segs[i]["source"]["end"] - segs[i]["source"]["start"] >= segs[w]["source"]["end"] - segs[w]["source"]["start"]: w = i
    if not seg_ok(segs[w]): return ("unmapped", "stale_source_map", None, None)
    return ("expression", None, (segs[w]["source"]["start"], segs[w]["source"]["end"]), w)

def seg_class(i, a, b):
    s = segs[i]
    if not seg_ok(s): return ("unmapped", "stale_source_map", None, None)
    v = verbatim(s); e = s["generated"]["end"]
    if v and a >= e - v and b <= e:
        return ("exact", None, (s["source"]["end"] - (e - a), s["source"]["end"] - (e - b)), i)
    return ("segment", None, (s["source"]["start"], s["source"]["end"]), i)

def consistent(sp, a, b):
    l1, c1 = file_pos(a); l2, c2 = file_pos(b)
    if any((sp["line_start"], sp["column_start"], sp["line_end"], sp["column_end"])):
        if (sp["line_start"], sp["column_start"], sp["line_end"], sp["column_end"]) != (l1, c1, l2, c2): return False
    text = sp["text"]
    if not text: return True
    if len(text) != l2 - l1 + 1: return False
    full = (prefix + rust)
    lines = full.split(b"\n")
    def chk(idx, line, expect):
        ours = lines[line - 1].decode("utf8").rstrip("\r") if line - 1 < len(lines) else None
        if ours is None: return False
        t = text[idx]
        if not (t["text"] == ours or (line == len(lines) and t["text"].startswith(ours))): return False
        piece = t["text"][t["highlight_start"] - 1:t["highlight_end"] - 1]
        return piece == expect
    st = rust[a:b].decode("utf8")
    first = st.split("\n")[0].rstrip("\r") if "\n" in st else st
    last = st.rsplit("\n", 1)[-1]
    return chk(0, l1, first) and (l1 == l2 or chk(len(text) - 1, l2, last))

def map_span(sp):
    cur = sp; via = None; hops = 0
    while cur["file_name"] != genfile:
        e = cur["expansion"]
        if e is None or hops >= 64: return {"precision": "unmapped", "reason": "foreign_file", "range": None}
        via = e["macro_decl_name"]; cur = e["span"]; hops += 1
    if cur["byte_end"] < cur["byte_start"]: return {"precision": "unmapped", "reason": "invalid_span", "range": None}
    if cur["byte_start"] < base or cur["byte_end"] > base + len(rust): return {"precision": "unmapped", "reason": "outside_generated_region", "range": None}
    a, b = cur["byte_start"] - base, cur["byte_end"] - base
    if not (is_boundary(rust, a) and is_boundary(rust, b)): return {"precision": "unmapped", "reason": "invalid_span", "range": None}
    if not consistent(cur, a, b): return {"precision": "unmapped", "reason": "stale_generated_file", "range": None}
    p, reason, rng, i = cls(a, b)
    out = {"precision": p, "reason": reason, "range": list(rng) if rng else None}
    if rng:
        out["slice"] = src[rng[0]:rng[1]].decode("utf8")
        out["start_lc"] = lc(src, rng[0]); out["end_lc"] = lc(src, rng[1])
        out["segment"] = [segs[i]["kind"], segs[i]["expr_index"], segs[i]["name"]]
        if hops: out["via_macro"] = via
    return out

def walk(m, path):
    spans = m["spans"]
    pidx = next((i for i, s in enumerate(spans) if s["is_primary"]), 0 if spans else None)
    for i, s in enumerate(spans):
        r = map_span(s); r["diag"] = path; r["role"] = "primary" if i == pidx else "secondary"; r["label"] = s["label"]
        r["suggestion"] = s["suggested_replacement"]
        yield r
    if not spans:
        yield {"diag": path, "role": "none", "precision": "unmapped", "reason": "no_span", "range": None}
    for ci, c in enumerate(m["children"]):
        yield from walk(c, path + [ci])

for n, line in enumerate(open(f"{d}/bare.jsonl")):
    m = json.loads(line)
    for r in walk(m, [n]):
        r["msg"] = m["message"][:60]
        print(json.dumps(r, ensure_ascii=False))
