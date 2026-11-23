import pathlib
import re
import sys
import tomllib

USAGE = "usage: gate.py (ledger|coverage) [ROOT]"
STATUSES = {"candidate", "covered", "proven", "disputed"}
SURFACES = {"interface", "internal"}
PROVABLE = {"none", "kani", "lean"}
DONE = {"covered", "proven"}
REQUIRED = ("id", "surface", "statement", "status", "provable", "covered_by")
FEATURE = r'feature="(?:tokio|async-std)"'
CFG_PREDICATE = re.compile(rf"test|{FEATURE}|any\({FEATURE}(?:,{FEATURE})*,?\)")
QUALIFIERS = re.compile(r"(?:(?:pub(?:\([^)]*\))?|async|unsafe|const|extern)\s+)+$")
FN_ALLOWED = {"test", "should_panic", "cfg", "allow", "doc"}
MOD_ALLOWED = {"cfg", "allow", "doc"}


def allowed_top(parts):
    if parts[0] == "tests" or parts[0].startswith("marigold-"):
        return True
    return parts[0] == "marigold" and len(parts) > 1 and parts[1] == "tests"


def blank(chunk):
    return "".join("\n" if c == "\n" else " " for c in chunk)


def strip_rust(text):
    out = []
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if text.startswith("//", i):
            j = text.find("\n", i)
            j = n if j == -1 else j
            out.append(blank(text[i:j]))
            i = j
        elif text.startswith("/*", i):
            depth = 1
            j = i + 2
            while j < n and depth:
                if text.startswith("/*", j):
                    depth += 1
                    j += 2
                elif text.startswith("*/", j):
                    depth -= 1
                    j += 2
                else:
                    j += 1
            out.append(blank(text[i:j]))
            i = j
        elif c == "r" and re.match(r'r#*"', text[i:i + 70]) and (i == 0 or not (text[i - 1].isalnum() or text[i - 1] == "_")):
            hashes = len(re.match(r"r(#*)", text[i:]).group(1))
            close = '"' + "#" * hashes
            j = text.find(close, i + 2 + hashes)
            j = n if j == -1 else j + len(close)
            out.append(blank(text[i:j]))
            i = j
        elif c == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            j = min(j + 1, n)
            out.append(blank(text[i:j]))
            i = j
        elif c == "'":
            m = re.match(r"'(?:\\.[^']*|[^\\'])'", text[i:i + 12])
            if m:
                out.append(blank(m.group(0)))
                i += len(m.group(0))
            else:
                out.append(c)
                i += 1
        else:
            out.append(c)
            i += 1
    return "".join(out)


def matching(text, start, open_c, close_c):
    depth = 0
    for j in range(start, len(text)):
        if text[j] == open_c:
            depth += 1
        elif text[j] == close_c:
            depth -= 1
            if depth == 0:
                return j
    return -1


def attributes_before(stripped, orig, pos):
    prefix = stripped[:pos].rstrip()
    prefix = QUALIFIERS.sub("", prefix + " ").rstrip()
    attrs = []
    while prefix.endswith("]"):
        depth = 0
        j = len(prefix) - 1
        while j >= 0:
            if prefix[j] == "]":
                depth += 1
            elif prefix[j] == "[":
                depth -= 1
                if depth == 0:
                    break
            j -= 1
        if j < 1 or prefix[j - 1] != "#":
            break
        attrs.append(orig[j + 1:len(prefix) - 1].strip())
        prefix = prefix[: j - 1].rstrip()
    return list(reversed(attrs))


def leading_attributes(stripped, orig, start, end, inner=False):
    attrs = []
    i = start
    opener = r"#!\[" if inner else r"#\["
    while True:
        m = re.compile(rf"\s*{opener}").match(stripped, i, end)
        if not m:
            break
        close = matching(stripped, m.end() - 1, "[", "]")
        if close == -1 or close >= end:
            break
        attrs.append(orig[m.end():close].strip())
        i = close + 1
    return attrs, i


def attr_name(attr):
    m = re.match(r"[A-Za-z_][\w:]*", attr)
    return m.group(0) if m else ""


def classify(header):
    h = header.strip()
    if re.fullmatch(r"(?:pub(?:\([^)]*\))?\s+)?mod\s+\w+", h):
        return "mod", h.split()[-1]
    if re.match(r"macro_rules\s*!", h):
        return "macro", ""
    m = re.search(r"(\w+)\s*!\s*$", h)
    if m:
        return ("proptest", "") if m.group(1) == "proptest" else ("macro", "")
    if re.search(r"(?:^|\W)fn\s+\w+", h):
        return "fn", ""
    return "other", ""


def contexts_at(stripped, orig, pos):
    stack = []
    header_start = 0
    pdepth = 0
    for i in range(pos):
        ch = stripped[i]
        if ch in "([":
            pdepth += 1
        elif ch in ")]":
            pdepth = max(0, pdepth - 1)
        elif ch == "{":
            attrs, after = leading_attributes(stripped, orig, header_start, i)
            kind, name = classify(stripped[after:i])
            stack.append({"kind": kind, "name": name, "attrs": attrs})
            header_start = i + 1
        elif ch == "}":
            if stack:
                stack.pop()
            header_start = i + 1
        elif ch == ";" and pdepth == 0:
            header_start = i + 1
    return stack


def check_attrs(attrs, allowed, label):
    for attr in attrs:
        name = attr_name(attr)
        if name == "ignore":
            return f"{label} is #[ignore]d"
        if name == "cfg":
            squashed = re.sub(r"\s+", "", attr)
            m = re.fullmatch(r"cfg\((.*)\)", squashed)
            if not m or not CFG_PREDICATE.fullmatch(m.group(1)):
                return f"{label} is gated by #[{attr}], which the gate cannot evaluate"
        elif name not in allowed and not name.endswith("::test"):
            return f"{label} has an unsupported attribute form: #[{attr}]"
    return None


def check_test_fn(text, entry_name):
    *module_path, name = entry_name.split("::")
    stripped = strip_rust(text)
    matches = list(re.finditer(rf"\bfn\s+{re.escape(name)}\b", stripped))
    candidates = []
    for m in matches:
        stack = contexts_at(stripped, text, m.start())
        chain = [c["name"] for c in stack if c["kind"] == "mod"]
        if module_path and chain[len(chain) - len(module_path):] != module_path:
            continue
        candidates.append((m, stack))
    if not candidates:
        return f"no fn {entry_name} (outside comments and strings)"
    if len(candidates) > 1:
        return f"ambiguous: fn {name} is defined {len(candidates)} times; cite its module path"
    m, stack = candidates[0]
    label = f"fn {name}"
    inner, _ = leading_attributes(stripped, text, 0, len(stripped), inner=True)
    problem = check_attrs(inner, MOD_ALLOWED, "the file")
    if problem:
        return problem
    in_proptest = False
    for ctx in stack:
        if ctx["kind"] == "fn":
            return f"{label} is nested inside another fn"
        if ctx["kind"] == "macro":
            return f"{label} is defined inside a macro"
        if ctx["kind"] == "other":
            return f"{label} is inside an unsupported block"
        if ctx["kind"] == "proptest":
            in_proptest = True
        elif ctx["kind"] == "mod":
            problem = check_attrs(ctx["attrs"], MOD_ALLOWED, f"module {ctx['name']}")
            if problem:
                return problem
    attrs = attributes_before(stripped, text, m.start())
    problem = check_attrs(attrs, FN_ALLOWED, label)
    if problem:
        return problem
    names = [attr_name(a) for a in attrs]
    if any(n == "test" or n.endswith("::test") for n in names) or in_proptest:
        return None
    return f"{label} is not a test"


def resolve(root, rel, label, errors, prefix=None):
    p = pathlib.PurePosixPath(rel)
    if p.is_absolute():
        errors.append(f"{label}: absolute path not allowed: {rel}")
        return None
    if ".." in p.parts:
        errors.append(f"{label}: '..' not allowed: {rel}")
        return None
    if prefix is not None:
        if not rel.startswith(prefix):
            errors.append(f"{label}: must be under {prefix}: {rel}")
            return None
    elif not p.parts or not allowed_top(p.parts):
        errors.append(f"{label}: must be a file under tests/, marigold/tests/ or marigold-*/: {rel}")
        return None
    full = root / rel
    if not full.exists():
        errors.append(f"{label}: path missing: {rel}")
        return None
    if not full.is_file():
        errors.append(f"{label}: not a file: {rel}")
        return None
    real = full.resolve()
    base = root.resolve()
    if not real.is_relative_to(base):
        errors.append(f"{label}: resolves outside the repository: {rel}")
        return None
    if prefix is None and not allowed_top(real.relative_to(base).parts):
        errors.append(f"{label}: resolves outside tests/ and marigold-*/: {rel}")
        return None
    if prefix is not None and not real.is_relative_to((base / prefix).resolve()):
        errors.append(f"{label}: resolves outside {prefix}: {rel}")
        return None
    return full


def check_covered_by(root, inv, errors):
    label = inv["id"]
    done = inv["status"] in DONE
    if done and not inv["covered_by"]:
        errors.append(f"{label}: {inv['status']} without covered_by")
    for entry in inv["covered_by"]:
        path, sep, name = entry.partition("::")
        full = resolve(root, path, label, errors)
        if full is None:
            continue
        if full.suffix != ".rs" or not path.endswith(".rs"):
            errors.append(f"{label}: covered_by must name a .rs file: {path}")
            continue
        if not done:
            continue
        if not sep or not name.split("::")[-1]:
            errors.append(f"{label}: {inv['status']} entries must be path::name: {entry}")
            continue
        problem = check_test_fn(full.read_text(errors="replace"), name)
        if problem:
            errors.append(f"{label}: {path}: {problem}")


def check_proof(root, inv, errors):
    label = inv["id"]
    if inv["status"] != "proven":
        return
    if inv["provable"] == "none":
        errors.append(f"{label}: proven but provable is none")
    proof = inv.get("proof")
    if not isinstance(proof, str) or not proof:
        errors.append(f"{label}: proven without a proof artifact path")
        return
    full = resolve(root, proof, f"{label}: proof", errors, prefix="tests/proofs/")
    if full is None:
        return
    if full.resolve().suffix not in (".rs", ".lean") or pathlib.PurePosixPath(proof).suffix not in (".rs", ".lean"):
        errors.append(f"{label}: proof suffix must be .rs or .lean: {proof}")
        return
    text = full.read_text(errors="replace")
    if not text.strip():
        errors.append(f"{label}: proof file is empty: {proof}")
        return
    if proof.endswith(".rs") and not re.search(r"#\s*\[\s*(?:kani::proof|test)\b", strip_rust(text)):
        errors.append(f"{label}: no proof harness (#[kani::proof] or #[test]) in {proof}")


def check_entry(root, inv, seen, errors):
    if not isinstance(inv, dict):
        errors.append("invariant entry is not a table")
        return
    label = inv.get("id", "?") if isinstance(inv.get("id", "?"), str) else "?"
    missing = [k for k in REQUIRED if k not in inv]
    if missing:
        errors.append(f"{label}: missing field(s): {', '.join(missing)}")
        return
    for k in ("id", "statement", "status", "surface", "provable"):
        if not isinstance(inv[k], str) or not inv[k]:
            errors.append(f"{label}: {k} must be a non-empty string")
            return
    if inv["id"] in seen:
        errors.append(f"{label}: duplicate id")
    seen.add(inv["id"])
    for key, allowed in (("status", STATUSES), ("surface", SURFACES), ("provable", PROVABLE)):
        if inv[key] not in allowed:
            errors.append(f"{label}: {key} must be one of {sorted(allowed)}, got {inv[key]!r}")
            return
    cb = inv["covered_by"]
    if not isinstance(cb, list) or not all(isinstance(x, str) for x in cb):
        errors.append(f"{label}: covered_by must be a list of strings")
        return
    for key in ("proof_blocker", "proof"):
        if key in inv and not isinstance(inv[key], str):
            errors.append(f"{label}: {key} must be a string")
            return
    check_covered_by(root, inv, errors)
    check_proof(root, inv, errors)


def main(argv):
    if not argv or argv[0] not in ("ledger", "coverage") or len(argv) > 2:
        print(USAGE, file=sys.stderr)
        return 2
    phase = argv[0]
    root = pathlib.Path(argv[1]) if len(argv) == 2 else pathlib.Path(__file__).resolve().parents[2]
    ledger_path = root / "tests/invariants.toml"
    try:
        data = tomllib.loads(ledger_path.read_text())
    except OSError as e:
        print(f"cannot read {ledger_path}: {e.strerror}")
        return 1
    except tomllib.TOMLDecodeError as e:
        print(f"{ledger_path} is not valid TOML: {e}")
        return 1
    ledger = data.get("invariant")
    if not isinstance(ledger, list) or not ledger:
        print(f"{ledger_path} has no [[invariant]] entries")
        return 1
    errors = []
    seen = set()
    for inv in ledger:
        check_entry(root, inv, seen, errors)
    if phase == "coverage":
        for inv in ledger:
            if isinstance(inv, dict) and inv.get("surface") == "interface" and inv.get("status") not in DONE:
                errors.append(f"{inv.get('id', '?')}: interface invariant not covered ({inv.get('status')})")
    print("\n".join(errors))
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
