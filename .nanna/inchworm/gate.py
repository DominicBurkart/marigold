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


def allowed_top(parts):
    if parts[0] == "tests" or parts[0].startswith("marigold-"):
        return True
    return parts[0] == "marigold" and len(parts) > 1 and parts[1] == "tests"


def strip_rust(text):
    out = []
    i = 0
    n = len(text)

    def blank(chunk):
        return "".join("\n" if c == "\n" else " " for c in chunk)

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


def attributes_before(text, pos):
    prefix = text[:pos].rstrip()
    prefix = re.sub(r"(?:(?:pub(?:\([^)]*\))?|async|unsafe|const)\s+)+$", "", prefix + " ").rstrip()
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
        attrs.append(prefix[j + 1:-1].strip())
        prefix = prefix[: j - 1].rstrip()
    return attrs


def attr_name(attr):
    m = re.match(r"[A-Za-z_][\w:]*", attr)
    return m.group(0) if m else ""


def inside_proptest(text, pos):
    for m in re.finditer(r"\bproptest!\s*\{", text):
        end = matching(text, m.end() - 1, "{", "}")
        if m.end() <= pos < (end if end != -1 else len(text)):
            return True
    return False


def check_test_fn(text, name):
    stripped = strip_rust(text)
    matches = list(re.finditer(rf"\bfn\s+{re.escape(name)}\b", stripped))
    if not matches:
        return f"no fn {name} (outside comments and strings)"
    reasons = []
    for m in matches:
        names = [attr_name(a) for a in attributes_before(stripped, m.start())]
        if "ignore" in names:
            reasons.append(f"fn {name} is #[ignore]d")
            continue
        is_test = any(a == "test" or a.endswith("::test") for a in names)
        if is_test or inside_proptest(stripped, m.start()):
            return None
        reasons.append(f"fn {name} is not a test")
    return reasons[0]


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
    return full


def check_covered_by(root, inv, errors):
    label = inv["id"]
    done = inv["status"] in DONE
    if done and not inv["covered_by"]:
        errors.append(f"{label}: {inv['status']} without covered_by")
    for entry in inv["covered_by"]:
        path, sep, name = entry.partition("::")
        full = resolve(root, path, label, errors)
        if full is None or not done:
            continue
        last = name.split("::")[-1]
        if not sep or not last:
            errors.append(f"{label}: {inv['status']} entries must be path::name: {entry}")
            continue
        problem = check_test_fn(full.read_text(errors="replace"), last)
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
    if full is not None and not full.read_text(errors="replace").strip():
        errors.append(f"{label}: proof file is empty: {proof}")


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
