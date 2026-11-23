import pathlib
import re
import sys
import tomllib

USAGE = "usage: gate.py (ledger|coverage) [ROOT]"
STATUSES = {"candidate", "untested", "covered", "proven", "disputed"}
SURFACES = {"interface", "internal"}
PROVABLE = {"none", "kani", "lean"}
DONE = {"covered", "proven"}
REQUIRED = ("id", "surface", "statement", "status", "provable", "covered_by")
TEST_DIRS = ("tests", "marigold")


def allowed_top(part):
    return part in TEST_DIRS or part.startswith("marigold-")


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
    elif not p.parts or not allowed_top(p.parts[0]):
        errors.append(f"{label}: must be a file under tests/ or marigold-*/: {rel}")
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
    if prefix is None and not allowed_top(real.relative_to(base).parts[0]):
        errors.append(f"{label}: resolves outside tests/ and marigold-*/: {rel}")
        return None
    return full


def check_covered_by(root, inv, errors):
    label = inv["id"]
    done = inv["status"] in DONE
    if done and not inv["covered_by"]:
        errors.append(f"{label}: {inv['status']} without covered_by")
    for entry in inv["covered_by"]:
        path, _, name = entry.partition("::")
        full = resolve(root, path, label, errors)
        if full is None or not done or not name:
            continue
        last = name.split("::")[-1]
        text = full.read_text(errors="replace")
        if not re.search(rf"\bfn\s+{re.escape(last)}\b", text):
            errors.append(f"{label}: no fn {last} in {path}")


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
    resolve(root, proof, f"{label}: proof", errors, prefix="tests/proofs/")


def check_entry(root, inv, seen, errors):
    if not isinstance(inv, dict):
        errors.append("invariant entry is not a table")
        return
    label = inv.get("id", "?")
    missing = [k for k in REQUIRED if k not in inv]
    if missing:
        errors.append(f"{label}: missing field(s): {', '.join(missing)}")
        return
    for k in ("id", "statement"):
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
