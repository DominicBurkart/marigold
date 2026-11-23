import os
import pathlib
import re
import subprocess
import sys
import tomllib
import unicodedata

USAGE = "usage: gate.py (ledger|coverage) [--run] [ROOT]"
STATUSES = {"candidate", "covered", "proven", "disputed"}
SURFACES = {"interface", "internal"}
PROVABLE = {"none", "kani", "lean"}
DONE = {"covered", "proven"}
REQUIRED = ("id", "surface", "statement", "status", "provable", "covered_by")
CRATES = ("marigold", "marigold-impl", "marigold-grammar", "marigold-lsp", "marigold-macros", "tests")
TEST_ATTRS = {"test", "tokio::test", "async_std::test", "rstest::rstest"}
FN_ALLOWED = TEST_ATTRS | {"should_panic", "cfg", "allow", "doc"}
MOD_ALLOWED = {"cfg", "allow", "doc"}
PROOF_FN_ALLOWED = {"kani::proof", "kani::unwind", "kani::solver", "cfg", "allow", "doc"}
PROPTEST_INNER = {"proptest_config"}
LOCATIONS = "<crate>/tests/*.rs or a <crate>/src file declared by mod"

SPECIAL = re.compile(r"""//|/\*|"|'|(?<![\w])(?:(?:br|cr|r)#*"|[bc]"|b')""")
STR_RE = re.compile(r'"(?:[^"\\]|\\.)*"?', re.S)
CHAR_RE = re.compile(r"'(?:\\.[^'\n]{0,10}|[^\\'\n])'")
FN_RE = re.compile(r"\bfn\s+(\w+)")
INNER_RE = re.compile(r"#\s*!\s*\[")
OUTER_START = re.compile(r"\s*#\s*\[")
INNER_START = re.compile(r"\s*#\s*!\s*\[")
QUAL_TAIL = re.compile(r"(?<![\w])(?:(?:pub(?:\([^)]*\))?|async|unsafe|const|extern)\b\s*)+\Z")
MOD_HEADER = re.compile(r"(?:pub(?:\([^)]*\))?\s+)?mod\s+(\w+)")
FN_HEADER = re.compile(r"(?:^|\W)fn\s+\w+")
PROPTEST_HEAD = re.compile(r"(?:proptest::)?proptest\s*!")
FEATURE_NAME = re.compile(r'feature\s*=\s*"([^"]+)"')


def cfg_regex(extra=()):
    atoms = ["test", *extra, r'feature\s*=\s*"(?:tokio|async-std)"']
    atom = "(?:" + "|".join(atoms) + ")"
    anyp = rf"any\s*\(\s*{atom}(?:\s*,\s*{atom})*\s*,?\s*\)"
    return re.compile(rf"cfg\s*\(\s*(?:{atom}|{anyp})\s*\)")


CFG_TEST = cfg_regex()
CFG_PROOF = cfg_regex(("kani",))


def blank(chunk):
    return "".join("\n" if c == "\n" else " " for c in chunk)


def strip_rust(text):
    out = []
    n = len(text)
    ps = 0
    i = 0
    while True:
        m = SPECIAL.search(text, i)
        if not m:
            break
        s = m.start()
        tok = m.group(0)
        end = None
        if tok == "//":
            j = text.find("\n", s)
            end = n if j == -1 else j
        elif tok == "/*":
            depth = 1
            j = s + 2
            while j < n and depth:
                if text.startswith("/*", j):
                    depth += 1
                    j += 2
                elif text.startswith("*/", j):
                    depth -= 1
                    j += 2
                else:
                    j += 1
            end = j
        elif tok == '"':
            end = STR_RE.match(text, s).end()
        elif tok == "'":
            cm = CHAR_RE.match(text, s)
            end = cm.end() if cm else None
        elif tok == "b'":
            cm = CHAR_RE.match(text, s + 1)
            end = cm.end() if cm else None
        elif tok in ('b"', 'c"'):
            end = STR_RE.match(text, s + 1).end()
        else:
            close = '"' + "#" * tok.count("#")
            j = text.find(close, m.end())
            end = n if j == -1 else j + len(close)
        if end is None:
            i = m.end() if tok != "'" else s + 1
            continue
        out.append(text[ps:s])
        out.append(blank(text[s:end]))
        ps = end
        i = end
    out.append(text[ps:])
    return "".join(out)


def rstrip_index(text, end):
    while end > 0 and text[end - 1].isspace():
        end -= 1
    return end


def matching(text, start, open_c, close_c, end):
    depth = 0
    for j in range(start, end):
        c = text[j]
        if c == open_c:
            depth += 1
        elif c == close_c:
            depth -= 1
            if depth == 0:
                return j
    return -1


def attributes_before(stripped, orig, pos):
    end = rstrip_index(stripped, pos)
    m = QUAL_TAIL.search(stripped, max(0, end - 400), end)
    if m:
        end = rstrip_index(stripped, m.start())
    attrs = []
    while end > 0 and stripped[end - 1] == "]":
        depth = 0
        j = end - 1
        while j >= 0:
            if stripped[j] == "]":
                depth += 1
            elif stripped[j] == "[":
                depth -= 1
                if depth == 0:
                    break
            j -= 1
        if j < 0:
            break
        k = rstrip_index(stripped, j)
        if k < 1 or stripped[k - 1] != "#":
            break
        attrs.append(orig[j + 1:end - 1].strip())
        end = rstrip_index(stripped, k - 1)
    return list(reversed(attrs))


def leading_attributes(stripped, orig, start, end, inner, consumed):
    pattern = INNER_START if inner else OUTER_START
    attrs = []
    i = start
    while True:
        m = pattern.match(stripped, i, end)
        if not m:
            break
        close = matching(stripped, m.end() - 1, "[", "]", end)
        if close == -1:
            break
        if consumed is not None:
            consumed.add(m.end() - 1)
        attrs.append(orig[m.end():close].strip())
        i = close + 1
    return attrs, i


def attr_name(attr):
    m = re.match(r"[A-Za-z_][\w:]*", attr)
    return m.group(0) if m else ""


def classify(header):
    h = header.strip()
    m = MOD_HEADER.fullmatch(h)
    if m:
        return "mod", m.group(1)
    if re.match(r"macro_rules\s*!", h):
        return "macro", ""
    if h.endswith("!"):
        return ("proptest", "") if PROPTEST_HEAD.fullmatch(h) else ("macro", "")
    if FN_HEADER.search(h):
        return "fn", ""
    return "other", ""


class Analysis:
    def __init__(self, text):
        self.text = text
        self.stripped = strip_rust(text)
        s = self.stripped
        n = len(s)
        consumed = set()
        self.file_inner, _ = leading_attributes(s, text, 0, n, True, consumed)
        marks = {m.start(): m.group(1) for m in FN_RE.finditer(s)}
        self.found = []
        stack = []
        hs = 0
        pd = 0
        for i, ch in enumerate(s):
            if ch == "f":
                name = marks.get(i)
                if name is not None:
                    self.found.append((i, name, tuple(stack)))
            elif ch in "([":
                pd += 1
            elif ch in ")]":
                pd = max(0, pd - 1)
            elif ch == "{":
                attrs, after = leading_attributes(s, text, hs, i, False, None)
                kind, name = classify(s[after:i])
                inner, _ = leading_attributes(s, text, i + 1, n, True, consumed)
                stack.append({"kind": kind, "name": name, "attrs": attrs, "inner": inner})
                hs = i + 1
            elif ch == "}":
                if stack:
                    stack.pop()
                hs = i + 1
            elif ch == ";" and pd == 0:
                hs = i + 1
        self.global_problem = None
        for m in INNER_RE.finditer(s):
            if m.end() - 1 not in consumed:
                self.global_problem = "inner attribute #![...] outside the head of the file or of a body"
                break


def check_attrs(attrs, allowed, cfg_re, label):
    for attr in attrs:
        name = attr_name(attr)
        if name == "ignore":
            return f"{label} is #[ignore]d"
        if name == "cfg":
            if not cfg_re.fullmatch(attr):
                return f"{label} is gated by #[{attr}], which the gate cannot evaluate"
        elif name not in allowed:
            return f"{label} has an unsupported attribute form: #[{attr}]"
    return None


def features_of(attrs):
    feats = []
    for attr in attrs:
        if attr_name(attr) == "cfg":
            found = FEATURE_NAME.findall(attr)
            if found:
                feats.append("tokio" if "tokio" in found else found[0])
    return feats


def check_contexts(stack, label, cfg_re):
    features = []
    in_proptest = False
    for ctx in stack:
        kind = ctx["kind"]
        if kind == "fn":
            return f"{label} is nested inside another fn", features
        if kind == "macro":
            return f"{label} is defined inside a macro", features
        if kind == "other":
            return f"{label} is inside an unsupported block", features
        if kind == "proptest":
            in_proptest = True
            problem = check_attrs(ctx["attrs"], MOD_ALLOWED, cfg_re, "proptest!")
            if problem:
                return problem, features
            problem = check_attrs(ctx["inner"], PROPTEST_INNER, cfg_re, "proptest!")
        else:
            problem = check_attrs(ctx["attrs"] + ctx["inner"], MOD_ALLOWED, cfg_re, f"module {ctx['name']}")
            features += features_of(ctx["attrs"] + ctx["inner"])
        if problem:
            return problem, features
    return None, features


def locate_test(text, entry_name):
    *module_path, name = entry_name.split("::")
    a = Analysis(text)
    if a.global_problem:
        return a.global_problem, []
    candidates = []
    for pos, fname, stack in a.found:
        if fname != name:
            continue
        chain = [c["name"] for c in stack if c["kind"] == "mod"]
        if module_path and chain[len(chain) - len(module_path):] != module_path:
            continue
        candidates.append((pos, stack))
    if not candidates:
        return f"no fn {entry_name} (outside comments and strings)", []
    if len(candidates) > 1:
        return f"ambiguous: fn {name} is defined {len(candidates)} times; cite its module path", []
    pos, stack = candidates[0]
    label = f"fn {name}"
    problem = check_attrs(a.file_inner, MOD_ALLOWED, CFG_TEST, "the file")
    if problem:
        return problem, []
    features = features_of(a.file_inner)
    problem, more = check_contexts(stack, label, CFG_TEST)
    if problem:
        return problem, []
    features += more
    attrs = attributes_before(a.stripped, text, pos)
    problem = check_attrs(attrs, FN_ALLOWED, CFG_TEST, label)
    if problem:
        return problem, []
    features += features_of(attrs)
    if not any(attr_name(x) in TEST_ATTRS for x in attrs):
        return f"{label} is not a test", []
    return None, sorted(set(features))


def proof_harness_problem(text):
    a = Analysis(text)
    if a.global_problem:
        return a.global_problem
    if check_attrs(a.file_inner, MOD_ALLOWED, CFG_PROOF, "the file"):
        return "no proof harness: file-level attribute not supported"
    for pos, name, stack in a.found:
        attrs = attributes_before(a.stripped, text, pos)
        if not any(attr_name(x) == "kani::proof" for x in attrs):
            continue
        problem, _ = check_contexts(stack, f"fn {name}", CFG_PROOF)
        if problem is None and check_attrs(attrs, PROOF_FN_ALLOWED, CFG_PROOF, f"fn {name}") is None:
            return None
    return "no proof harness (a #[kani::proof] fn outside macros, nested fns and unsupported cfg)"


def declared_problem(root, crate, parts, depth=0):
    if parts in (["lib.rs"], ["main.rs"]):
        return None
    if depth > 32:
        return "module nesting too deep"
    *dirs, fname = parts
    if fname == "mod.rs":
        name = dirs[-1]
        parent_dirs = dirs[:-1]
    else:
        name = fname[:-3]
        parent_dirs = dirs
    if parent_dirs:
        parents = [parent_dirs[:-1] + [parent_dirs[-1] + ".rs"], parent_dirs + ["mod.rs"]]
    else:
        parents = [["lib.rs"], ["main.rs"]]
    base = root / crate / "src"
    last = None
    for par in parents:
        f = base.joinpath(*par)
        if not f.is_file():
            continue
        text = f.read_text(errors="replace")
        stripped = strip_rust(text)
        pattern = re.compile(rf"^[ \t]*(?P<d>(?:pub(?:\([^)]*\))?\s+)?mod\s+{re.escape(name)}\s*;)", re.M)
        m = pattern.search(stripped)
        if not m:
            continue
        attrs = attributes_before(stripped, text, m.start("d"))
        problem = check_attrs(attrs, MOD_ALLOWED, CFG_TEST, f"mod {name} declaration")
        if problem:
            return problem
        last = declared_problem(root, crate, par, depth + 1)
        if last is None:
            return None
    return last or f"{'/'.join([crate, 'src', *parts])} is not declared by a mod chain from lib.rs or main.rs"


def location_problem(root, rel):
    parts = rel.split("/")
    if parts[0] not in CRATES:
        return f"unknown crate directory {parts[0]!r}; citations must be {LOCATIONS}"
    if len(parts) == 3 and parts[1] == "tests" and parts[2].endswith(".rs"):
        return None
    if len(parts) >= 3 and parts[1] == "src" and parts[-1].endswith(".rs"):
        return declared_problem(root, parts[0], parts[2:])
    return f"{rel} is not compiled by cargo test; citations must be {LOCATIONS}"


def resolve(root, rel, label, errors, prefix=None):
    p = pathlib.PurePosixPath(rel)
    if p.is_absolute():
        errors.append(f"{label}: absolute path not allowed: {rel}")
        return None
    if ".." in p.parts:
        errors.append(f"{label}: '..' not allowed: {rel}")
        return None
    if prefix is not None and not rel.startswith(prefix):
        errors.append(f"{label}: must be under {prefix}: {rel}")
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
    real_rel = real.relative_to(base).as_posix()
    if prefix is None:
        problem = location_problem(root, rel)
        if problem:
            errors.append(f"{label}: {problem}")
            return None
        if real_rel != rel and location_problem(root, real_rel):
            errors.append(f"{label}: resolves outside the allowed test locations: {rel}")
            return None
    elif not real.is_relative_to((base / prefix).resolve()):
        errors.append(f"{label}: resolves outside {prefix}: {rel}")
        return None
    return full


def check_covered_by(root, inv, errors, targets, extra_features=()):
    label = inv["id"]
    done = inv["status"] in DONE
    if done and not inv["covered_by"]:
        errors.append(f"{label}: {inv['status']} without covered_by")
    for entry in inv["covered_by"]:
        path, sep, name = entry.partition("::")
        full = resolve(root, path, label, errors)
        if full is None:
            continue
        if not path.endswith(".rs"):
            errors.append(f"{label}: covered_by must name a .rs file: {path}")
            continue
        if not done:
            continue
        if not sep or not name.split("::")[-1]:
            errors.append(f"{label}: {inv['status']} entries must be path::name: {entry}")
            continue
        problem, features = locate_test(full.read_text(errors="replace"), name)
        if problem:
            errors.append(f"{label}: {path}: {problem}")
        else:
            targets.append((label, path, name, sorted(set(features) | set(extra_features))))


def check_proof(root, inv, errors, proofs):
    label = inv["id"]
    if inv["status"] != "proven":
        return
    if inv["provable"] == "none":
        errors.append(f"{label}: proven but provable is none")
        return
    proof = inv.get("proof")
    if not isinstance(proof, str) or not proof:
        errors.append(f"{label}: proven without a proof artifact path")
        return
    if (root / "tests/proofs").is_symlink():
        errors.append(f"{label}: tests/proofs is a symlink")
        return
    full = resolve(root, proof, f"{label}: proof", errors, prefix="tests/proofs/")
    if full is None:
        return
    want = ".rs" if inv["provable"] == "kani" else ".lean"
    if pathlib.PurePosixPath(proof).suffix != want or full.resolve().suffix != want:
        errors.append(f"{label}: proof suffix must be {want} for provable = {inv['provable']}: {proof}")
        return
    if proof in proofs:
        errors.append(f"{label}: proof file shared with {proofs[proof]}: {proof}")
        return
    proofs[proof] = label
    text = full.read_text(errors="replace")
    if not text.strip():
        errors.append(f"{label}: proof file is empty: {proof}")
        return
    if want == ".rs":
        problem = proof_harness_problem(text)
        if problem:
            errors.append(f"{label}: {proof}: {problem}")
        return
    code = re.sub(r"--[^\n]*", "", re.sub(r"/-.*?-/", "", text, flags=re.S))
    for word in ("sorry", "axiom", "admit"):
        if re.search(rf"\b{word}\b", code):
            errors.append(f"{label}: {proof} contains {word}")
            return
    if not re.search(r"\b(?:theorem|lemma)\b", code):
        errors.append(f"{label}: {proof} has no theorem or lemma")


def normalise_id(value):
    return unicodedata.normalize("NFC", value).casefold().strip()


def check_entry(root, inv, seen, errors, targets, proofs):
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
    key = normalise_id(inv["id"])
    if key in seen:
        errors.append(f"{label}: duplicate id")
    seen.add(key)
    for field, allowed in (("status", STATUSES), ("surface", SURFACES), ("provable", PROVABLE)):
        if inv[field] not in allowed:
            errors.append(f"{label}: {field} must be one of {sorted(allowed)}, got {inv[field]!r}")
            return
    cb = inv["covered_by"]
    if not isinstance(cb, list) or not all(isinstance(x, str) for x in cb):
        errors.append(f"{label}: covered_by must be a list of strings")
        return
    for field in ("proof_blocker", "proof"):
        if field in inv and not isinstance(inv[field], str):
            errors.append(f"{label}: {field} must be a string")
            return
    feats = inv.get("features", [])
    if not isinstance(feats, list) or not all(isinstance(x, str) and re.fullmatch(r"[\w-]+", x) for x in feats):
        errors.append(f"{label}: features must be a list of cargo feature names")
        return
    check_covered_by(root, inv, errors, targets, feats)
    check_proof(root, inv, errors, proofs)


def cargo_args(root, path, features):
    parts = path.split("/")
    base = ["test", "-p", parts[0]]
    if parts[1] == "tests":
        base += ["--test", parts[2][:-3]]
    elif (root / parts[0] / "src" / "lib.rs").exists():
        base += ["--lib"]
    else:
        base += ["--bins"]
    if features:
        base += ["--features", ",".join(features)]
    return base


def run_cargo(root, args, env):
    timeout = int(os.environ.get("GATE_CARGO_TIMEOUT", "3600"))
    return subprocess.run(["cargo", *args], cwd=root, env=env, capture_output=True, text=True, timeout=timeout)


def run_targets(root, targets):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    errors = []
    done = set()
    for label, path, name, features in targets:
        if (path, name) in done:
            continue
        done.add((path, name))
        *module_path, last = name.split("::")
        base = cargo_args(root, path, features)
        try:
            listed = run_cargo(root, base + ["--", "--list"], env)
            hits = []
            for line in listed.stdout.splitlines():
                m = re.fullmatch(r"(\S+): test", line.strip())
                if not m:
                    continue
                segs = m.group(1).split("::")
                if segs[-1] != last:
                    continue
                if module_path and segs[len(segs) - 1 - len(module_path):-1] != module_path:
                    continue
                hits.append(m.group(1))
            if listed.returncode != 0:
                errors.append(f"{label}: cargo test --list failed for {path}::{name}")
                continue
            if len(hits) != 1:
                errors.append(f"{label}: collected {len(hits)} tests for {path}::{name}, expected exactly 1")
                continue
            ran = run_cargo(root, base + ["--", "--exact", hits[0]], env)
        except FileNotFoundError:
            errors.append("cargo not found on PATH")
            return errors
        except subprocess.TimeoutExpired:
            errors.append(f"{label}: cargo timed out for {path}::{name}")
            continue
        if ran.returncode != 0 or not re.search(r"test result: ok\. 1 passed;", ran.stdout):
            errors.append(f"{label}: {path}::{name} did not pass exactly one test")
    return errors


def parse_args(argv):
    if not argv or argv[0] not in ("ledger", "coverage"):
        return None
    run = False
    root = None
    for arg in argv[1:]:
        if arg == "--run":
            run = True
        elif arg.startswith("--") or root is not None:
            return None
        else:
            root = arg
    return argv[0], run, root


def main(argv):
    parsed = parse_args(argv)
    if parsed is None:
        print(USAGE, file=sys.stderr)
        return 2
    phase, run, root_arg = parsed
    root = pathlib.Path(root_arg) if root_arg else pathlib.Path(__file__).resolve().parents[2]
    if run and "CARGO_TARGET_DIR" not in os.environ:
        print("--run needs CARGO_TARGET_DIR in the environment", file=sys.stderr)
        return 2
    ledger_path = root / "tests/invariants.toml"
    try:
        data = tomllib.loads(ledger_path.read_text())
    except UnicodeDecodeError:
        print(f"{ledger_path} is not valid UTF-8")
        return 1
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
    targets = []
    proofs = {}
    for inv in ledger:
        check_entry(root, inv, seen, errors, targets, proofs)
    if phase == "coverage":
        for inv in ledger:
            if isinstance(inv, dict) and inv.get("surface") == "interface" and inv.get("status") not in DONE:
                errors.append(f"{inv.get('id', '?')}: interface invariant not covered ({inv.get('status')})")
    if run and not errors:
        errors += run_targets(root, targets)
    print("\n".join(errors))
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
