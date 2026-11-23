import os
import pathlib
import re
import signal
import subprocess
import sys
import tempfile
import time
import json
import tomllib
import types
import unicodedata

USAGE = "usage: gate.py (ledger|coverage) [--run] [ROOT]"
STATUSES = {"candidate", "covered", "proven", "disputed"}
SURFACES = {"interface", "internal"}
PROVABLE = {"none", "kani", "lean"}
DONE = {"covered", "proven"}
REQUIRED = ("id", "surface", "statement", "status", "provable", "covered_by")
CRATES = ("marigold", "marigold-impl", "marigold-grammar", "marigold-lsp", "marigold-macros", "tests")
TEST_ATTRS = {"test", "tokio::test", "async_std::test", "rstest::rstest"}
LINTS = {"allow", "warn", "deny", "forbid", "expect", "doc"}
FN_ALLOWED = TEST_ATTRS | LINTS | {"should_panic", "cfg"}
MOD_ALLOWED = LINTS | {"cfg"}
PROOF_FN_ALLOWED = LINTS | {"kani::proof", "kani::unwind", "kani::solver", "cfg"}
PROPTEST_INNER = {"proptest_config"}
LOCATIONS = "<crate>/tests/*.rs or a <crate>/src file declared by mod"
SECRET_WORDS = ("TOKEN", "SECRET", "KEY", "PASS", "AUTH", "CREDENTIAL")
SECRET_NAMES = ("DATABASE_URL",)
DEFAULT_TIMEOUT = 900

SPECIAL = re.compile(r"""//|/\*|"|'|(?<![\w])(?:(?:br|cr|r)#*"|[bc]"|b')""")
STR_RE = re.compile(r'"(?:[^"\\]|\\.)*"?', re.S)
CHAR_RE = re.compile(r"'(?:\\.[^'\n]{0,10}|[^\\'\n])'")
FN_RE = re.compile(r"\bfn\s+(\w+)")
DECL_RE = re.compile(r"\bmod\s+(\w+)\s*;")
INNER_RE = re.compile(r"#\s*!\s*\[")
OUTER_START = re.compile(r"\s*#\s*\[")
INNER_START = re.compile(r"\s*#\s*!\s*\[")
QUAL_TAIL = re.compile(r"(?<![\w])(?:(?:pub(?:\([^)]*\))?|async|unsafe|const|extern)\b\s*)+\Z")
MOD_HEADER = re.compile(r"(?:pub(?:\([^)]*\))?\s+)?mod\s+(\w+)")
FN_HEADER = re.compile(r"(?:^|\W)fn\s+\w+")
PROPTEST_HEAD = re.compile(r"(?:proptest::)?proptest\s*!")
FEATURE_NAME = re.compile(r'feature\s*=\s*"([^"]+)"')
FEATURE_OK = re.compile(r"[A-Za-z0-9_][A-Za-z0-9_-]*")
ASSERT_EQ = re.compile(r"\bassert_(?:eq|ne)\s*!\s*[(\[{]")
ASSERT_CALL = re.compile(r"(?:\bkani::assert|\bassert)(?P<bang>!?)\s*[(\[{]\s*(?P<cond>[^,)\]}]*)")
ASSUME_CALL = re.compile(r"kani::assume\s*\(\s*(?P<cond>[^;]*?)\s*\)\s*;")
LEAN_FORBIDDEN = (
    ("sorry", r"\bsorry\b"),
    ("sorryAx", r"\bsorryAx\b"),
    ("stop", r"\bstop\b"),
    ("axiom", r"\baxiom\b"),
    ("admit", r"\badmit\b"),
    ("native_decide", r"\bnative_decide\b"),
    ("ofReduceBool", r"\bofReduceBool\b"),
    ("reduceBool", r"\breduceBool\b"),
    ("unsafe", r"\bunsafe\b"),
    ("implemented_by", r"\bimplemented_by\b"),
    ("macro_rules", r"\bmacro_rules\b"),
    ("macro", r"\bmacro\b"),
    ("elab", r"\belab\b"),
    ("syntax", r"\bsyntax\b"),
    ("run_cmd", r"\brun_cmd\b"),
    ("import Lean", r"\bimport\s+Lean\b"),
)


def cfg_regex(extra=(), include_test=True):
    atoms = (["test"] if include_test else []) + [*extra, r'feature\s*=\s*"(?:tokio|async-std)"']
    atom = "(?:" + "|".join(atoms) + ")"
    anyp = rf"any\s*\(\s*{atom}(?:\s*,\s*{atom})*\s*,?\s*\)"
    return re.compile(rf"cfg\s*\(\s*(?:{atom}|{anyp})\s*\)")


CFG_TEST = cfg_regex()
CFG_PROOF = cfg_regex(("kani",), include_test=False)


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


def strip_lean(text):
    out = []
    n = len(text)
    i = 0
    while i < n:
        if text.startswith("/-", i):
            depth = 1
            j = i + 2
            while j < n and depth:
                if text.startswith("/-", j):
                    depth += 1
                    j += 2
                elif text.startswith("-/", j):
                    depth -= 1
                    j += 2
                else:
                    j += 1
            i = j
            out.append(" ")
        elif text.startswith("--", i):
            j = text.find("\n", i)
            i = n if j == -1 else j
        elif text[i] == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            i = min(j + 1, n)
            out.append(" ")
        elif text[i] == "'":
            cm = CHAR_RE.match(text, i)
            if cm:
                i = cm.end()
                out.append(" ")
            else:
                out.append("'")
                i += 1
        else:
            out.append(text[i])
            i += 1
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
        self.stripped = s = strip_rust(text)
        n = len(s)
        consumed = set()
        self.file_inner, _ = leading_attributes(s, text, 0, n, True, consumed)
        fn_marks = {m.start(): m.group(1) for m in FN_RE.finditer(s)}
        decl_marks = {m.start(): m.group(1) for m in DECL_RE.finditer(s)}
        self.found = []
        self.mod_decls = []
        stack = []
        hs = 0

        def visible():
            return tuple(c for c in stack if c["kind"] != "paren")

        for i, ch in enumerate(s):
            if ch == "f":
                name = fn_marks.get(i)
                if name is not None:
                    self.found.append((i, name, visible()))
            elif ch == "m":
                name = decl_marks.get(i)
                if name is not None:
                    self.mod_decls.append((i, name, visible()))
            elif ch in "([":
                end = rstrip_index(s, i)
                if end >= 2 and s[end - 1] == "!" and (s[end - 2].isalnum() or s[end - 2] == "_"):
                    attrs, after = leading_attributes(s, text, hs, i, False, None)
                    kind = "proptest" if PROPTEST_HEAD.fullmatch(s[after:end].strip()) else "macro"
                    inner, _ = leading_attributes(s, text, i + 1, n, True, consumed)
                    stack.append({"kind": kind, "name": "", "attrs": attrs, "inner": inner, "delim": ch})
                    hs = i + 1
                else:
                    stack.append({"kind": "paren", "delim": ch})
            elif ch in ")]":
                if stack and stack[-1]["delim"] in "([":
                    stack.pop()
            elif ch == "{":
                attrs, after = leading_attributes(s, text, hs, i, False, None)
                kind, name = classify(s[after:i])
                inner, _ = leading_attributes(s, text, i + 1, n, True, consumed)
                stack.append({"kind": kind, "name": name, "attrs": attrs, "inner": inner, "delim": "{"})
                hs = i + 1
            elif ch == "}":
                while stack and stack[-1]["delim"] in "([":
                    stack.pop()
                if stack:
                    stack.pop()
                hs = i + 1
            elif ch == ";" and not (stack and stack[-1]["delim"] in "(["):
                hs = i + 1
        self.global_problem = None
        for m in INNER_RE.finditer(s):
            if m.end() - 1 not in consumed:
                self.global_problem = "inner attribute #![...] outside the head of the file or of a body"
                break


_ANALYSES = {}


def analyze_file(path):
    key = str(path)
    if key not in _ANALYSES:
        _ANALYSES[key] = Analysis(pathlib.Path(path).read_text(errors="replace"))
    return _ANALYSES[key]


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
    for ctx in stack:
        kind = ctx["kind"]
        if kind == "fn":
            return f"{label} is nested inside another fn", features
        if kind == "macro":
            return f"{label} is defined inside a macro", features
        if kind == "other":
            return f"{label} is inside an unsupported block", features
        if kind == "proptest":
            problem = check_attrs(ctx["attrs"], MOD_ALLOWED, cfg_re, "proptest!")
            problem = problem or check_attrs(ctx["inner"], PROPTEST_INNER, cfg_re, "proptest!")
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
        return a.global_problem, [], []
    candidates = []
    for pos, fname, stack in a.found:
        if fname != name:
            continue
        chain = [c["name"] for c in stack if c["kind"] == "mod"]
        if module_path and chain[len(chain) - len(module_path):] != module_path:
            continue
        candidates.append((pos, stack, chain))
    if not candidates:
        return f"no fn {entry_name} (outside comments and strings)", [], []
    if len(candidates) > 1:
        return f"ambiguous: fn {name} is defined {len(candidates)} times; cite its module path", [], []
    pos, stack, chain = candidates[0]
    label = f"fn {name}"
    problem = check_attrs(a.file_inner, MOD_ALLOWED, CFG_TEST, "the file")
    if problem:
        return problem, [], []
    features = features_of(a.file_inner)
    problem, more = check_contexts(stack, label, CFG_TEST)
    if problem:
        return problem, [], []
    features += more
    attrs = attributes_before(a.stripped, text, pos)
    problem = check_attrs(attrs, FN_ALLOWED, CFG_TEST, label)
    if problem:
        return problem, [], []
    features += features_of(attrs)
    if not any(attr_name(x) in TEST_ATTRS for x in attrs):
        return f"{label} is not a test", [], []
    return None, sorted(set(features)), chain


def harness_body(stripped, pos):
    j = stripped.find("{", pos)
    if j == -1:
        return None
    close = matching(stripped, j, "{", "}", len(stripped))
    return stripped[j:close] if close != -1 else stripped[j:]


def meaningful_assertion(body):
    if ASSERT_EQ.search(body):
        return True
    return any(m.group("cond").strip() not in ("true", "") for m in ASSERT_CALL.finditer(body))


def vacuous_assume(body):
    for m in ASSUME_CALL.finditer(body):
        cond = m.group("cond").strip()
        if cond in ("false", "!true"):
            return cond
        eq = re.fullmatch(r"(\d+)\s*(==|!=)\s*(\d+)", cond)
        if eq and ((eq.group(2) == "==") != (eq.group(1) == eq.group(3))):
            return cond
    return None


def proof_harness_problem(text):
    a = Analysis(text)
    if a.global_problem:
        return a.global_problem
    if check_attrs(a.file_inner, MOD_ALLOWED, CFG_PROOF, "the file"):
        return "no proof harness: file-level attribute not supported"
    last = "no #[kani::proof] fn outside macros, nested fns and unsupported cfg"
    for pos, name, stack in a.found:
        attrs = attributes_before(a.stripped, text, pos)
        if not any(attr_name(x) == "kani::proof" for x in attrs):
            continue
        problem, _ = check_contexts(stack, f"fn {name}", CFG_PROOF)
        problem = problem or check_attrs(attrs, PROOF_FN_ALLOWED, CFG_PROOF, f"fn {name}")
        if problem:
            last = problem
            continue
        body = harness_body(a.stripped, pos)
        if body is None:
            last = f"fn {name} has no body"
            continue
        vacuous = vacuous_assume(body)
        if vacuous is not None:
            last = f"fn {name} uses a vacuous kani::assume({vacuous})"
        elif not meaningful_assertion(body):
            last = f"fn {name} has no non-trivial assertion (kani::assert, assert!, assert_eq! or assert_ne!)"
        else:
            return None
    return f"no proof harness: {last}"


DEFAULT_ROOTS = (("lib.rs",), ("main.rs",))


def module_info(root, crate, parts, depth=0, roots=DEFAULT_ROOTS):
    roots = [list(r) for r in roots]
    if parts in roots:
        return [], [], None
    last = f"{'/'.join([crate, 'src', *parts])} is not declared by a mod chain from lib.rs or main.rs"
    if depth > 32:
        return [], [], "module nesting too deep"
    *dirs, fname = parts
    if fname == "mod.rs":
        name = dirs[-1]
        D = dirs[:-1]
    else:
        name = fname[:-3]
        D = dirs
    base = root / crate / "src"
    if base.joinpath(*D, name + ".rs").is_file() and base.joinpath(*D, name, "mod.rs").is_file():
        return [], [], f"both {name}.rs and {name}/mod.rs exist next to each other"
    for split in range(len(D), -1, -1):
        inline = D[split:]
        if split == 0:
            parents = roots
        else:
            parents = [D[:split - 1] + [D[split - 1] + ".rs"], D[:split] + ["mod.rs"]]
        for par in parents:
            f = base.joinpath(*par)
            if not f.is_file():
                continue
            a = analyze_file(f)
            if a.global_problem:
                last = a.global_problem
                continue
            for pos, dname, stack in a.mod_decls:
                if dname != name:
                    continue
                if any(c["kind"] != "mod" for c in stack):
                    last = f"mod {name} is declared inside a fn, macro or other block"
                    continue
                if [c["name"] for c in stack] != inline:
                    continue
                attrs = attributes_before(a.stripped, a.text, pos)
                problem = check_attrs(a.file_inner, MOD_ALLOWED, CFG_TEST, "the file")
                problem = problem or check_attrs(attrs, MOD_ALLOWED, CFG_TEST, f"mod {name} declaration")
                feats = []
                if not problem:
                    problem, feats = check_contexts(stack, f"mod {name}", CFG_TEST)
                if problem:
                    last = problem
                    continue
                if par in roots:
                    pm, pf, pp = [], [], None
                else:
                    pm, pf, pp = module_info(root, crate, par, depth + 1, roots)
                if pp:
                    last = pp
                    continue
                gathered = pf + features_of(a.file_inner) + feats + features_of(attrs)
                return pm + inline + [name], gathered, None
    return [], [], last


def location_info(root, rel):
    parts = rel.split("/")
    if parts[0] not in CRATES:
        return f"unknown crate directory {parts[0]!r}; citations must be {LOCATIONS}", [], []
    if len(parts) == 3 and parts[1] == "tests" and parts[2].endswith(".rs"):
        return None, [], []
    if len(parts) >= 3 and parts[1] == "src" and parts[-1].endswith(".rs"):
        modpath, feats, problem = module_info(root, parts[0], parts[2:])
        return problem, modpath, feats
    return f"{rel} is not compiled by cargo test; citations must be {LOCATIONS}", [], []


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
        problem = location_info(root, rel)[0]
        if problem:
            errors.append(f"{label}: {problem}")
            return None
        if real_rel != rel and location_info(root, real_rel)[0]:
            errors.append(f"{label}: resolves outside the allowed test locations: {rel}")
            return None
    elif not real.is_relative_to((base / prefix).resolve()):
        errors.append(f"{label}: resolves outside {prefix}: {rel}")
        return None
    return full


def check_covered_by(root, inv, errors, targets, features):
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
        problem, found, chain = locate_test(full.read_text(errors="replace"), name)
        if problem:
            errors.append(f"{label}: {path}: {problem}")
            continue
        _, modpath, decl_features = location_info(root, path)
        targets.append(
            {
                "label": label,
                "path": path,
                "tail": chain + [name.split("::")[-1]],
                "modpath": modpath,
                "features": sorted(set(found) | set(features.get(path.split("/")[0], []))),
                "decl_features": decl_features,
            }
        )


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
    code = strip_lean(text)
    for word, pattern in LEAN_FORBIDDEN:
        if re.search(pattern, code):
            errors.append(f"{label}: {proof} contains {word}")
            return
    if not re.search(r"\b(?:theorem|lemma)\b", code):
        errors.append(f"{label}: {proof} has no theorem or lemma")


def normalise_id(value):
    return unicodedata.normalize("NFC", value).casefold().strip()


def parse_features(inv, label, errors):
    raw = inv.get("features", [])

    def valid(names):
        return isinstance(names, list) and all(isinstance(x, str) and FEATURE_OK.fullmatch(x) for x in names)

    message = f"{label}: features must be a list of cargo feature names or a table of crate = [names]"
    if isinstance(raw, dict):
        if not all(k in CRATES and valid(v) for k, v in raw.items()):
            errors.append(message)
            return None
        return raw
    if not valid(raw):
        errors.append(message)
        return None
    crates = {e.partition("::")[0].split("/")[0] for e in inv["covered_by"]}
    if raw and len(crates) > 1:
        errors.append(f"{label}: features list is ambiguous for an entry citing several crates; use a table")
        return None
    return {c: raw for c in crates}


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
    features = parse_features(inv, label, errors)
    if features is None:
        return
    check_covered_by(root, inv, errors, targets, features)
    check_proof(root, inv, errors, proofs)


def cargo_flags(root):
    flags = []
    if (root / "Cargo.lock").exists():
        flags.append("--locked")
    if os.environ.get("GATE_OFFLINE") == "1":
        flags.append("--offline")
    return flags


def cargo_args(root, crate, selector, features):
    base = ["test", *cargo_flags(root), "-p", crate, *selector]
    if features:
        base += ["--features", ",".join(features)]
    return base


def scrubbed_env():
    env = {}
    for k, v in os.environ.items():
        upper = k.upper()
        if k.startswith("GIT_") or k in SECRET_NAMES or any(w in upper for w in SECRET_WORDS):
            continue
        env[k] = v
    return env


def run_cargo(root, args, env, timeout):
    with tempfile.TemporaryFile() as out, tempfile.TemporaryFile() as err:
        proc = subprocess.Popen(
            ["cargo", *args],
            cwd=root,
            env=env,
            stdin=subprocess.DEVNULL,
            stdout=out,
            stderr=err,
            start_new_session=True,
        )
        try:
            deadline = time.monotonic() + timeout
            while os.waitid(os.P_PID, proc.pid, os.WEXITED | os.WNOHANG | os.WNOWAIT) is None:
                if time.monotonic() > deadline:
                    raise subprocess.TimeoutExpired(args, timeout)
                time.sleep(0.05)
        finally:
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except (ProcessLookupError, PermissionError):
                pass
            proc.wait()
        out.seek(0)
        err.seek(0)
        return types.SimpleNamespace(
            returncode=proc.returncode,
            stdout=out.read().decode(errors="replace"),
            stderr=err.read().decode(errors="replace"),
        )


LIB_KINDS = {"lib", "rlib", "dylib", "cdylib", "staticlib", "proc-macro"}


def package_targets(metadata, crate):
    for package in metadata.get("packages", []):
        if package.get("name") == crate:
            return package.get("targets", [])
    return None


def src_roots(root, crate, targets):
    base = (root / crate / "src").resolve()
    found = []
    for t in targets:
        kinds = set(t.get("kind", []))
        path = pathlib.Path(t.get("src_path", "")).resolve()
        if not path.is_relative_to(base):
            continue
        rel = list(path.relative_to(base).parts)
        if kinds & LIB_KINDS:
            found.append((rel, ["--lib"]))
        elif "bin" in kinds:
            found.append((rel, ["--bin", t["name"]]))
    return found


def select_target(root, crate, path, tail, base_modpath, targets):
    parts = path.split("/")
    if parts[1] == "tests":
        want = (root / path).resolve()
        for t in targets:
            if "test" in t.get("kind", []) and pathlib.Path(t.get("src_path", "")).resolve() == want:
                return ["--test", t["name"]], base_modpath, None, None
        return None, [], None, f"no cargo test target has {path} as its source"
    roots = src_roots(root, crate, targets)
    if not roots:
        return None, [], None, f"{crate} has no lib or bin root under src/"
    matches = []
    last = f"{path} is not declared by a mod chain from any lib or bin root"
    for rel, selector in roots:
        modpath, feats, problem = module_info(root, crate, parts[2:], 0, [rel])
        if problem:
            last = problem
        else:
            matches.append((selector, modpath, feats))
    if not matches:
        return None, [], None, last
    if len(matches) > 1:
        return None, [], None, f"ambiguous: {path} is declared from several roots ({', '.join(' '.join(m[0]) for m in matches)})"
    selector, modpath, feats = matches[0]
    return selector, modpath, feats, None


def run_targets(root, targets, timeout):
    env = scrubbed_env()
    errors = []
    if not targets:
        return errors
    try:
        meta = run_cargo(root, ["metadata", *cargo_flags(root), "--no-deps", "--format-version", "1"], env, timeout)
        try:
            metadata = json.loads(meta.stdout)
            if meta.returncode != 0 or not isinstance(metadata, dict):
                raise ValueError
        except ValueError:
            return ["cargo metadata failed or printed invalid JSON"]
    except FileNotFoundError:
        return ["cargo not found on PATH"]
    except subprocess.TimeoutExpired:
        return [f"cargo metadata timed out after {timeout}s (GATE_CARGO_TIMEOUT)"]
    except OSError as e:
        return [f"cannot run cargo: {e.strerror or e}"]
    done = set()
    for item in targets:
        label, path = item["label"], item["path"]
        crate = path.split("/")[0]
        crate_targets = package_targets(metadata, crate)
        if crate_targets is None:
            errors.append(f"{label}: cargo metadata has no package named {crate}")
            continue
        selector, modpath, decl_features, problem = select_target(
            root, crate, path, item["tail"], item["modpath"], crate_targets
        )
        if problem:
            errors.append(f"{label}: {problem}")
            continue
        features = sorted(set(item["features"]) | set(decl_features if decl_features is not None else item["decl_features"]))
        expected = "::".join(modpath + item["tail"])
        key = (path, expected, tuple(selector), tuple(features))
        if key in done:
            continue
        done.add(key)
        base = cargo_args(root, crate, selector, features)
        try:
            listed = run_cargo(root, base + ["--", "--list"], env, timeout)
            if listed.returncode != 0:
                errors.append(f"{label}: cargo test --list failed for {path}::{expected}")
                continue
            hits = []
            for line in listed.stdout.splitlines():
                m = re.fullmatch(r"(\S+): test", line.strip())
                if m and m.group(1) == expected:
                    hits.append(m.group(1))
            if len(hits) != 1:
                errors.append(f"{label}: collected {len(hits)} tests named {expected} from {path}, expected exactly 1")
                continue
            ran = run_cargo(root, base + ["--", "--exact", expected], env, timeout)
        except FileNotFoundError:
            errors.append("cargo not found on PATH")
            return errors
        except subprocess.TimeoutExpired:
            errors.append(f"{label}: cargo timed out after {timeout}s (GATE_CARGO_TIMEOUT) for {path}::{expected}")
            continue
        except OSError as e:
            errors.append(f"cannot run cargo: {e.strerror or e}")
            return errors
        if ran.returncode != 0 or not re.search(r"test result: ok\. 1 passed;", ran.stdout):
            errors.append(f"{label}: {path}::{expected} did not pass exactly one test")
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
    timeout = DEFAULT_TIMEOUT
    if run:
        if "CARGO_TARGET_DIR" not in os.environ:
            print("--run needs CARGO_TARGET_DIR in the environment", file=sys.stderr)
            return 2
        raw = os.environ.get("GATE_CARGO_TIMEOUT")
        if raw is not None:
            try:
                timeout = int(raw)
                if timeout <= 0:
                    raise ValueError
            except ValueError:
                print("GATE_CARGO_TIMEOUT must be a positive integer number of seconds", file=sys.stderr)
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
        errors += run_targets(root, targets, timeout)
    print("\n".join(errors))
    return 1 if errors else 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except KeyboardInterrupt:
        print("interrupted", file=sys.stderr)
        sys.exit(130)
