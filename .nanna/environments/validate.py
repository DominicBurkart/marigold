import json
import pathlib
import platform
import re
import shutil
import subprocess
import sys
import tomllib

FORBIDDEN_MARKER = re.compile(
    r"\b(lean|aeneas|charon|elan|lake|mathlib|batteries|fstar|hol4)", re.IGNORECASE
)
REQUIRED_LEAN_MARKERS = ("lean", "aeneas")
FLAKE_OUTPUT = re.compile(r"^devContainerImage-[a-z0-9][a-z0-9-]*$")
FLAKE_PREFIX = "devContainerImage-"
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_'-]*")
BINDING = re.compile(r"(?<![\w.'-])([A-Za-z_][\w'-]*)\s*=(?!=)")
ENVIRONMENTS_OPEN = re.compile(r"\benvironments\s*=\s*\{")
ENTRY_NAME = re.compile(r"([A-Za-z0-9_][A-Za-z0-9_-]*)\s*=$")
ASSIGN = re.compile(r"(?<![=!<>])=(?!=)")
DYNAMIC = re.compile(
    r"\.\s*\"|\.\s*\$\{(?!system\})|\bgetAttr\b|\battrByPath\b|\bgetFlake\b"
)
ALWAYS = (
    "environmentSpecs",
    "environmentImages",
    "environmentManifest",
    "mkImage",
    "packageName",
)
LEAN_ENV = "lean"
CAPABILITY_KEYS = {"id", "description", "exec"}
TOP_KEYS = {"schema", "name", "build", "capability"}
BUILD_KEYS = {"flake_output"}
MANIFEST_ATTR = "environmentManifest"
NIX_TIMEOUT = 600


class FlakeError(Exception):
    pass


def load(path):
    with open(path, "rb") as handle:
        return tomllib.load(handle)


def blank(text, spans):
    chars = list(text)
    for start, end in spans:
        for i in range(start, end):
            if chars[i] != "\n":
                chars[i] = " "
    return "".join(chars)


def scan(text):
    comments = []
    strings = []
    i = 0
    n = len(text)
    while i < n:
        two = text[i:i + 2]
        if text[i] == "#":
            j = text.find("\n", i)
            j = n if j < 0 else j
            comments.append((i, j))
            i = j
        elif two == "/*":
            j = text.find("*/", i + 2)
            j = n if j < 0 else j + 2
            comments.append((i, j))
            i = j
        elif two == "''":
            j = i + 2
            while j < n and text[j:j + 2] != "''":
                j += 3 if text[j:j + 3] in ("'''", "''$") else 1
            strings.append((i + 2, min(j, n)))
            i = min(j + 2, n)
        elif text[i] == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            strings.append((i + 1, min(j, n)))
            i = min(j + 1, n)
        else:
            i += 1
    return comments, strings


def depths(text):
    result = []
    depth = 0
    for ch in text:
        if ch in "{[(":
            depth += 1
        result.append(depth)
        if ch in "}])":
            depth -= 1
    return result


def parse_flake_text(text):
    comments, _ = scan(text)
    uncommented = blank(text, comments)
    _, strings = scan(uncommented)
    blanked = blank(uncommented, strings)
    opening = ENVIRONMENTS_OPEN.search(blanked)
    if opening is None:
        raise FlakeError("no `environments = {` attrset found")
    depth = depths(blanked)
    base = depth[opening.start()]
    inner = base + 1
    block_start = opening.end()
    block_end = block_start
    while block_end < len(blanked) and depth[block_end] >= inner:
        block_end += 1

    entries = {}
    for eq in ASSIGN.finditer(blanked, block_start, block_end):
        at = eq.start()
        if depth[at] != inner:
            continue
        head = ENTRY_NAME.search(blanked[block_start:at + 1])
        if head is None:
            raise FlakeError("cannot parse an environment name inside `environments`")
        name = head.group(1)
        after = at + 1
        while after < len(blanked) and blanked[after].isspace():
            after += 1
        if after >= len(blanked) or blanked[after] != "{":
            raise FlakeError(f"environment {name} is not an attrset literal")
        close = after + 1
        while close < len(blanked) and depth[close] > inner:
            close += 1
        entries[name] = (after, close)
    if not entries:
        raise FlakeError("`environments` attrset has no entries")

    bindings = {}
    for match in BINDING.finditer(blanked):
        if depth[match.start()] != base:
            continue
        if opening.start() <= match.start() < block_end:
            continue
        end = match.end()
        while end < len(blanked) and not (blanked[end] == ";" and depth[end] == base):
            end += 1
        bindings.setdefault(match.group(1), []).append((match.end(), end))
    return uncommented, blanked, entries, bindings


def reachable_texts(span, blanked, uncommented, bindings):
    queue = [span]
    seen_names = set(ALWAYS)
    for name in ALWAYS:
        queue.extend(bindings.get(name, []))
    seen_spans = set()
    blank_chunks = []
    raw_chunks = []
    while queue:
        start, end = queue.pop()
        if (start, end) in seen_spans:
            continue
        seen_spans.add((start, end))
        blank_chunks.append(blanked[start:end])
        raw_chunks.append(uncommented[start:end])
        for ident in IDENT.findall(blanked[start:end]):
            if ident in seen_names or ident not in bindings:
                continue
            seen_names.add(ident)
            queue.extend(bindings[ident])
    return "\n".join(blank_chunks), "\n".join(raw_chunks)


def text_manifest(text):
    uncommented, blanked, entries, bindings = parse_flake_text(text)
    manifest = {}
    dynamic = {}
    for name, span in entries.items():
        blank_text, raw_text = reachable_texts(span, blanked, uncommented, bindings)
        manifest[name] = [blank_text]
        dynamic[name] = DYNAMIC.search(raw_text) is not None
    return manifest, dynamic


def nix_system():
    machine = platform.machine().lower()
    arch = {"amd64": "x86_64", "arm64": "aarch64"}.get(machine, machine)
    return f"{arch}-linux"


def nix_manifest(nix, root):
    ref = f"path:{root}#lib.{nix_system()}.{MANIFEST_ATTR}"
    try:
        done = subprocess.run(
            [nix, "eval", "--json", ref],
            capture_output=True,
            text=True,
            timeout=NIX_TIMEOUT,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise FlakeError(f"nix eval of {ref} could not run: {exc}")
    if done.returncode != 0:
        raise FlakeError(f"nix eval of {ref} failed: {done.stderr.strip()[-400:]}")
    try:
        data = json.loads(done.stdout)
    except json.JSONDecodeError as exc:
        raise FlakeError(f"nix eval of {ref} returned invalid JSON: {exc}")
    shape_ok = isinstance(data, dict) and all(
        isinstance(v, list) and all(isinstance(x, str) for x in v) for v in data.values()
    )
    if not shape_ok:
        raise FlakeError(
            f"{MANIFEST_ATTR} must map environment names to lists of package names"
        )
    return data


def flake_manifest(root):
    nix = shutil.which("nix")
    if nix:
        return nix_manifest(nix, root), {}
    print(
        "warning: nix not found, using the text fallback for flake.nix "
        "(weaker than evaluating the manifest)",
        file=sys.stderr,
    )
    try:
        return text_manifest((root / "flake.nix").read_text())
    except FlakeError as exc:
        raise FlakeError(f"flake.nix: text fallback cannot parse the flake: {exc}")


def check_flake(root, envs, outputs_wanted):
    errors = []
    if not (root / "flake.nix").is_file():
        return ["flake.nix: not found"]
    try:
        manifest, dynamic = flake_manifest(root)
    except FlakeError as exc:
        return [str(exc)]
    for name, output in sorted(outputs_wanted.items()):
        if name not in manifest:
            errors.append(f"{name}: {output} is not defined in flake.nix")
    for name, packages in sorted(manifest.items()):
        if name not in envs:
            continue
        joined = " ".join(packages)
        if name == LEAN_ENV:
            for marker in REQUIRED_LEAN_MARKERS:
                if not re.search(rf"\b{marker}", joined, re.IGNORECASE):
                    errors.append(f"{name}: environment must contain {marker}")
            continue
        if FORBIDDEN_MARKER.search(joined):
            errors.append(
                f"{name}: lean toolchain must only be built by the {LEAN_ENV} environment"
            )
        if dynamic.get(name):
            errors.append(
                f"{name}: dynamic attribute access cannot be analysed textually, "
                "install nix to evaluate the manifest"
            )
    return errors


def validate(root):
    root = pathlib.Path(root)
    envs_dir = root / ".nanna/environments"
    errors = []
    envs = {}
    outputs_wanted = {}

    for env_dir in sorted(p for p in envs_dir.iterdir() if p.is_dir() and not p.name.startswith("_")):
        name = env_dir.name
        declaration = env_dir / "environment.toml"
        if not declaration.is_file():
            errors.append(f"{name}: missing environment.toml")
            continue
        data = load(declaration)
        if data.get("schema") != 1:
            errors.append(f"{name}: schema must be 1")
        if data.get("name") != name:
            errors.append(f"{name}: name must equal the directory name")
        extra_top = set(data) - TOP_KEYS
        if extra_top:
            errors.append(f"{name}: unsupported keys {sorted(extra_top)}")
        build = data.get("build", {})
        if "containerfile" in build:
            errors.append(f"{name}: containerfile is no longer supported, use [build] flake_output")
        extra_build = set(build) - BUILD_KEYS - {"containerfile"}
        if extra_build:
            errors.append(f"{name}: unsupported [build] keys {sorted(extra_build)}")
        output = build.get("flake_output")
        if output is None:
            errors.append(f"{name}: [build] flake_output is required")
        elif not isinstance(output, str) or not FLAKE_OUTPUT.match(output):
            errors.append(f"{name}: flake_output must match {FLAKE_OUTPUT.pattern}")
        elif output != FLAKE_PREFIX + name:
            errors.append(f"{name}: flake_output must equal {FLAKE_PREFIX}{name}")
        else:
            outputs_wanted[name] = output
        ids = [c["id"] for c in data.get("capability", [])]
        if len(ids) != len(set(ids)):
            errors.append(f"{name}: duplicate capability ids")
        for cap in data.get("capability", []):
            extra = set(cap) - CAPABILITY_KEYS
            if extra:
                errors.append(f"{name}: capability {cap.get('id')} has unsupported keys {sorted(extra)}")
        envs[name] = set(ids)

    errors.extend(check_flake(root, envs, outputs_wanted))

    cards = {}
    for card_file in sorted((root / ".nanna/catalog").glob("*.toml")):
        data = load(card_file)
        cards[data["identity"]["name"]] = set(data["scope"]["tools"])

    container_ids = set().union(*envs.values()) if envs else set()
    mapping = load(envs_dir / "cards.toml")["map"]
    for card in cards:
        if card not in mapping:
            errors.append(f"{card}: unmapped card")
    for card, env in mapping.items():
        if card not in cards:
            errors.append(f"{card}: mapped but no such card")
            continue
        if env not in envs:
            errors.append(f"{card}: unknown environment {env}")
            continue
        wanted = {t for t in cards[card] if t.startswith("cargo_") or t in container_ids}
        provided = envs[env]
        for tool in sorted(wanted - provided):
            errors.append(f"{card}: tool {tool} is not provided by environment {env}")
        for tool in sorted(provided - wanted):
            errors.append(f"{card}: environment {env} provides {tool} which the card does not allow")

    lean_users = [c for c, e in mapping.items() if e == LEAN_ENV]
    if len(lean_users) > 1:
        errors.append(f"{LEAN_ENV}: reserved for one card, mapped by {sorted(lean_users)}")
    return errors


def main():
    root = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else pathlib.Path(".")
    errors = validate(root)
    for error in errors:
        print(error)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
