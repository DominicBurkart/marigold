import pathlib
import re
import sys
import tomllib

LEAN_MARKER = re.compile(r"\b(lean|aeneas|charon|elan|lake)", re.IGNORECASE)
FLAKE_OUTPUT = re.compile(r"^devContainerImage-[a-z0-9][a-z0-9-]*$")
FLAKE_PREFIX = "devContainerImage-"
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_'-]*")
BINDING = re.compile(r"^ {8}([A-Za-z_][A-Za-z0-9_'-]*) = ")
ENVIRONMENTS_OPEN = re.compile(r"^\s*environments = \{\s*$")
ENTRY_OPEN = re.compile(r"^\s*([A-Za-z0-9_-]+) = \{\s*$")
SKIPPED_BINDINGS = {"environments", "environmentImages"}
LEAN_ENV = "lean"
CAPABILITY_KEYS = {"id", "description", "exec"}


def load(path):
    with open(path, "rb") as handle:
        return tomllib.load(handle)


def flake_environments(text):
    lines = text.splitlines()
    start = next((i for i, l in enumerate(lines) if ENVIRONMENTS_OPEN.match(l)), None)
    if start is None:
        return {}
    result = {}
    depth = 1
    current = None
    for line in lines[start + 1:]:
        if depth == 1:
            match = ENTRY_OPEN.match(line)
            if match:
                current = match.group(1)
                result[current] = []
        elif current is not None:
            result[current].append(line)
        depth += line.count("{") - line.count("}")
        if depth <= 0:
            break
        if depth == 1:
            current = None
    return {name: "\n".join(body) for name, body in result.items()}


def flake_bindings(text):
    bindings = {}
    current = None
    for line in text.splitlines():
        match = BINDING.match(line)
        if match:
            current = match.group(1)
            bindings[current] = []
        if current is not None:
            bindings[current].append(line)
    return {k: "\n".join(v) for k, v in bindings.items() if k not in SKIPPED_BINDINGS}


def reachable_text(body, bindings):
    seen = set()
    chunks = [body]
    queue = list(IDENT.findall(body))
    while queue:
        ident = queue.pop()
        if ident in seen or ident not in bindings:
            continue
        seen.add(ident)
        chunks.append(bindings[ident])
        queue.extend(IDENT.findall(bindings[ident]))
    return "\n".join(chunks)


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
        build = data.get("build", {})
        if "containerfile" in build:
            errors.append(f"{name}: containerfile is no longer supported, use [build] flake_output")
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
        available = set(ids)
        envs[name] = available

    flake = root / "flake.nix"
    if not flake.is_file():
        errors.append("flake.nix: not found")
    else:
        text = flake.read_text()
        defined = flake_environments(text)
        bindings = flake_bindings(text)
        for name, output in sorted(outputs_wanted.items()):
            if name not in defined:
                errors.append(f"{name}: {output} is not defined in flake.nix")
        for name, body in sorted(defined.items()):
            if name != LEAN_ENV and name in envs and LEAN_MARKER.search(reachable_text(body, bindings)):
                errors.append(f"{name}: lean toolchain must only be built by the {LEAN_ENV} environment")

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
