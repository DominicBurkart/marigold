import pathlib
import sys
import tomllib

LEAN_MARKERS = ("elan", "lake ", "aeneas", "charon")
LEAN_ENV = "lean"
CAPABILITY_KEYS = {"id", "description", "exec"}


def load(path):
    with open(path, "rb") as handle:
        return tomllib.load(handle)


def validate(root):
    root = pathlib.Path(root)
    envs_dir = root / ".nanna/environments"
    errors = []
    envs = {}

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
        containerfile = env_dir / data.get("build", {}).get("containerfile", "Containerfile")
        if not containerfile.is_file():
            errors.append(f"{name}: Containerfile not found")
            continue
        ids = [c["id"] for c in data.get("capability", [])]
        if len(ids) != len(set(ids)):
            errors.append(f"{name}: duplicate capability ids")
        for cap in data.get("capability", []):
            extra = set(cap) - CAPABILITY_KEYS
            if extra:
                errors.append(f"{name}: capability {cap.get('id')} has unsupported keys {sorted(extra)}")
        available = set(ids)
        text = containerfile.read_text()
        if name != LEAN_ENV and any(m in text for m in LEAN_MARKERS):
            errors.append(f"{name}: lean toolchain must only be built by the {LEAN_ENV} environment")
        envs[name] = available

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
