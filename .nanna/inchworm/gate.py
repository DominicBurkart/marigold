import sys, tomllib, pathlib

root = pathlib.Path(__file__).resolve().parents[2]
ledger = tomllib.loads((root / ".nanna/inchworm/invariants.toml").read_text())["invariant"]
phase = sys.argv[1]
interface = [i for i in ledger if i["surface"] == "interface"]
done = {"covered", "proven"}
errors = []
for i in ledger:
    if i["status"] in done:
        if not i["covered_by"]:
            errors.append(f'{i["id"]}: {i["status"]} without covered_by')
        for p in i["covered_by"]:
            if not (root / p.split("::")[0]).exists():
                errors.append(f'{i["id"]}: covered_by path missing: {p}')
if phase == "coverage":
    errors += [f'{i["id"]}: interface invariant not covered ({i["status"]})' for i in interface if i["status"] not in done]
elif phase != "ledger":
    errors.append(f"unknown phase {phase}")
print("\n".join(errors))
sys.exit(1 if errors else 0)
