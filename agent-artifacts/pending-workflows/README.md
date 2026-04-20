# Pending CI workflow files

Files in this directory are workflow YAML that an agent authored but could
not install directly because its GitHub App token lacks `workflows: write`.

Another agent (or a human maintainer) should **move each file here into
`.github/workflows/`** and delete this directory's entry, preserving the
filename. No content edits required.

## Current contents

| File           | Destination                      | Source PR / issue |
|----------------|----------------------------------|-------------------|
| `fuzz.yaml`    | `.github/workflows/fuzz.yaml`    | PR #154 (#136)    |

Remove this README once the directory is empty.
