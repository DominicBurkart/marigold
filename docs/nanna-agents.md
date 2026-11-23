# nanna agent identities

nanna spawns agents from human-authored identity cards. This repository ships
four cards scoped to its workspace layout, a base catalog they narrow, and a
check that parses both with nanna's own parser.

## Status

The cards in `.nanna/agents/` take effect only in `nanna agents list --repo`
(DominicBurkart/nanna-coder#931). They do not constrain running tasks, which
get the global base card.

The inchworm flow in `inchworm.toml` is configuration only today. A flow cannot
reach `pr_open` until nanna creates the branch and pull request, because
inchworm itself never publishes.

## Files

- `.nanna/agents/*.toml`: Repo-local cards. Each names a base card and may only
  narrow it.
- `.nanna/catalog/*.toml`: Reference copy of the base catalog (including the
  required `auditor`).
- `.nanna/catalog/prompts/`: System prompts for the base cards.
- `scripts/check_nanna_agents.sh`: Parses both layers with nanna and fails on a
  widening override.

`.nanna/**` is in nanna's fixed protected set for every repository, so an
agent cannot edit these cards.

## Installing the base catalog

nanna merges `.nanna/agents/` over a host-owned global catalog
(`$NANNA_CONFIG_DIR/agents`, else `$XDG_CONFIG_HOME/nanna/agents`, else
`~/.config/nanna/agents`). A repo-local card must share its name with a global
card and may only narrow it: `scope.max_effect` and every `[limits]` value at
or below the base; `scope.repos`, `scope.paths`, `scope.tools` and
`scope.read_paths` string-subsets of the base lists. A name with no global card
is rejected, and `auditor` cannot be overridden.

`.nanna/catalog` is a marigold-only reference copy, not an installer. A host
catalog is shared across repositories (its cards list several repositories and
paths), so copying this directory over it would overwrite those cards. Merge
the marigold entries into the host cards by hand, or install the reference only
into a scratch directory:

```sh
mkdir -p /tmp/nanna-scratch/agents
cp -r .nanna/catalog/. /tmp/nanna-scratch/agents/
NANNA_CONFIG_DIR=/tmp/nanna-scratch nanna agents list --repo .
```

## Identities

Write scopes (`paths`) use the workspace crates: `marigold`, `marigold-macros`,
`marigold-grammar`, `marigold-lsp`, `marigold-impl`, plus `tests/` and
`examples/`.

- `rust-implementer`
  - Loop: inner
  - Writes (`paths`, repo-local): the five crates and `tests/**` (base also
    allows `examples/**`)
  - Ceiling: workspace
  - Tools (repo-local): `read_file`, `write_file`, `list_directory`, `search`,
    `git_status`, `git_diff`, `cargo_check`, `cargo_test` (base adds
    `cargo_build`)
- `pr-shepherd`
  - Loop: middle
  - Writes (`paths`, repo-local): none
  - Ceiling: repository
  - Tools (repo-local): `read_file`, `search`, `git_status`, `git_diff`,
    `git_commit`, `git_push_branch`, `github_pr_open`, `github_pr_comments`
- `reviewer`
  - Loop: middle
  - Writes (`paths`, repo-local): none
  - Ceiling: none
  - Tools (repo-local): `read_file`, `list_directory`, `search`, `git_status`,
    `git_diff`
- `docs-writer`
  - Loop: inner
  - Writes (`paths`, repo-local): `docs/**`, `README.md`
  - Ceiling: workspace
  - Tools (repo-local): `read_file`, `write_file`, `list_directory`, `search`,
    `git_status`, `git_diff`
- `auditor`
  - Loop: inner
  - Writes (`paths`, repo-local): none
  - Ceiling: none
  - Tools (repo-local): none (base only, cannot be overridden)

All cards use model `gemma4:e4b` and repository
`github.com/DominicBurkart/marigold`.
`rust-implementer` has no network: its ceiling is workspace and it is given no
`github_*` tool. `pr-shepherd` is the only card with the repository ceiling.
Its four tools (`git_commit`, `git_push_branch`, `github_pr_open`,
`github_pr_comments`) all exist on nanna main, so none was omitted. It is
classified `middle` because it also reads pull request comments.

Because every writing card restricts paths, none receives `run_command`.
Tool names are exact strings, since the narrowing check does not interpret
globs.

`docs-writer` has no markdown-specific tool: nanna ships none, so it gets the
file tools only. The pre-commit hook requires `README.md` and
`marigold/README.md` to be identical, but the host base card does not allow
writing `marigold/README.md`, and a repo card cannot widen its base. So
`docs-writer` cannot keep the two files identical: its prompt tells it to
report the needed sync, and a human or a card with the right paths does the
copy. No flow step can sync them today. `rust-implementer`'s `marigold/**` path
technically allows writing `marigold/README.md`, while its prompt forbids it.

## No deployer card

There is no `deployer`. This repository has no deploy path: no deploy workflow,
no `.nanna/deploy.toml`, no hosted service. `publish.sh` publishes crates to
crates.io, which is a release performed by a maintainer with registry
credentials, not something an agent should hold a card for.

## Listing and verifying

```sh
scripts/check_nanna_agents.sh
```

The script (needs a `nanna` binary on `PATH`, or `NANNA_BIN`, and `python3`)
copies the repo catalog to a scratch config directory and runs
`nanna agents list --repo .`. When a host catalog exists (`NANNA_CONFIG_DIR`,
else `$XDG_CONFIG_HOME/nanna`, else `~/.config/nanna`, with an `agents`
directory), it also runs the listing against that real config, because the
host base is what nanna merges at run time. Rejection by the host is a failure.
A difference between `.nanna/catalog` and the host catalog is only a printed
warning, since the repo catalog is intentionally a marigold-only subset.

It reads each card name from `[identity]` with `tomllib`. It exits 1 when an
override widens its base (from either catalog), names an unknown card, fails to
parse, declares a name that differs from its file name, or when
`.nanna/agents` has no cards. It is not wired into CI.

`scripts/test_check_nanna_agents.sh` runs the script against fixtures under
`scripts/fixtures/nanna_agents/` and asserts the exact exit status and message
for a narrowed card (0), one widened card per dimension (`scope.paths`,
`scope.max_effect`, `scope.tools`; 1), a name and file mismatch (1), a missing
agents directory (1), a matching host catalog (0), a drifted but compatible host
catalog (0 with a warning) and a host base narrower than the repo catalog (1).

## Limits

- nanna applies `.nanna/agents/` only in `nanna agents list --repo`. `mcp-serve`
  and `delegate` load the global catalog alone, so a running task gets the base
  card, not the narrowed one (DominicBurkart/nanna-coder#931).
- There is no per-card container field (DominicBurkart/nanna-coder#1012). The
  per-repo image comes from the flake `devContainerImage`.

## Container

nanna runs a task in a dev container only when the repository has both
`flake.nix` and `.devcontainer/`. This repository now has both:

- `flake.nix`: Exposes `packages.<system>.devContainerImage` (nix2container,
  x86_64 and aarch64 linux) and a `devShells.default`.
- `flake.lock`: Pins nixpkgs, rust-overlay and nix2container.
- `.devcontainer/devcontainer.json`: Opt-in marker; nanna only checks that the
  directory exists.
- `.nanna/protected.toml`: Declares `inchworm.toml`, `.inchworm/**` and other CI
  and release files as extra protected globs.
- `.nanna/prompt.md`: Per-project prompt: workspace layout and the checks CI and
  the pre-commit hook run.

The image carries the toolchain read from `rust-toolchain.toml` through
`rust-bin.fromRustupToolchainFile` (stable, with rustfmt and clippy), a C
toolchain, `pkg-config`, OpenSSL, zlib, git and coreutils. It sets
`CARGO_HOME=/tmp/cargo` and `MARIGOLD_WORKSPACE_PATH=/workspace/marigold`, which
`cargo_check`, `cargo_test` and `cargo_build` need to compile the workspace.
Build it with `nix build .#devContainerImage`.

`flake.nix`, `flake.lock` and `.devcontainer/**` are in nanna's fixed protected
set, so an agent cannot edit them.

CI does not use the flake: no workflow under `.github/workflows` invokes `nix`
or references `flake.nix`, so the required checks are unaffected. The Rust
toolchain used by CI still comes from `rust-toolchain.toml`; the flake only
reads that file.
