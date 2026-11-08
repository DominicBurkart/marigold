# nanna agent identities

nanna spawns agents from human-authored identity cards. This repository ships
four cards scoped to its workspace layout, a base catalog they narrow, and a
check that parses both with nanna's own parser.

## Files

| Path | Role |
| --- | --- |
| `.nanna/agents/*.toml` | Repo-local cards. Each names a base card and may only narrow it. |
| `.nanna/catalog/*.toml` | Reference copy of the base catalog (including the required `auditor`). |
| `.nanna/catalog/prompts/` | System prompts for the base cards. |
| `scripts/check_nanna_agents.sh` | Parses both layers with nanna and fails on a widening override. |

`.nanna/**` is in nanna's fixed protected set for every repository, so an
agent cannot edit these cards.

## Installing the base catalog

nanna merges `.nanna/agents/` over a host-owned global catalog
(`$NANNA_CONFIG_DIR/agents`, else `$XDG_CONFIG_HOME/nanna/agents`, else
`~/.config/nanna/agents`). A repo-local card must share its name with a global
card and may only narrow it: `scope.max_effect` and every `[limits]` value at
or below the base; `scope.repos`, `scope.paths`, `scope.tools` and
`scope.read_paths` string-subsets of the base lists. A name with no global card
is rejected, and `auditor` cannot be overridden. Installing the base is a human
step on the host that runs nanna:

```
mkdir -p ~/.config/nanna/agents
cp -r .nanna/catalog/. ~/.config/nanna/agents/
```

## Identities

Write scopes (`paths`) use the workspace crates: `marigold`, `marigold-macros`,
`marigold-grammar`, `marigold-lsp`, `marigold-impl`, plus `tests/` and
`examples/`.

| Name | Loop | Writes (`paths`, repo-local) | Ceiling | Tools (repo-local) |
| --- | --- | --- | --- | --- |
| `rust-implementer` | inner | the five crates and `tests/**` (base also allows `examples/**`) | workspace | `read_file`, `write_file`, `list_directory`, `search`, `git_status`, `git_diff`, `cargo_check`, `cargo_test` (base adds `cargo_build`) |
| `pr-shepherd` | middle | none | repository | `read_file`, `search`, `git_status`, `git_diff`, `git_commit`, `git_push_branch`, `github_pr_open`, `github_pr_comments` |
| `reviewer` | middle | none | none | `read_file`, `list_directory`, `search`, `git_status`, `git_diff` |
| `docs-writer` | inner | `docs/**`, `README.md` | workspace | `read_file`, `write_file`, `list_directory`, `search`, `git_status`, `git_diff` |
| `auditor` | inner | none | none | none (base only, cannot be overridden) |

All cards use model `gemma4:e4b` and repository `github.com/DominicBurkart/marigold`.
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
`marigold/README.md` to be identical; the card cannot write the latter, so its
prompt tells it to report the needed sync.

## No deployer card

There is no `deployer`. This repository has no deploy path: no deploy workflow,
no `.nanna/deploy.toml`, no hosted service. `publish.sh` publishes crates to
crates.io, which is a release performed by a maintainer with registry
credentials, not something an agent should hold a card for.

## Listing and verifying

```
scripts/check_nanna_agents.sh
```

The script (needs a `nanna` binary on `PATH`, or `NANNA_BIN`) copies the base
catalog to a scratch config directory and runs `nanna agents list --repo .`.
It exits non-zero when an override widens its base, names an unknown card, or
fails to parse. It is not wired into CI.

## Limits

- nanna applies `.nanna/agents/` only in `nanna agents list --repo`. `mcp-serve`
  and `delegate` load the global catalog alone, so a running task gets the base
  card, not the narrowed one (DominicBurkart/nanna-coder#931).
- There is no per-card container field (DominicBurkart/nanna-coder#1012). The
  per-repo image comes from the flake `devContainerImage`. nanna runs a task in
  a dev container only when the repository has both `flake.nix` and
  `.devcontainer/`. This repository has neither, so adding them is a
  prerequisite for running these cards in a container, and for the `cargo_*`
  tools, which exist only in the container registry.
