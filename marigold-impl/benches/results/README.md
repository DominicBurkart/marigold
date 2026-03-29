# Benchmark Results

Checked-in benchmark data for reproducibility and cross-reference.

## Directory Convention

```
results/
  {commit_short}/          # Short SHA of the code commit benchmarked
    {machine_label}/       # Descriptive label: cores + environment
      machine_info.txt     # CPU info (nproc, arch, environment)
      summary.md           # Cited results with source permalinks
      bench_summary.md     # Raw output from collect_all_data.sh
      cpu_utilization.txt  # Custom harness: effective cores
      driver_worker_split.txt  # Custom harness: driver vs worker timing
      criterion/           # Criterion JSON artifacts
        {bench_name}/
          new/estimates.json
          base/estimates.json  # (if available)
```

## Adding a New Run

1. Checkout the commit you want to benchmark
2. Run: `cd marigold-impl && bash benches/collect_all_data.sh`
3. Create the output directory: `results/$(git rev-parse --short HEAD)/{machine_label}/`
4. Copy artifacts into that directory following the structure above
5. Include `machine_info.txt` with at least: core count, architecture, environment

## Comparing Runs

- **Same commit, different machines:** Compare directories under the same commit SHA
  (e.g., `a1bd575/4core-ci/` vs `a1bd575/16core-laptop/`)
- **Different commits, same machine:** Compare matching machine labels across commits
  (e.g., `a1bd575/16core-laptop/` vs `{new_commit}/16core-laptop/`)
- **Full suite vs targeted:** Each `summary.md` lists which benchmarks were run.
  Partial runs are valid — just note which benchmarks are present.

## Current Data

| Commit | Machine | Benchmarks | Notes |
|--------|---------|------------|-------|
| `a1bd575` | 4core-ci | Full suite | Cloud VM, Intel Xeon @ 2.1GHz |
| `a1bd575` | 16core-laptop | Full suite | Bare metal, 16-core x86_64 |
