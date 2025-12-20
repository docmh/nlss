# Statistic Skills

R-based statistics helpers that produce machine-readable outputs (CSV/TSV/JSON) plus APA 7-ready tables and narrative text. The repo is organized as "subskills" with a shared workflow and consistent output locations.

## Requirements and system support

- R 4.0+ (base R only; no extra packages required).
- Windows, WSL (Ubuntu), or Linux.
- PowerShell 5.1+ is recommended on Windows for the wrapper script.
- Optional: WSL if you want the wrapper to run Linux Rscript first and fall back to Windows Rscript.

## Install

No build step. Clone and run the scripts directly:

```bash
git clone <repo-url>
cd r-core-stats
```

If you are on Windows, ensure `Rscript.exe` is on your PATH or set `RSCRIPT` to its full path.

## Quick start

Outputs go to `./outputs/tmp` by default when `--out` is omitted.

### Windows (PowerShell wrapper; WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File r-core-stats/scripts/run_rscript.ps1 `
  r-core-stats/scripts/descriptive_stats.R `
  --csv data.csv --vars age,score --out outputs\tmp
```

### WSL/Linux (Rscript directly)

```bash
Rscript r-core-stats/scripts/descriptive_stats.R \
  --csv data.csv --vars age,score --out outputs/tmp
```

## Available modules (subskills)

Each subskill has a reference file describing inputs, flags, and outputs.

| Subskill | Script | Purpose |
| --- | --- | --- |
| `descriptive-stats-r` | `r-core-stats/scripts/descriptive_stats.R` | Descriptive statistics with APA tables/text. |
| `frequencies-r` | `r-core-stats/scripts/frequencies.R` | Frequency tables for categorical variables. |
| `crosstabs-r` | `r-core-stats/scripts/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. |
| `correlations-r` | `r-core-stats/scripts/correlations.R` | Correlations, partial correlations, diagnostics. |

Reference docs:
- `r-core-stats/references/descriptive-stats-r.md`
- `r-core-stats/references/frequencies-r.md`
- `r-core-stats/references/crosstabs-r.md`
- `r-core-stats/references/correlations-r.md`

## Basic usage by module

### Descriptive statistics

```bash
Rscript r-core-stats/scripts/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

Outputs: `descriptive_summary.csv`, `apa_table.md`, `apa_text.txt`.

### Frequencies

```bash
Rscript r-core-stats/scripts/frequencies.R \
  --csv data.csv --vars gender,condition --group condition
```

Outputs: `frequencies_summary.csv`, `apa_table.md`, `apa_text.txt`.

### Cross-tabulations

```bash
Rscript r-core-stats/scripts/crosstabs.R \
  --csv data.csv --row gender --col condition --group site
```

Outputs: `crosstabs_cells.csv`, `crosstabs_tests.csv`, `crosstabs_diagnostics.csv`, `apa_table.md`, `apa_text.txt`.

### Correlations

```bash
Rscript r-core-stats/scripts/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

Outputs: `correlations_summary.csv`, `correlations_diagnostics.csv`, `apa_table.md`, `apa_text.txt`.

## Using with Codex (Codes)

Codex discovers this repo's skill via `AGENTS.md` and `r-core-stats/SKILL.md`. Open Codex in the repo root and ask for a statistical task; it should route to the correct subskill automatically.

Example prompt:

```
Use r-core-stats to run correlations (Pearson) on data.csv for age, score, and stress.
Write outputs to outputs/tmp and summarize the APA text.
```

## Using with Claude Code

Claude Code can use the same repo structure. Open the repo and tell Claude to use the r-core-stats subskill reference files and scripts.

Example prompt:

```
Use the r-core-stats repo. Run descriptive_stats on data.csv for age and score.
Use outputs/tmp and report the APA narrative and table file names.
```

## Where outputs go

All scripts default to `./outputs/tmp` when `--out` is omitted. Keep this folder in the working directory you run the scripts from.

