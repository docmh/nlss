# R-Core-Stats

R-based statistics helpers that produce an APA 7-ready report plus machine-readable JSONL logs. The repo is organized as "subskills" with a shared workflow and consistent output locations.

## Requirements and system support

- R 4.0+ (base R is enough for CSV/APA outputs).
- Required R packages: `yaml` (configuration + templates) and `jsonlite` (analysis logging).
- Optional R packages: `haven` (preferred) or `foreign` for SPSS `.sav` input support.
- Windows, WSL (Ubuntu), or Linux.
- PowerShell 5.1+ is recommended on Windows for the wrapper script.
- Optional: WSL if you want the wrapper to run Linux Rscript first and fall back to Windows Rscript.

Install the R dependencies:

```r
install.packages(c("yaml", "jsonlite", "haven"))
# install.packages("foreign") # legacy fallback if haven is not available
```

## Install

No build step. Clone and run the scripts directly:

```bash
git clone <repo-url>
cd r-core-stats
```

If you are on Windows, ensure `Rscript.exe` is on your PATH or set `RSCRIPT` to its full path.

## Quick start

Outputs go to `./outputs/tmp` by default when `--out` is omitted. Each run writes `apa_report.md` and, when logging is enabled, appends to `analysis_log.jsonl`.

### Windows (PowerShell wrapper; WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File r-core-stats/scripts/run_rscript.ps1 `
  r-core-stats/scripts/R/descriptive_stats.R `
  --csv data.csv --vars age,score --out outputs\tmp
```

### WSL/Linux (Rscript directly)

```bash
Rscript r-core-stats/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --out outputs/tmp
```

## Available modules (subskills)

Each subskill has a reference file describing inputs, flags, and outputs. Template-driven modules can be customized via `r-core-stats/assets/<subskill>/` and `templates.*` in `r-core-stats/scripts/config.yml`.

| Subskill | Script | Purpose | APA template |
| --- | --- | --- | --- |
| `descriptive-stats-r` | `r-core-stats/scripts/R/descriptive_stats.R` | Descriptive statistics with APA tables/text. | Yes (`descriptive-stats/default-template.md`) |
| `frequencies-r` | `r-core-stats/scripts/R/frequencies.R` | Frequency tables for categorical variables. | Built-in (no template file) |
| `crosstabs-r` | `r-core-stats/scripts/R/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
| `correlations-r` | `r-core-stats/scripts/R/correlations.R` | Correlations, partial correlations, diagnostics. | Yes (`correlations/default-template.md`, `correlations/cross-correlation-template.md`) |
| `data-explorer-r` | `r-core-stats/scripts/R/data_explorer.R` | Data dictionary exploration with missingness and level summaries. | Built-in (no template file) |
| `data-transform-r` | `r-core-stats/scripts/R/data_transform.R` | Derived variables, recoding, binning, renaming, and drop operations. | Built-in (no template file) |

Reference docs:
- `r-core-stats/references/descriptive-stats-r.md`
- `r-core-stats/references/frequencies-r.md`
- `r-core-stats/references/crosstabs-r.md`
- `r-core-stats/references/correlations-r.md`
- `r-core-stats/references/data-explorer-r.md`
- `r-core-stats/references/data-transform-r.md`

## Basic usage by module

Each run writes `apa_report.md` in the output directory and appends to `analysis_log.jsonl` when logging is enabled.

### Descriptive statistics

```bash
Rscript r-core-stats/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition --out outputs/tmp
```

### Frequencies

```bash
Rscript r-core-stats/scripts/R/frequencies.R \
  --csv data.csv --vars gender,condition --group condition --out outputs/tmp
```

### Cross-tabulations

```bash
Rscript r-core-stats/scripts/R/crosstabs.R \
  --csv data.csv --row gender --col condition --group site --out outputs/tmp
```

### Correlations

```bash
Rscript r-core-stats/scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman --out outputs/tmp
```

### Data exploration

```bash
Rscript r-core-stats/scripts/R/data_explorer.R \
  --csv data.csv --vars age,score --max-levels 15 --top-n 8 --out outputs/tmp
```

### Data transformation

```bash
Rscript r-core-stats/scripts/R/data_transform.R \
  --csv data.csv --standardize age,score --out outputs/tmp
```

## Where outputs go

All scripts default to `./outputs/tmp` when `--out` is omitted. Keep this folder in the working directory you run the scripts from.

## Configuration logic

Defaults live in `r-core-stats/scripts/config.yml` and are loaded via `r-core-stats/scripts/R/lib/config.R`.

- `defaults.*` apply across all modules (for example `defaults.output_dir`).
- `modules.<subskill>.*` holds per-module defaults (for example `modules.crosstabs.percent`).
- `templates.<subskill>.*` controls the template file used for APA outputs (see next section).
- CLI flags always override config values at runtime (for example `--out`, `--digits`, module-specific flags).
- When `config.yml` is missing or unreadable, built-in defaults in `config.R` are used.

## APA template logic (YAML)

Templates are Markdown files under `r-core-stats/assets/<subskill>/` with YAML front matter. They drive `apa_report.md` output for the subskills that ship with templates (descriptive stats, correlations, and crosstabs).

Key YAML fields:

- `tokens`: static strings you can reference as `{{token}}` in the template body.
- `table.columns`: ordered column specs with `key`, `label`, and optional `drop_if_empty`.
- `note.template`: controls `{{note_body}}` rendering (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: controls `{{narrative}}` rendering; `row_template` repeats over `narrative_rows` and can be joined with `narrative.join`.

Template paths can be overridden in `r-core-stats/scripts/config.yml` under `templates.<subskill>.<name>` (for example `templates.crosstabs.grouped`). Edit the template files or point to your own to change APA output without touching the R scripts.

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
