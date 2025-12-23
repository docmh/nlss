# Core-Stats

R-based statistics helpers that produce an APA 7-ready report plus machine-readable JSONL logs. The repo is organized as "subskills" with a shared workflow and consistent output locations.

## Requirements and system support

- R 4.0+ (base R is enough for CSV/APA outputs).
- Required R packages: `yaml` (configuration + templates), `jsonlite` (analysis logging), and `arrow` (parquet workspace copies).
- Optional R packages: `haven` (preferred) or `foreign` for SPSS `.sav` input support; `car` for Type II/III ANOVA sums of squares.
- Windows, WSL (Ubuntu), or Linux.
- PowerShell 5.1+ is recommended on Windows for the wrapper script.
- Optional: WSL if you want the wrapper to run Linux Rscript first and fall back to Windows Rscript.

Install the R dependencies:

```r
install.packages(c("yaml", "jsonlite", "arrow", "haven"))
# install.packages("foreign") # legacy fallback if haven is not available
# install.packages("car") # optional for Type II/III ANOVA sums of squares
```

## Install

No build step. Clone and run the scripts directly:

```bash
git clone <repo-url>
cd core-stats
```

If you are on Windows, ensure `Rscript.exe` is on your PATH or set `RSCRIPT` to its full path.

## Quick start

Outputs always go to the dataset workspace at `<workspace-root>/<dataset-name>/` and are not user-overridable. Workspace root is the current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; if no manifest is present, scripts fall back to `defaults.output_dir` in `core-stats/scripts/config.yml`. Each run writes `apa_report.md` and, when logging is enabled, appends to `analysis_log.jsonl` inside that dataset folder.

### Windows (PowerShell wrapper; WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File core-stats/scripts/run_rscript.ps1 `
  core-stats/scripts/R/descriptive_stats.R `
  --csv data.csv --vars age,score
```

### WSL/Linux (Rscript directly)

```bash
Rscript core-stats/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score
```

## Stateful workspace architecture

Core-stats is stateful. The workspace root is the current directory, its parent, or a one-level child containing `core-stats-workspace.yml` (fallback: `defaults.output_dir`), and each dataset gets its own subfolder.

- Run `core-stats/scripts/R/init_workspace.R` to create `<workspace-root>/<dataset-name>/scratchpad.md`, `apa_report.md`, `analysis_log.jsonl`, a parquet workspace copy, and `core-stats-workspace.yml` in the workspace root.
- For any input dataset (CSV/SAV/RDS/RData/Parquet), the workspace copy lives at `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- All subskills read from the workspace parquet copy (prefer `--parquet` pointing to the workspace file or rely on auto-copy).
- When running from a dataset folder without input flags, scripts select that dataset; otherwise they load `active_dataset` from the manifest.
- Workspaces must be non-nested and unique per parent folder; scripts stop if nested or sibling workspace manifests are detected.
- `data-transform` and `missings` update the workspace parquet copy in place and save a backup to `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting.

## Available modules (subskills)

Each subskill has a reference file describing inputs, flags, and outputs. Template-driven modules can be customized via `core-stats/assets/<subskill>/` and `templates.*` in `core-stats/scripts/config.yml`.

| Subskill | Script | Purpose | APA template |
| --- | --- | --- | --- |
| `descriptive-stats` | `core-stats/scripts/R/descriptive_stats.R` | Descriptive statistics with APA tables/text. | Yes (`descriptive-stats/default-template.md`) |
| `frequencies` | `core-stats/scripts/R/frequencies.R` | Frequency tables for categorical variables. | Yes (`frequencies/default-template.md`, `frequencies/grouped-template.md`) |
| `crosstabs` | `core-stats/scripts/R/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
| `correlations` | `core-stats/scripts/R/correlations.R` | Correlations, partial correlations, diagnostics. | Yes (`correlations/default-template.md`, `correlations/cross-correlation-template.md`) |
| `scale` | `core-stats/scripts/R/scale.R` | Item analysis and reliability (alpha/omega) for scales. | Yes (`scale/default-template.md`) |
| `data-explorer` | `core-stats/scripts/R/data_explorer.R` | Data dictionary exploration with missingness and level summaries. | Yes (`data-explorer/default-template.md`) |
| `data-transform` | `core-stats/scripts/R/data_transform.R` | Derived variables, recoding, binning, renaming, and drop operations. | Yes (`data-transform/default-template.md`) |
| `missings` | `core-stats/scripts/R/missings.R` | Missing-data patterns, handling decisions, and transformed datasets. | Yes (`missings/default-template.md`) |
| `assumptions` | `core-stats/scripts/R/assumptions.R` | Assumption checks for t-tests, ANOVA, and regression. | Yes (`assumptions/ttest-template.md`, `assumptions/anova-template.md`, `assumptions/regression-template.md`) |
| `regression` | `core-stats/scripts/R/regression.R` | Multiple and hierarchical regression (OLS/GLM) with interactions and bootstrap CIs. | Yes (`regression/default-template.md`) |
| `anova` | `core-stats/scripts/R/anova.R` | Between-, within-, and mixed ANOVA with post-hoc comparisons. | Yes (`anova/default-template.md`, `anova/posthoc-template.md`) |
| `t-test` | `core-stats/scripts/R/t_test.R` | One-sample, independent-samples, and paired-samples t-tests. | Yes (`t-test/default-template.md`) |
| `init-workspace` | `core-stats/scripts/R/init_workspace.R` | Initialize workspace folder with scratchpad.md, APA report, and .parquet copies. | Yes (`init-workspace/default-template.md`) |

Reference docs:
- `core-stats/references/descriptive-stats.md`
- `core-stats/references/frequencies.md`
- `core-stats/references/crosstabs.md`
- `core-stats/references/correlations.md`
- `core-stats/references/scale.md`
- `core-stats/references/data-explorer.md`
- `core-stats/references/data-transform.md`
- `core-stats/references/missings.md`
- `core-stats/references/assumptions.md`
- `core-stats/references/regression.md`
- `core-stats/references/anova.md`
- `core-stats/references/t-test.md`
- `core-stats/references/init-workspace.md`

## Basic usage by module

Each run writes `apa_report.md` in the output directory and appends to `analysis_log.jsonl` when logging is enabled.

### Descriptive statistics

```bash
Rscript core-stats/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

### Frequencies

```bash
Rscript core-stats/scripts/R/frequencies.R \
  --csv data.csv --vars gender,condition --group condition
```

### Cross-tabulations

```bash
Rscript core-stats/scripts/R/crosstabs.R \
  --csv data.csv --row gender --col condition --group site
```

### Correlations

```bash
Rscript core-stats/scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

### Scale analysis

```bash
Rscript core-stats/scripts/R/scale.R \
  --csv data.csv --vars item1,item2,item3 --group condition
```

### Data exploration

```bash
Rscript core-stats/scripts/R/data_explorer.R \
  --csv data.csv --vars age,score --max-levels 15 --top-n 8
```

### Data transformation

```bash
Rscript core-stats/scripts/R/data_transform.R \
  --csv data.csv --standardize age,score
```

### Missing data handling

```bash
Rscript core-stats/scripts/R/missings.R \
  --csv data.csv --vars age,score,stress
```

### Assumptions checks

```bash
Rscript core-stats/scripts/R/assumptions.R \
  --csv data.csv --analysis ttest --vars score --group condition
```

### Regression

```bash
Rscript core-stats/scripts/R/regression.R \
  --csv data.csv --dv outcome --blocks "age,gender;stress,trait" --interactions stress:trait --center mean
```

### ANOVA

```bash
Rscript core-stats/scripts/R/anova.R \
  --csv data.csv --dv outcome --between group
```

### t-tests

```bash
Rscript core-stats/scripts/R/t_test.R \
  --csv data.csv --vars score --group condition
```

### Workspace initialization

```bash
Rscript core-stats/scripts/R/init_workspace.R \
  --csv data.csv
```

## Where outputs go

All scripts write to the dataset workspace at `<workspace-root>/<dataset-name>/` and do not accept a custom output directory. Workspace root is the current directory, its parent, or a one-level child containing `core-stats-workspace.yml` (fallback: `defaults.output_dir` in `core-stats/scripts/config.yml`).
Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`; `data_transform` and `missings` update these copies in place and create backups in `<workspace-root>/<dataset-name>/backup/`.

## Configuration logic

Defaults live in `core-stats/scripts/config.yml` and are loaded via `core-stats/scripts/R/lib/config.R`.

- `defaults.*` apply across all modules (for example `defaults.output_dir`, `defaults.workspace_manifest`).
- `modules.<subskill>.*` holds per-module defaults (for example `modules.crosstabs.percent`).
- `templates.<subskill>.*` controls the template file used for APA outputs (see next section).
- CLI flags always override config values at runtime (for example `--digits`, module-specific flags).
- When `config.yml` is missing or unreadable, built-in defaults in `config.R` are used.

## APA template logic (YAML)

Templates are Markdown files under `core-stats/assets/<subskill>/` with YAML front matter. They drive `apa_report.md` output for the subskills that ship with templates (descriptive stats, frequencies, crosstabs, correlations, scale, data exploration, data transformation, missingness handling, assumptions, regression, ANOVA, and t-tests).

Key YAML fields:

- `tokens`: static strings you can reference as `{{token}}` in the template body.
- `table.columns`: ordered column specs with `key`, `label`, and optional `drop_if_empty`.
- `note.template`: controls `{{note_body}}` rendering (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: controls `{{narrative}}` rendering; `row_template` repeats over `narrative_rows` and can be joined with `narrative.join`.

Template paths can be overridden in `core-stats/scripts/config.yml` under `templates.<subskill>.<name>` (for example `templates.crosstabs.grouped`). Edit the template files or point to your own to change APA output without touching the R scripts.

## Using with Codex (Codes)

Codex discovers this repo's skill via `AGENTS.md` and `core-stats/SKILL.md`. Open Codex in the repo root and ask for a statistical task; it should route to the correct subskill automatically.

Example prompt:

```
Use core-stats to run correlations (Pearson) on data.csv for age, score, and stress.
Write outputs to outputs/tmp and summarize the APA text.
```

## Using with Claude Code

Claude Code can use the same repo structure. Open the repo and tell Claude to use the core-stats subskill reference files and scripts.

Example prompt:

```
Use the core-stats repo. Run descriptive_stats on data.csv for age and score.
Use outputs/tmp and report the APA narrative and table file names.
```
