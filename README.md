# nlss - Natural Language Statistics Suite

R-based statistics helpers that produce an APA 7-ready report plus machine-readable JSONL logs. The repo is organized as "subskills" with a shared workflow and consistent output locations.

## Requirements and system support

- R 4.4+ (base R is enough for CSV/APA outputs).
- Required R packages: `yaml` (configuration + templates), `jsonlite` (analysis logging), `arrow` (parquet workspace copies), and `ggplot2` (plot subskill).
- Optional R packages: `haven` (preferred) or `foreign` for SPSS `.sav` input support; `car` for Type II/III ANOVA sums of squares; `lme4` for mixed models; `lmerTest` for df/p-values; `emmeans` for marginal means/contrasts; `performance` for R2/ICC; `lavaan` for SEM/CFA/mediation.
- Windows, WSL (Ubuntu), or Linux.
- PowerShell 5.1+ is recommended on Windows for the wrapper script.
- Optional: WSL if you want the wrapper to run Linux Rscript first and fall back to Windows Rscript.

Install the R dependencies:

```bash
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('yaml','jsonlite','arrow','ggplot2','haven','foreign','car','lme4','lmerTest','emmeans','performance','lavaan'))"
```

Or install them interactively in R:

```r
install.packages(c('yaml','jsonlite','arrow','ggplot2','haven','foreign','car','lme4','lmerTest','emmeans','performance','lavaan'))
```


## Install

No build step. Clone and run the scripts directly:

```bash
git clone <repo-url>
cd nlss
```

If you are on Windows, ensure `Rscript.exe` is on your PATH or set `RSCRIPT` to its full path.

## Quick start

Outputs always go to the dataset workspace at `<workspace-root>/<dataset-name>/` and are not user-overridable. Workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml`; if no manifest is present, scripts fall back to `defaults.output_dir` in `nlss/scripts/config.yml`. Each run writes `apa_report.md` and, when logging is enabled, appends to `analysis_log.jsonl` inside that dataset folder.

### Windows (PowerShell wrapper; WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File nlss/scripts/run_rscript.ps1 `
  nlss/scripts/R/descriptive_stats.R `
  --csv data.csv --vars age,score
```

### PowerShell wrapper cheat sheet

- First argument must be the `.R` script path; all other flags come after it.
- Lists use comma-separated values with no spaces: `x1,x2,x3`.
- Quote values that contain spaces or semicolons (for example `--blocks "x1,x2;x3,mediator"`).
- Relative paths are resolved from the current PowerShell working directory; use absolute paths when in doubt.
- For paths with non-ASCII characters (for example, umlauts), the wrapper prefers Windows Rscript when available; set `NLSS_FORCE_WSL=1` to keep WSL, or `NLSS_SKIP_WSL=1` to always skip WSL.
- If a path arrives with mangled characters (for example `?`), the wrapper attempts to repair it by matching on-disk names.

Examples:

```powershell
# From workspace root, using the manifest + active dataset
powershell -ExecutionPolicy Bypass -File nlss/scripts/run_rscript.ps1 `
  nlss/scripts/R/descriptive_stats.R `
  --vars "x1,x2,x3"

# Absolute CSV path with spaces
powershell -ExecutionPolicy Bypass -File nlss/scripts/run_rscript.ps1 `
  nlss/scripts/R/descriptive_stats.R `
  --csv "C:\Users\me\My Data\study.csv" --vars "x1,x2,x3"

# Workspace parquet copy (preferred)
powershell -ExecutionPolicy Bypass -File nlss/scripts/run_rscript.ps1 `
  nlss/scripts/R/descriptive_stats.R `
  --parquet "C:\workspaces\nlss\study\study.parquet" --vars "x1,x2,x3"

# Blocks with semicolons need quotes
powershell -ExecutionPolicy Bypass -File nlss/scripts/run_rscript.ps1 `
  nlss/scripts/R/regression.R `
  --dv outcome --blocks "x1,x2;x3,mediator"
```

### WSL/Linux (Rscript directly)

```bash
Rscript nlss/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score
```

## Stateful workspace architecture

nlss is stateful. The workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir`), and each dataset gets its own subfolder.

- Run `nlss/scripts/R/init_workspace.R` to create `<workspace-root>/<dataset-name>/scratchpad.md`, `apa_report.md`, `analysis_log.jsonl`, a parquet workspace copy, and `nlss-workspace.yml` in the workspace root.
- For any input dataset (CSV/SAV/RDS/RData/Parquet), the workspace copy lives at `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- All subskills read from the workspace parquet copy (prefer `--parquet` pointing to the workspace file or rely on auto-copy).
- When running from a dataset folder without input flags, scripts select that dataset; otherwise they load `active_dataset` from the manifest.
- Workspaces must be non-nested and unique per parent folder; scripts stop if nested or sibling workspace manifests are detected.
- `data-transform` and `missings` update the workspace parquet copy in place and save a backup to `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting.

## Available modules (subskills)

Each subskill has a reference file describing inputs, flags, and outputs. Template-driven modules can be customized via `nlss/assets/<subskill>/` and `templates.*` in `nlss/scripts/config.yml`.

| Subskill | Script | Purpose | APA template |
| --- | --- | --- | --- |
| `descriptive-stats` | `nlss/scripts/R/descriptive_stats.R` | Descriptive statistics with APA tables/text. | Yes (`descriptive-stats/default-template.md`) |
| `frequencies` | `nlss/scripts/R/frequencies.R` | Frequency tables for categorical variables. | Yes (`frequencies/default-template.md`, `frequencies/grouped-template.md`) |
| `crosstabs` | `nlss/scripts/R/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
| `correlations` | `nlss/scripts/R/correlations.R` | Correlations, partial correlations, diagnostics. | Yes (`correlations/default-template.md`, `correlations/cross-correlation-template.md`) |
| `scale` | `nlss/scripts/R/scale.R` | Item analysis and reliability (alpha/omega) for scales. | Yes (`scale/default-template.md`) |
| `reliability` | `nlss/scripts/R/reliability.R` | Inter-rater/test-retest reliability (ICC, kappa, correlations). | Yes (`reliability/default-template.md`) |
| `data-explorer` | `nlss/scripts/R/data_explorer.R` | Data dictionary exploration with missingness and level summaries. | Yes (`data-explorer/default-template.md`) |
| `plot` | `nlss/scripts/R/plot.R` | APA-ready figures (histogram, bar, box/violin, scatter/line, QQ, correlation heatmaps). | Yes (`plot/default-template.md`) |
| `data-transform` | `nlss/scripts/R/data_transform.R` | Derived variables, recoding, binning, renaming, and drop operations. | Yes (`data-transform/default-template.md`) |
| `missings` | `nlss/scripts/R/missings.R` | Missing-data patterns, handling decisions, and transformed datasets. | Yes (`missings/default-template.md`) |
| `assumptions` | `nlss/scripts/R/assumptions.R` | Assumption checks for t-tests, ANOVA, regression, mixed models, and SEM. | Yes (`assumptions/ttest-template.md`, `assumptions/anova-template.md`, `assumptions/regression-template.md`, `assumptions/mixed-models-template.md`, `assumptions/sem-template.md`) |
| `regression` | `nlss/scripts/R/regression.R` | Multiple and hierarchical regression (OLS/GLM) with interactions and bootstrap CIs. | Yes (`regression/default-template.md`) |
| `mixed-models` | `nlss/scripts/R/mixed_models.R` | Linear mixed-effects models with random effects and marginal means. | Yes (`mixed-models/default-template.md`, `mixed-models/emmeans-template.md`) |
| `sem` | `nlss/scripts/R/sem.R` | Structural equation modeling (SEM), CFA, mediation, path analysis, invariance. | Yes (`sem/default-template.md`, `sem/cfa-template.md`, `sem/mediation-template.md`, `sem/invariance-template.md`) |
| `anova` | `nlss/scripts/R/anova.R` | Between-, within-, and mixed ANOVA with post-hoc comparisons. | Yes (`anova/default-template.md`, `anova/posthoc-template.md`) |
| `t-test` | `nlss/scripts/R/t_test.R` | One-sample, independent-samples, and paired-samples t-tests. | Yes (`t-test/default-template.md`) |
| `nonparametric` | `nlss/scripts/R/nonparametric.R` | Wilcoxon, Mann-Whitney, Kruskal-Wallis, and Friedman tests. | Yes (`nonparametric/default-template.md`, `nonparametric/posthoc-template.md`) |
| `init-workspace` | `nlss/scripts/R/init_workspace.R` | Initialize workspace folder with scratchpad.md, APA report, and .parquet copies. | Yes (`init-workspace/default-template.md`) |
| `metaskill-runner` | `nlss/scripts/R/metaskill_runner.R` | Log metaskill activations (intent + dataset) for traceability. | Yes (`metaskill-runner/default-template.md`) |

Metaskill specs live under `nlss/references/metaskills/` and are executed by the agent; use `metaskill-runner` to log activations.

Available metaskills:
- `describe-sample`: `nlss/references/metaskills/describe-sample.md`
- `check-instruments`: `nlss/references/metaskills/check-instruments.md`

Reference docs:
- `nlss/references/subskills/descriptive-stats.md`
- `nlss/references/subskills/frequencies.md`
- `nlss/references/subskills/crosstabs.md`
- `nlss/references/subskills/correlations.md`
- `nlss/references/subskills/scale.md`
- `nlss/references/subskills/reliability.md`
- `nlss/references/subskills/data-explorer.md`
- `nlss/references/subskills/plot.md`
- `nlss/references/subskills/data-transform.md`
- `nlss/references/subskills/missings.md`
- `nlss/references/subskills/assumptions.md`
- `nlss/references/subskills/regression.md`
- `nlss/references/subskills/mixed-models.md`
- `nlss/references/subskills/sem.md`
- `nlss/references/subskills/anova.md`
- `nlss/references/subskills/t-test.md`
- `nlss/references/subskills/nonparametric.md`
- `nlss/references/subskills/init-workspace.md`
- `nlss/references/subskills/metaskill-runner.md`
- `nlss/references/metaskills/describe-sample.md`
- `nlss/references/metaskills/check-instruments.md`

## Basic usage by module

Each run writes `apa_report.md` in the output directory and appends to `analysis_log.jsonl` when logging is enabled.

### Descriptive statistics

```bash
Rscript nlss/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

### Frequencies

```bash
Rscript nlss/scripts/R/frequencies.R \
  --csv data.csv --vars gender,condition --group condition
```

### Cross-tabulations

```bash
Rscript nlss/scripts/R/crosstabs.R \
  --csv data.csv --row gender --col condition --group site
```

### Correlations

```bash
Rscript nlss/scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

### Scale analysis

```bash
Rscript nlss/scripts/R/scale.R \
  --csv data.csv --vars item1,item2,item3 --group condition
```

### Reliability analysis

```bash
Rscript nlss/scripts/R/reliability.R \
  --csv data.csv --analysis icc --vars rater1,rater2,rater3
```

### Data exploration

```bash
Rscript nlss/scripts/R/data_explorer.R \
  --csv data.csv --vars age,score --max-levels 15 --top-n 8
```

### Plots

```bash
Rscript nlss/scripts/R/plot.R \
  --csv data.csv --type scatter --x age --y score --group condition
```

### Data transformation

```bash
Rscript nlss/scripts/R/data_transform.R \
  --csv data.csv --standardize age,score
```

### Missing data handling

```bash
Rscript nlss/scripts/R/missings.R \
  --csv data.csv --vars age,score,stress
```

### Metaskill activation logging

```bash
Rscript nlss/scripts/R/metaskill_runner.R \
  --csv data.csv --meta sample-description --intent "describe the sample"
```

### Assumptions checks

```bash
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis ttest --vars score --group condition
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis mixed_models --formula "score ~ time + (1|id)"
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis sem --factors "F1=item1,item2;F2=item3,item4"
```

### Nonparametric tests

```bash
Rscript nlss/scripts/R/nonparametric.R \
  --csv data.csv --vars score --group condition --test mann_whitney
```

### Regression

```bash
Rscript nlss/scripts/R/regression.R \
  --csv data.csv --dv outcome --blocks "age,gender;stress,trait" --interactions stress:trait --center mean
```

### Mixed models

```bash
Rscript nlss/scripts/R/mixed_models.R \
  --csv data.csv --formula "score ~ time + (1|id)"
```

### SEM (lavaan)

```bash
Rscript nlss/scripts/R/sem.R \
  --csv data.csv --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

### ANOVA

```bash
Rscript nlss/scripts/R/anova.R \
  --csv data.csv --dv outcome --between group
```

### t-tests

```bash
Rscript nlss/scripts/R/t_test.R \
  --csv data.csv --vars score --group condition
```

### Workspace initialization

```bash
Rscript nlss/scripts/R/init_workspace.R \
  --csv data.csv
```

## Where outputs go

All scripts write to the dataset workspace at `<workspace-root>/<dataset-name>/` and do not accept a custom output directory. Workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir` in `nlss/scripts/config.yml`).
Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`; `data_transform` and `missings` update these copies in place and create backups in `<workspace-root>/<dataset-name>/backup/`.
Plots are saved under `<workspace-root>/<dataset-name>/plots/` with figure-numbered filenames.

## Configuration logic

Defaults live in `nlss/scripts/config.yml` and are loaded via `nlss/scripts/R/lib/config.R`.

- `defaults.*` apply across all modules (for example `defaults.output_dir`, `defaults.workspace_manifest`).
- `modules.<subskill>.*` holds per-module defaults (for example `modules.crosstabs.percent`).
- `templates.<subskill>.*` controls the template file used for APA outputs (see next section).
- CLI flags always override config values at runtime (for example `--digits`, module-specific flags).
- When `config.yml` is missing or unreadable, built-in defaults in `config.R` are used.

## APA template logic (YAML)

Templates are Markdown files under `nlss/assets/<subskill>/` with YAML front matter. They drive `apa_report.md` output for the subskills that ship with templates (descriptive stats, frequencies, crosstabs, correlations, scale, data exploration, plotting, data transformation, missingness handling, assumptions, regression, SEM, ANOVA, and t-tests).

Key YAML fields:

- `tokens`: static strings you can reference as `{{token}}` in the template body.
- `table.columns`: ordered column specs with `key`, `label`, and optional `drop_if_empty`.
- `note.template`: controls `{{note_body}}` rendering (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: controls `{{narrative}}` rendering; `row_template` repeats over `narrative_rows` and can be joined with `narrative.join`.

Template paths can be overridden in `nlss/scripts/config.yml` under `templates.<subskill>.<name>` (for example `templates.crosstabs.grouped`). Edit the template files or point to your own to change APA output without touching the R scripts.
You can also pass `--template <name|path>` to any subskill to select a configured template reference (for example `default` or `grouped`) or a direct template path; unresolved references fall back to the default template selection.

## Using with Codex (Codes)

Codex discovers this repo's skill via `nlss/SKILL.md`. Open Codex in the repo root and ask for a statistical task; it should route to the correct subskill automatically.

Example prompt:

```
Use nlss to run correlations (Pearson) on data.csv for age, score, and stress.
Write outputs to outputs/tmp and summarize the APA text.
```

## Using with Claude Code

Claude Code can use the same repo structure. Open the repo and tell Claude to use the nlss subskill reference files and scripts.

Example prompt:

```
Use the nlss repo. Run descriptive_stats on data.csv for age and score.
Use outputs/tmp and report the APA narrative and table file names.
```
