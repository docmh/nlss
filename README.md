# NLSS - Natural Language Statistics Suite

R-based statistics helpers that produce an APA 7-ready report plus machine-readable JSONL logs. The repo is organized as "subskills" with a shared workflow and consistent output locations.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Requirements and System Support

- R 4.4+ (base R is enough for CSV/APA outputs).
- `Rscript` available on PATH in the shell where you run NLSS.
- Required R packages: `yaml` (configuration + templates), `jsonlite` (analysis logging), `arrow` (parquet workspace copies), and `ggplot2` (plot subskill).
- Optional R packages: `haven` (preferred) or `foreign` for SPSS `.sav` input support; `mice` or `VIM` for imputation engines; `car` for Type II/III ANOVA sums of squares; `psych` (EFA/PCA, KMO/Bartlett); `GPArotation` for oblique rotations; `lme4` + `performance` for mixed models (required when using mixed-models); `lmerTest` for df/*p*-values; `emmeans` for marginal means/contrasts; `lavaan` for SEM/CFA/mediation; `pwr` for power analysis; `semPower` for SEM power; `viridisLite` for palettes; `influence.ME` and `DHARMa` for mixed-model diagnostics; `MVN` for Mardia test.
- Windows, WSL (Ubuntu), or Linux.

## Rscript Setup (Required)

NLSS runs `.R` scripts directly with `Rscript`. Ensure `Rscript` is available in the same shell where you run NLSS.

### Check Availability

- Windows PowerShell: `Get-Command Rscript` or `Rscript --version`
- WSL/Linux: `which Rscript` or `Rscript --version`

### Install R With Rscript on PATH

- Windows (PowerShell):
  - Install R from CRAN or with `winget install --id RProject.R -e`.
  - Ensure the installer option "Add R to PATH" is enabled, or add `C:\Program Files\R\R-x.y.z\bin` (or `bin\x64`) to PATH manually, then restart the terminal.
- WSL (Ubuntu):
  - `sudo apt update && sudo apt install r-base`

### Windows + WSL Environment Choice

- If `Rscript` is available in WSL but not in Windows PowerShell: prefer switching the Codex IDE to a WSL environment; alternatively install R in Windows.
- If `Rscript` is available in Windows PowerShell but not in WSL: prefer installing R in WSL and switching Codex to WSL; alternatively stay in Windows PowerShell.

Install the R dependencies:

```bash
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('yaml','jsonlite','arrow','ggplot2','haven','foreign','mice','VIM','car','psych','GPArotation','lme4','lmerTest','emmeans','performance','lavaan','pwr','semPower','viridisLite','influence.ME','DHARMa','MVN'))"
```

Or install them interactively in *r*:

```r
install.packages(c('yaml','jsonlite','arrow','ggplot2','haven','foreign','mice','VIM','car','psych','GPArotation','lme4','lmerTest','emmeans','performance','lavaan','pwr','semPower','viridisLite','influence.ME','DHARMa','MVN'))
```

## Open Source and Third-Party Software

NLSS is licensed under Apache-2.0 (see `LICENSE` and `NOTICE`). It relies on open-source R and CRAN packages that are installed separately and remain under their respective licenses.

- Core runtime packages: `yaml`, `jsonlite`, `arrow`, `ggplot2`.
- Optional feature packages: `haven`, `foreign`, `mice`, `VIM`, `car`, `psych`, `GPArotation`, `lme4`, `lmerTest`, `emmeans`, `performance`, `lavaan`, `pwr`, `semPower`.
- Optional packages used when available: `viridisLite`, `influence.ME`, `DHARMa`, `MVN`.
- Test tooling (optional): Python 3 for the test harness.

## Disclaimer and Intended Use

- Provided "AS IS" under Apache-2.0; no warranties or conditions of any kind.
- Users are responsible for validating results and decisions made from them; outputs are aids, not a substitute for expert review.
- Source availability helps transparency, but does not guarantee correctness or fitness for a particular purpose.
- Not intended for safety-critical, medical, legal, or regulatory decision making without independent verification.
- Modified versions may behave differently; anyone distributing changes should review and test their modifications.


## Install

No build step. Clone and run the scripts directly:

```bash
git clone <repo-url>
cd nlss
```

If you are on Windows, ensure `Rscript.exe` is on your PATH.

## Quick Start

Outputs always go to the dataset workspace at `<workspace-root>/<dataset-name>/` and are not user-overridable. Workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml`; if no manifest is present, scripts fall back to `defaults.output_dir` in `nlss/scripts/config.yml`. Each run writes `report_canonical.md` and, when logging is enabled, appends to `analysis_log.jsonl` inside that dataset folder (the monotonic log counter is stored in `nlss-workspace.yml` as `analysis_log_seq`; if `analysis_log.jsonl` is missing, the counter restarts at 1).

### Path Handling

- Displayed paths in console output and reports default to workspace-relative when the target is inside the workspace root; absolute paths are used only for locations outside the workspace.
- Mask workspace-external paths in `scratchpad.md`, `report_canonical.md`, and `analysis_log.jsonl` as `<external>/<filename>`; avoid full absolute external paths in documentation and logs.
- Use workspace-relative paths in examples and configs; use absolute paths when referencing files outside the workspace root.

### Rscript (All Platforms)

```bash
Rscript nlss/scripts/R/descriptive_stats.R --csv data.csv --vars age,score
```

Notes:

- Use Windows paths (for example `C:\path\file.csv`) in Windows PowerShell.
- Use WSL paths (for example `/mnt/c/path/file.csv`) in WSL.

## Stateful Workspace Architecture

nlss is stateful. The workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir`), and each dataset gets its own subfolder.

- Run `nlss/scripts/R/init_workspace.R` to create `<workspace-root>/<dataset-name>/scratchpad.md`, `report_canonical.md`, `analysis_log.jsonl`, a parquet workspace copy, and `nlss-workspace.yml` in the workspace root.
- For any input dataset (CSV/SAV/RDS/RData/Parquet), the workspace copy lives at `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- All subskills read from the workspace parquet copy (prefer `--parquet` pointing to the workspace file or rely on auto-copy).
- When running from a dataset folder without input flags, scripts select that dataset; otherwise they load `active_dataset` from the manifest.
- Workspaces must be non-nested and unique per parent folder; scripts stop if nested or sibling workspace manifests are detected.
- `data-transform` and `missings` update the workspace parquet copy in place and save a backup to `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting.

## Available Modules (Subskills)

Each subskill has a reference file describing inputs, flags, and outputs. Template-driven modules can be customized via `nlss/assets/<subskill>/` and `templates.*` in `nlss/scripts/config.yml`.

| Subskill | Script | Purpose | APA template |
| --- | --- | --- | --- |
| `descriptive-stats` | `nlss/scripts/R/descriptive_stats.R` | Descriptive statistics with APA tables/text. | Yes (`descriptive-stats/default-template.md`, `descriptive-stats/robust-template.md`, `descriptive-stats/distribution-template.md`) |
| `frequencies` | `nlss/scripts/R/frequencies.R` | Frequency tables for categorical variables. | Yes (`frequencies/default-template.md`, `frequencies/grouped-template.md`) |
| `crosstabs` | `nlss/scripts/R/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
| `correlations` | `nlss/scripts/R/correlations.R` | Correlations, partial correlations, diagnostics, bootstrap CIs, Fisher r-to-z comparisons. | Yes (`correlations/default-template.md`, `correlations/cross-correlation-template.md`, `correlations/matrix-template.md`, `correlations/comparison-template.md`) |
| `scale` | `nlss/scripts/R/scale.R` | Item analysis and reliability (alpha/omega) for scales. | Yes (`scale/default-template.md`) |
| `efa` | `nlss/scripts/R/efa.R` | Exploratory factor analysis / PCA with rotation, KMO/Bartlett diagnostics, and loadings tables. | Yes (`efa/default-template.md`) |
| `reliability` | `nlss/scripts/R/reliability.R` | Inter-rater/test-retest reliability (ICC, kappa, correlations). | Yes (`reliability/default-template.md`) |
| `data-explorer` | `nlss/scripts/R/data_explorer.R` | Data dictionary exploration with missingness and level summaries. | Yes (`data-explorer/default-template.md`) |
| `plot` | `nlss/scripts/R/plot.R` | APA-ready figures (histogram, bar, box/violin, scatter/line, QQ, correlation heatmaps). | Yes (`plot/default-template.md`) |
| `data-transform` | `nlss/scripts/R/data_transform.R` | Derived variables, recoding, binning, renaming, and drop operations. | Yes (`data-transform/default-template.md`) |
| `missings` | `nlss/scripts/R/missings.R` | Missing-data patterns, handling decisions, and transformed datasets. | Yes (`missings/default-template.md`) |
| `impute` | `nlss/scripts/R/impute.R` | Imputation into new columns with optional mice/VIM engines. | Yes (`impute/default-template.md`) |
| `assumptions` | `nlss/scripts/R/assumptions.R` | Assumption checks for t-tests, ANOVA, regression, mixed models, and SEM. | Yes (`assumptions/ttest-template.md`, `assumptions/anova-template.md`, `assumptions/regression-template.md`, `assumptions/mixed-models-template.md`, `assumptions/sem-template.md`) |
| `regression` | `nlss/scripts/R/regression.R` | Multiple and hierarchical regression (OLS/GLM) with interactions and bootstrap CIs. | Yes (`regression/default-template.md`) |
| `power` | `nlss/scripts/R/power.R` | Power analysis for t-tests, ANOVA, correlations, regression, and SEM. | Yes (`power/default-template.md`) |
| `mixed-models` | `nlss/scripts/R/mixed_models.R` | Linear mixed-effects models with random effects, marginal means, and planned contrasts. | Yes (`mixed-models/default-template.md`, `mixed-models/emmeans-template.md`) |
| `sem` | `nlss/scripts/R/sem.R` | Structural equation modeling (SEM), CFA, mediation, path analysis, invariance. | Yes (`sem/default-template.md`, `sem/cfa-template.md`, `sem/mediation-template.md`, `sem/invariance-template.md`) |
| `anova` | `nlss/scripts/R/anova.R` | Between-, within-, and mixed ANOVA with post-hoc comparisons and planned contrasts. | Yes (`anova/default-template.md`, `anova/posthoc-template.md`, `anova/contrasts-template.md`) |
| `t-test` | `nlss/scripts/R/t_test.R` | One-sample, independent-samples, and paired-samples t-tests. | Yes (`t-test/default-template.md`) |
| `nonparametric` | `nlss/scripts/R/nonparametric.R` | Wilcoxon, Mann-Whitney, Kruskal-Wallis, and Friedman tests. | Yes (`nonparametric/default-template.md`, `nonparametric/posthoc-template.md`) |
| `init-workspace` | `nlss/scripts/R/init_workspace.R` | Initialize workspace folder with scratchpad.md, APA report, and .parquet copies. | Yes (`init-workspace/default-template.md`) |
| `metaskill-runner` | `nlss/scripts/R/metaskill_runner.R` | Log metaskill activations (intent + dataset) for traceability. | Yes (`metaskill-runner/default-template.md`, `metaskill-runner/finalization-template.md`) |

Metaskill specs live under `nlss/references/metaskills/` and are executed by the agent; use `metaskill-runner` to log activations and finalizations. Exception: `explain-statistics` is conversational and does not require `metaskill-runner` or report outputs unless explicitly requested. Metaskill completion writes `report_<YYYYMMDD>_<metaskill>_<intent>.md` first, then logs finalization with `metaskill-runner --synopsis` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

Default metaskill reports should follow `nlss/assets/metaskills/report-template.md` (APA-style paper sections). Omit Introduction and Keywords when no theoretical context is available, adjust subsections as needed, and design tables/figures specifically for the report rather than copying from `report_canonical.md`.

Available metaskills:

- `explain-statistics`: `nlss/references/metaskills/explain-statistics.md`
- `format-document`: `nlss/references/metaskills/format-document.md`
- `describe-sample`: `nlss/references/metaskills/describe-sample.md`
- `generate-r-script`: `nlss/references/metaskills/generate-r-script.md`
- `check-instruments`: `nlss/references/metaskills/check-instruments.md`
- `plan-power`: `nlss/references/metaskills/plan-power.md`
- `explore-data`: `nlss/references/metaskills/explore-data.md`
- `prepare-data`: `nlss/references/metaskills/prepare-data.md`
- `test-hypotheses`: `nlss/references/metaskills/test-hypotheses.md`

Utilities:

- `calc`: `nlss/references/utilities/calc.md`
- `check-integrity`: `nlss/references/utilities/check-integrity.md`
- `reconstruct-reports`: `nlss/references/utilities/reconstruct-reports.md`

Reference docs:

- `nlss/references/subskills/descriptive-stats.md`
- `nlss/references/subskills/frequencies.md`
- `nlss/references/subskills/crosstabs.md`
- `nlss/references/subskills/correlations.md`
- `nlss/references/subskills/scale.md`
- `nlss/references/subskills/efa.md`
- `nlss/references/subskills/reliability.md`
- `nlss/references/subskills/data-explorer.md`
- `nlss/references/subskills/plot.md`
- `nlss/references/subskills/data-transform.md`
- `nlss/references/subskills/missings.md`
- `nlss/references/subskills/impute.md`
- `nlss/references/subskills/assumptions.md`
- `nlss/references/subskills/regression.md`
- `nlss/references/subskills/power.md`
- `nlss/references/subskills/mixed-models.md`
- `nlss/references/subskills/sem.md`
- `nlss/references/subskills/anova.md`
- `nlss/references/subskills/t-test.md`
- `nlss/references/subskills/nonparametric.md`
- `nlss/references/subskills/init-workspace.md`
- `nlss/references/subskills/metaskill-runner.md`
- `nlss/references/metaskills/describe-sample.md`
- `nlss/references/metaskills/explain-statistics.md`
- `nlss/references/metaskills/format-document.md`
- `nlss/references/metaskills/generate-r-script.md`
- `nlss/references/metaskills/check-instruments.md`
- `nlss/references/metaskills/plan-power.md`
- `nlss/references/metaskills/explore-data.md`
- `nlss/references/metaskills/prepare-data.md`
- `nlss/references/metaskills/test-hypotheses.md`
- `nlss/references/metaskills/formatting/*.md`
- `nlss/references/utilities/calc.md`
- `nlss/references/utilities/check-integrity.md`
- `nlss/references/utilities/reconstruct-reports.md`

## Basic Usage by Module

Each run writes `report_canonical.md` in the output directory and appends to `analysis_log.jsonl` when logging is enabled.

### Descriptive Statistics

```bash
Rscript nlss/scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

### Frequencies

```bash
Rscript nlss/scripts/R/frequencies.R \
  --csv data.csv --vars gender,condition --group condition
```

### Cross-Tabulations

```bash
Rscript nlss/scripts/R/crosstabs.R \
  --csv data.csv --row gender --col condition --group site
```

### Correlations

```bash
Rscript nlss/scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

### Scale Analysis

```bash
Rscript nlss/scripts/R/scale.R \
  --csv data.csv --vars item1,item2,item3 --group condition
```

### Exploratory Factor Analysis

```bash
Rscript nlss/scripts/R/efa.R \
  --csv data.csv --vars item1,item2,item3 --method pca --rotation varimax
```

### Reliability Analysis

```bash
Rscript nlss/scripts/R/reliability.R \
  --csv data.csv --analysis icc --vars rater1,rater2,rater3
```

### Data Exploration

```bash
Rscript nlss/scripts/R/data_explorer.R \
  --csv data.csv --vars age,score --max-levels 15 --top-n 8
```

### Plots

```bash
Rscript nlss/scripts/R/plot.R \
  --csv data.csv --type scatter --x age --y score --group condition
```

### Data Transformation

```bash
Rscript nlss/scripts/R/data_transform.R \
  --csv data.csv --standardize age,score
```

### Missing Data Handling

```bash
Rscript nlss/scripts/R/missings.R \
  --csv data.csv --vars age,score,stress
```

### Metaskill Activation Logging

```bash
Rscript nlss/scripts/R/metaskill_runner.R \
  --csv data.csv --meta sample-description --intent "describe the sample"
```

### Assumptions Checks

```bash
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis ttest --vars score --group condition
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis mixed_models --formula "score ~ time + (1|id)"
Rscript nlss/scripts/R/assumptions.R \
  --csv data.csv --analysis sem --factors "F1=item1,item2;F2=item3,item4"
```

### Nonparametric Tests

```bash
Rscript nlss/scripts/R/nonparametric.R \
  --csv data.csv --vars score --group condition --test mann_whitney
```

### Regression

```bash
Rscript nlss/scripts/R/regression.R \
  --csv data.csv --dv outcome --blocks "age,gender;stress,trait" --interactions stress:trait --center mean
```

### Power Analysis

```bash
Rscript nlss/scripts/R/power.R \
  --csv data.csv --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
```

### Mixed Models

```bash
Rscript nlss/scripts/R/mixed_models.R \
  --csv data.csv --formula "score ~ time + (1|id)"
```

### SEM (Lavaan)

```bash
Rscript nlss/scripts/R/sem.R \
  --csv data.csv --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

### ANOVA

```bash
Rscript nlss/scripts/R/anova.R \
  --csv data.csv --dv outcome --between group
```

### T-Tests

```bash
Rscript nlss/scripts/R/t_test.R \
  --csv data.csv --vars score --group condition
```

### Workspace Initialization

```bash
Rscript nlss/scripts/R/init_workspace.R \
  --csv data.csv
```

## Where Outputs Go

All scripts write to the dataset workspace at `<workspace-root>/<dataset-name>/` and do not accept a custom output directory. Workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir` in `nlss/scripts/config.yml`). Subskills only extend `report_canonical.md` and never create standalone report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.
Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`; `data_transform` and `missings` update these copies in place and create backups in `<workspace-root>/<dataset-name>/backup/`.
Plots are saved under `<workspace-root>/<dataset-name>/plots/` with figure-numbered filenames.

## Configuration Logic

Defaults live in `nlss/scripts/config.yml` and are loaded via `nlss/scripts/R/lib/config.R`.

- `defaults.*` apply across all modules (for example `defaults.output_dir`, `defaults.workspace_manifest`).
- `modules.<subskill>.*` holds per-module defaults (for example `modules.crosstabs.percent`).
- `templates.<subskill>.*` controls the template file used for APA outputs (see next section).
- CLI flags always override config values at runtime (for example `--digits`, module-specific flags).
- When `config.yml` is missing or unreadable, built-in defaults in `config.R` are used.

## APA Template Logic (YAML)

Templates are Markdown files under `nlss/assets/<subskill>/` with YAML front matter. They drive `report_canonical.md` output for the subskills that ship with templates (descriptive stats, frequencies, crosstabs, correlations, scale, data exploration, plotting, data transformation, missingness handling, imputation, assumptions, regression, power, SEM, ANOVA, and t-tests).

Key YAML fields:

- `tokens`: static strings you can reference as `{{token}}` in the template body.
- `table.columns`: ordered column specs with `key`, `label`, and optional `drop_if_empty`.
- `note.template`: controls `{{note_body}}` rendering (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: controls `{{narrative}}` rendering; `row_template` repeats over `narrative_rows` and can be joined with `narrative.join`.

Template paths can be overridden in `nlss/scripts/config.yml` under `templates.<subskill>.<name>` (for example `templates.crosstabs.grouped`). Edit the template files or point to your own to change APA output without touching the R scripts.
You can also pass `--template <name|path>` to any subskill to select a configured template reference (for example `default` or `grouped`) or a direct template path; unresolved references fall back to the default template selection.

## Using With Codex (Codes)

Codex discovers this repo's skill via `nlss/SKILL.md`. Open Codex in the repo root and ask for a statistical task; it should route to the correct subskill automatically.

Example prompt:

```
Use nlss to run correlations (Pearson) on data.csv for age, score, and stress.
Write outputs to outputs/tmp and summarize the APA text.
```

## Using With Claude Code

Claude Code can use the same repo structure. Open the repo and tell Claude to use the nlss subskill reference files and scripts.

Example prompt:

```
Use the nlss repo. Run descriptive_stats on data.csv for age and score.
Use outputs/tmp and report the APA narrative and table file names.
```