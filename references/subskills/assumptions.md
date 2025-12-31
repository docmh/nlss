---
name: assumptions
description: Assumption and diagnostic checks for t-tests, ANOVA, regression, mixed models, and SEM (normality, homogeneity, sphericity, VIF, influence, convergence) with NLSS format outputs.
license: Apache-2.0
---

# Assumptions Checks (NLSS format)

## Overview

Run assumption and diagnostic checks for t-tests, ANOVA (between, within, mixed), regression (including multiple and hierarchical models), mixed models, and SEM/CFA/mediation/path models. The script outputs NLSS format-ready tables/narratives plus a machine-readable JSONL log.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose the analysis family (`ttest`, `anova`, `regression`, `mixed_models`, or `sem`) and specify variables.
3. Run `scripts/R/assumptions.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for reporting or downstream modules.

## Script: `scripts/R/assumptions.R`

### T-Test Assumptions (Independent Samples)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --vars score --group condition
```

### T-Test Assumptions (Paired)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --x pre_score --y post_score
```

### ANOVA Assumptions (Between-Subjects)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --dv score --between group,gender
```

### ANOVA Assumptions (Within or Mixed; Wide Format)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --within pre,mid,post --between group
```

### Regression Assumptions (Multiple Regression)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --ivs age,stress,trait
```

### Regression Assumptions (Hierarchical Blocks)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --blocks age,gender;stress,trait
```

### Mixed Models Assumptions

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis mixed_models --formula "score ~ time + (1|id)"
```

### SEM Assumptions (CFA Builder)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis sem --factors "F1=item1,item2;F2=item3,item4"
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/assumptions.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--analysis` defaults to `modules.assumptions.analysis` (`auto`, `ttest`, `anova`, `regression`, `mixed_models`, `sem`; `cfa`, `path`, `mediation`, `invariance` also route to SEM checks).
- `--vars` defaults to `modules.assumptions.vars_default` (numeric columns).
- `--normality` defaults to `modules.assumptions.normality` (`shapiro` or `none`).
- `--homogeneity` defaults to `modules.assumptions.homogeneity` (`levene`, `bartlett`, `fligner`, `f`, `all`, `none`).
- `--linearity` defaults to `modules.assumptions.linearity` (TRUE/FALSE).
- `--homoscedasticity` defaults to `modules.assumptions.homoscedasticity` (TRUE/FALSE).
- `--vif` defaults to `modules.assumptions.vif` (TRUE/FALSE).
- `--durbin-watson` defaults to `modules.assumptions.durbin_watson` (TRUE/FALSE).
- `--outliers` defaults to `modules.assumptions.outliers` (TRUE/FALSE).
- `--influence` defaults to `modules.assumptions.influence` (TRUE/FALSE).
- `--alpha` defaults to `modules.assumptions.alpha`.
- Thresholds can be overridden via:
  - `--vif-warn` (`modules.assumptions.vif_warn`)
  - `--vif-high` (`modules.assumptions.vif_high`)
  - `--outlier-z` (`modules.assumptions.outlier_z`)
  - `--cook-multiplier` (`modules.assumptions.cook_multiplier`)
  - `--max-shapiro-n` (`modules.assumptions.max_shapiro_n`)
- Mixed models inputs: `--formula` or `--dv` + `--fixed` + `--random`.
  - `--reml` uses `modules.mixed_models.reml`.
  - `--optimizer` uses `modules.mixed_models.optimizer`.
  - `--maxfun` uses `modules.mixed_models.maxfun`.
  - Mixed-model assumption toggles: `--random-effects`, `--singular`, `--convergence`, `--dharma`, `--performance` (defaults from `modules.assumptions.mixed_models.*`).
- SEM inputs: `--model`, `--model-file`, `--paths`, `--factors`, or builders (`--dv`/`--ivs`, `--x`/`--m`/`--y`).
  - `--estimator` uses `modules.sem.estimator`.
  - `--missing` uses `modules.sem.missing`.
  - `--se` uses `modules.sem.se`.
  - `--ci` uses `modules.sem.ci`.
  - `--bootstrap`/`--bootstrap-samples` use `modules.sem.bootstrap`/`modules.sem.bootstrap_samples`.
  - `--std` uses `modules.sem.std`.
  - SEM assumption toggles: `--mardia`, `--mahalanobis`, `--mahalanobis-alpha`, `--collinearity`, `--max-cor`, `--max-kappa`, `--heywood`, `--convergence` (defaults from `modules.assumptions.sem.*`).
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- `ttest`:
  - One-sample/independent: use `--vars` and optional `--group`.
  - Paired: use `--x` and `--y` lists of equal length.
- `anova`:
  - Between-subjects: use `--dv` and `--between`.
  - Within/mixed: use `--within` (wide format, one column per repeated measure) and optional `--between`.
- `regression`:
  - Multiple regression: use `--dv` and `--ivs`.
  - Hierarchical regression: use `--blocks` (semicolon-separated blocks; blocks are cumulative).
- `mixed_models`:
  - Use `--formula` (recommended) or `--dv` + `--fixed` + `--random`.
  - Requires the `lme4` package; random-effects terms are required.
- `sem`:
  - Use `--analysis sem/cfa/path/mediation/invariance` plus `--model`/`--model-file` or a builder (`--factors`, `--dv`/`--ivs`, `--x`/`--m`/`--y`).
  - Optional `--ordered` for ordered categorical indicators; `--group` for multigroup fits.
- Missing values are handled listwise per test or model.

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).
- `report_canonical.md`: NLSS format report containing analysis flags, table, and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).
- Diagnostics include normality tests, homogeneity tests, sphericity (when applicable), regression diagnostics (VIF, Breusch-Pagan, Durbin-Watson, outliers, influence), mixed-model checks (singularity, convergence, random-effects normality, optional DHARMa/performance/influence.ME), and SEM checks (univariate/multivariate normality, Mahalanobis outliers, collinearity, Heywood cases, convergence).

## NLSS format Templates (YAML)

Templates are stored under `assets/assumptions/` and mapped in `scripts/config.yml`:

- `templates.assumptions.ttest`: `assumptions/ttest-template.md`
- `templates.assumptions.anova`: `assumptions/anova-template.md`
- `templates.assumptions.regression`: `assumptions/regression-template.md`
- `templates.assumptions.mixed_models`: `assumptions/mixed-models-template.md`
- `templates.assumptions.sem`: `assumptions/sem-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys include:

`analysis_type`, `model`, `assumption`, `test`, `target`, `group`, `n`, `statistic`, `df`, `p`, `value`, `decision`, `note`.

Use `drop_if_empty: true` to hide columns with all empty values.

### Note Tokens

Available note tokens include:

`note_default`, `alpha`, `homogeneity_tests`, `vif_warn`, `vif_high`, `outlier_z`, `cook_threshold`, `mahalanobis_alpha`, `max_cor`, `max_kappa`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `analysis_type`, `model`, `assumption`, `test`, `target`, `group`, `statistic`, `df`, `p`, `value`, `decision`, `n`.

## NLSS format Reporting Guidance

- Report the assumption tests used (e.g., Shapiro-Wilk, Levene, Mauchly, Breusch-Pagan) with statistics and *p*-values.
- For regression, report VIF values and any influence/outlier flags.
- For mixed models, report singularity/convergence flags, residual diagnostics, and any random-effects normality/outlier findings.
- For SEM, report multivariate normality (Mardia), Mahalanobis outliers, collinearity/Heywood cases, and convergence status.
- Note any violations and consider corrections or robust alternatives when assumptions are not met.

## Dependencies

- Parquet input requires the R package `arrow`.
- Mixed-model assumptions require the R package `lme4`.
- SEM assumptions require the R package `lavaan`.
- Optional checks use `performance`, `DHARMa`, `influence.ME`, and `MVN` when available.