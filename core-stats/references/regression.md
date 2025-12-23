---
name: regression-r
description: Multiple and hierarchical regression (OLS/GLM) with interactions, bootstrap CIs, and APA-ready tables/narratives.
---

# Regression (Base R, APA 7)

## Overview

Run linear regression (OLS) or generalized linear models (binomial/logistic, Poisson) with optional hierarchical blocks, interaction terms, grouping, and bootstrap confidence intervals. Outputs include APA 7-ready tables and narratives plus JSONL logs.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Specify the dependent variable and predictors (either `--ivs` or hierarchical `--blocks`).
3. Optionally add `--interactions`, `--center`, and bootstrap options.
4. Run `scripts/R/regression.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`apa_report.md`, `analysis_log.jsonl`) for APA reporting.

## Script: `scripts/R/regression.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\regression.R> --csv <path to CSV file> --dv outcome --ivs x1,x2,x3
```

### Linear regression (CSV)

```bash
Rscript <path to scripts/R/regression.R> --csv <path to CSV file> --dv outcome --ivs x1,x2,x3
```

### Hierarchical regression (blocks)

```bash
Rscript <path to scripts/R/regression.R> --csv <path to CSV file> --dv outcome --blocks "age,gender;stress,trait"
```

### Moderation via interaction terms

```bash
Rscript <path to scripts/R/regression.R> --csv <path to CSV file> --dv outcome --ivs x1,moderator --interactions x1:moderator --center mean
```

### Grouped regressions

```bash
Rscript <path to scripts/R/regression.R> --csv <path to CSV file> --dv outcome --ivs x1,x2 --group site
```

### Logistic regression (binomial)

```bash
Rscript <path to scripts/R/regression.R> --csv <path to CSV file> --dv binary_outcome --ivs x1,x2 --family binomial
```

### Parquet input

```bash
Rscript <path to scripts/R/regression.R> --parquet <path to parquet file> --dv outcome --ivs x1,x2
```

### Interactive prompts

```bash
Rscript <path to scripts/R/regression.R> --interactive
```

## Options

Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--dv` required dependent variable.
- `--ivs` uses `modules.regression.ivs_default` when omitted (typically numeric columns).
- `--blocks` enables hierarchical blocks (semicolon-separated; blocks are cumulative).
- `--interactions` adds interaction terms (comma-separated; supports `:` or `*`).
- `--group` optionally splits the analysis by group levels.
- `--family` uses `modules.regression.family` (`gaussian`, `binomial`, `poisson`).
- `--link` uses `modules.regression.link` (blank uses the family default).
- `--center` uses `modules.regression.center` (`none`, `mean`).
- `--standardize` uses `modules.regression.standardize` (`none`, `predictors`) to report standardized betas.
- `--conf-level` uses `modules.regression.conf_level`.
- `--bootstrap` and `--bootstrap-samples` use `modules.regression.bootstrap` and `modules.regression.bootstrap_samples`.
- `--seed` sets the random seed for bootstrap resampling (optional).
- `--digits` uses `defaults.digits`.
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Non-numeric predictors are coerced to factors.
- For `gaussian` and `poisson`, the dependent variable must be numeric.
- For `binomial`, the dependent variable can be numeric (0/1) or a two-level factor.
- Missing values are removed listwise across all variables used in the full model.
- Standardized betas (`--standardize predictors`) are reported for OLS models only.
- Interaction variables are added to Block 1 if missing and are appended as a final block for hierarchical change testing.

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).
- `apa_report.md`: APA 7 report containing regression coefficients and narrative summaries.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `coefficients_df`, `summary_df`, `comparisons_df`, and `diagnostics_df` (Shapiro-Wilk residual checks for OLS).

## APA 7 Templates (YAML)

Templates are stored under `core-stats/assets/regression/` and mapped in `core-stats/scripts/config.yml`:

- `templates.regression.default`: `regression/default-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`model`, `group`, `term`, `b`, `se`, `t`, `z`, `p`, `ci_low`, `ci_high`, `beta`, `exp_b`, `exp_ci_low`, `exp_ci_high`, `boot_ci_low`, `boot_ci_high`.

### Note tokens

Available note tokens include:

`note_default`.

### Narrative tokens

Use `narrative.row_template` for per-model lines. Available row tokens include:

`full_sentence`, `model`, `group`, `n`, `f`, `df1`, `df2`, `p`, `r2`, `adj_r2`, `rmse`, `chisq`, `pseudo_r2`,
`delta_r2`, `delta_f`, `delta_df1`, `delta_df2`, `delta_p`, `delta_deviance`, `delta_chisq`.

## APA 7 Reporting Guidance

- Report model fit (F/Chi-square, df, p, R2/pseudo R2) for each block.
- Report unstandardized coefficients (b), SE, and p-values; include standardized betas when requested.
- For logistic/Poisson models, report exp(b) as odds ratios or incidence rate ratios.
- If hierarchical blocks are used, report change statistics (Delta R2 or Delta deviance).

## Dependencies

- Parquet input requires the R package `arrow`.
