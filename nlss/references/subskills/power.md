---
name: power
description: A priori/post hoc/sensitivity power for t-tests, ANOVA, correlations, regression, and SEM (RMSEA), with optional effect-size estimation from data and APA outputs.
---

# Power Analysis (Pwr + semPower, APA 7)

## Overview

Run a priori, post hoc, or sensitivity power analyses for t-tests, ANOVA, correlations, regression, and SEM (RMSEA-based) and produce APA 7-ready tables and narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose `--analysis` (ttest/anova/correlation/regression/sem) and `--mode` (apriori/posthoc/sensitivity).
3. Provide effect size (or set `--estimate-effect TRUE` with the needed variables), alpha, and power targets.
4. Run `scripts/R/power.R` with the appropriate flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for APA reporting.

## Script: `scripts/R/power.R`

### Examples

```bash
# t-test (a priori)
Rscript <path to scripts/R/power.R> --parquet <path> --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8

# ANOVA (sensitivity)
Rscript <path to scripts/R/power.R> --parquet <path> --analysis anova --mode sensitivity --groups 3 --n-per-group 30 --power 0.8

# Correlation (post hoc)
Rscript <path to scripts/R/power.R> --parquet <path> --analysis correlation --mode posthoc --effect-size 0.3 --n 120

# Regression (a priori)
Rscript <path to scripts/R/power.R> --parquet <path> --analysis regression --mode apriori --effect-metric f2 --effect-size 0.15 --u 3

# SEM (RMSEA; a priori)
Rscript <path to scripts/R/power.R> --parquet <path> --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
```

### Options

Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--analysis` uses `modules.power.analysis` (`ttest`, `anova`, `correlation`, `regression`, `sem`).
- `--mode` uses `modules.power.mode` (`apriori`, `posthoc`, `sensitivity`).
- `--effect-size` numeric value; required unless `--estimate-effect TRUE` or `--mode sensitivity`.
- `--effect-metric` uses `modules.power.effect_metric` (`d`, `f`, `f2`, `r`, `eta2`, `r2`, `rmsea`).
- `--alpha` uses `modules.power.alpha`.
- `--power` uses `modules.power.power`.
- `--alternative` uses `modules.power.alternative` (two.sided/greater/less; t-test/correlation).
- `--t-type` uses `modules.power.t_type` (one-sample/two-sample/paired; t-test only).
- `--ratio` uses `modules.power.ratio` (n2/n1 for two-sample t-tests).
- `--mu` uses `modules.power.mu` (one-sample test value).
- `--n` or `--n-total` sets total sample size (post hoc/sensitivity).
- `--n-per-group` sets per-group size (ANOVA, two-sample t-test).
- `--n1` / `--n2` set two-sample sizes explicitly.
- `--groups` uses `modules.power.groups` (ANOVA `k`).
- `--u` uses `modules.power.u` (regression predictors).
- `--df` sets SEM degrees of freedom.
- `--rmsea0` / `--rmsea1` set RMSEA under H0/H1 for SEM power.
- `--estimate-effect` uses `modules.power.estimate_effect` (TRUE/FALSE; supported for ttest/anova/correlation/regression).
- `--vars`, `--group`/`--between`, `--x`, `--y`, `--dv`, `--ivs` provide variables for effect estimation.

Effect metric constraints:

- t-test: `d`
- ANOVA: `f` or `eta2`
- Correlation: `r`
- Regression: `f2` or `r2`
- SEM: `rmsea`
- `--digits` uses `defaults.digits`.
- `--template` selects a template key/path (defaults to `templates.power.default`).
- `--log` uses `defaults.log` (TRUE/FALSE).
- `--user-prompt` stores the AI user prompt in the JSONL log.

### Effect Estimation (Optional)

Set `--estimate-effect TRUE` and provide variables:

- t-test: `--vars` (one-sample/independent) and optionally `--group` for independent; `--x` and `--y` for paired.
- ANOVA: `--dv` and `--group` (or `--between`).
- Correlation: `--x` and `--y`.
- Regression: `--dv` and `--ivs`.
- SEM power does not support `--estimate-effect`.

Effect sizes are estimated from the dataset and then used for power calculations.

### Package Requirements

- `pwr` is required for t-tests, ANOVA, correlations, and regression.
- `semPower` is required for SEM (RMSEA) power.

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- `report_canonical.md`: APA 7 report containing the power analysis table and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates (YAML)

Use the Markdown template in `nlss/assets/power/default-template.md` when assembling power reports.

- Template path can be overridden via `templates.power.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders.

### Table Column Keys

Available column keys include:

`analysis`, `mode`, `effect_metric`, `effect_size`, `alpha`, `power`, `n_total`, `n_per_group`, `n1`, `n2`, `groups`, `ratio`, `u`, `df`, `r2`, `rmsea0`, `rmsea1`, `t_type`, `alternative`, `effect_source`.

### Note Tokens

Available note tokens include:

`note_default`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`.

## APA 7 Reporting Guidance

- Report the analysis type, effect size metric/value, alpha, power target, and resulting sample size (or achieved power for post hoc).
- For sensitivity analyses, report the minimum detectable effect.
- When effect sizes are estimated from data, note that they are sample-based and used for planning.