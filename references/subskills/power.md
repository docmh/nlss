---
name: power
description: A priori/post hoc/sensitivity power for t-tests, ANOVA, correlations, regression, and SEM (RMSEA), with optional effect-size estimation from data and NLSS format outputs.
license: Apache-2.0
---

# Power Analysis (Pwr + semPower, NLSS format)

## Overview

Run a priori, post hoc, or sensitivity power analyses for t-tests, ANOVA, correlations, regression, and SEM (RMSEA-based) and produce NLSS format-ready tables and narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Inspect data when used; for parameter-only planning, clarify the research design and effect assumptions. Document decisions in the applicable scratchpad and write a context-sensitive final report when requested.

## Core Workflow

1. Choose parameter-only planning or dataset-backed execution. No dataset is needed when the design and effect are specified; pilot estimation requires an explicit dataset and variable roles.
2. Choose `--analysis` (ttest/anova/correlation/regression/sem) and `--mode` (apriori/posthoc/sensitivity).
3. Provide effect size (or set `--estimate-effect TRUE` with the needed variables), alpha, and power targets.
4. Run the `power` operation through `run_nlss.R` with the appropriate flags.
5. Review the immutable run bundle, warnings and canonical output; write the final research interpretation semantically, using [plan-power](../metaskills/plan-power.md).

## Input modes and provenance

With no source flag, and with effect estimation disabled,
Power uses parameter-only planning, even if a dataset is active. `--planning TRUE`
makes this explicit and rejects simultaneous dataset selection or effect
estimation. `--planning FALSE` loads the current/active dataset as before.
Explicit CSV/SAV/RDS/RData/Parquet inputs remain dataset-backed even when the
effect size is supplied numerically. Source-backed runs use the shared
[import contract](../import-contract.md), including SPSS labels and user-missings.
`--dataset-name` names a source import; it is not an active-dataset selector and
requires an explicit input file. CSV reader settings require `--csv` and
`--import-action` requires a source. Inapplicable explicitly supplied design
options are errors; canonical defaults for other families do not constrain the
selected family. Variable roles without effect estimation are context only.

Planning runs in a current project use `.nlss/runs/` and the root protocol,
never a dummy dataset or a separate project planning folder. Deliberately
unmarked standalone planning retains its configured planning destination and
collision checks. See [the run contract](../run-contract.md) for schema-2
parameter input, schema-1 dataset input, replay and locking.

## Execution: `power`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

### Examples

```bash
# t-test (a priori)
Rscript "<skill>/scripts/R/run_nlss.R" power --planning TRUE --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8 --effect-basis "Researcher-approved planning assumption"

# ANOVA (sensitivity)
Rscript "<skill>/scripts/R/run_nlss.R" power --parquet <path> --analysis anova --mode sensitivity --groups 3 --n-per-group 30 --power 0.8

# Correlation (post hoc)
Rscript "<skill>/scripts/R/run_nlss.R" power --parquet <path> --analysis correlation --mode posthoc --effect-size 0.3 --n 120

# Regression (a priori)
Rscript "<skill>/scripts/R/run_nlss.R" power --parquet <path> --analysis regression --mode apriori --effect-metric f2 --effect-size 0.15 --u 3

# SEM (RMSEA; a priori)
Rscript "<skill>/scripts/R/run_nlss.R" power --planning TRUE --analysis sem --mode apriori --sem-df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
```

### Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--analysis` uses `modules.power.analysis` (`ttest`, `anova`, `correlation`, `regression`, `sem`).
- `--mode` uses `modules.power.mode` (`apriori`, `posthoc`, `sensitivity`).
- `--effect-size` numeric value; required unless `--estimate-effect TRUE` or `--mode sensitivity`.
- `--effect-basis` free-text justification/source supplied by the researcher; recorded as context, not independently verified evidence.
- `--planning TRUE/FALSE` explicitly selects parameter-only or dataset-backed execution; see input modes above.
- `--effect-metric` uses `modules.power.effect_metric` (`d`, `f`, `f2`, `r`, `eta2`, `r2`, `rmsea`).
- `--alpha` uses `modules.power.alpha`.
- `--power` uses `modules.power.power`.
- `--alternative` uses `modules.power.alternative` (two.sided/greater/less; t-test/correlation).
- `--t-type` uses `modules.power.t_type` (one-sample/two-sample/paired; t-test only).
- `--ratio` uses `modules.power.ratio` (n2/n1 for two-sample t-tests).
- `--mu` uses `modules.power.mu` (one-sample test value).
- `--n` or `--n-total` sets total sample size (post hoc/sensitivity).
- `--n-per-group` sets ANOVA per-group size or two-sample t-test group-1 size (`n2 = ceiling(n1 * ratio)`).
- `--n1` / `--n2` set two-sample sizes explicitly.
- `--groups` uses `modules.power.groups` (ANOVA `k`).
- `--u` uses `modules.power.u` (regression numerator degrees of freedom, not necessarily the number of named variables).
- `--sem-df` sets SEM degrees of freedom. Legacy numeric `--df` is retained as an alias without `--rdata`; with RData, `--df` selects the data-frame object and `--sem-df` supplies the model degrees of freedom.
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
- `--user-prompt` stores the AI user prompt in the saved request.

### Effect Estimation (Optional)

Set `--estimate-effect TRUE` and provide variables:

- t-test: `--vars` (one-sample/independent) and optionally `--group` for independent; `--x` and `--y` for paired.
- ANOVA: `--dv` and `--group` (or `--between`).
- Correlation: `--x` and `--y`.
- Regression: `--dv` and `--ivs`.
- SEM power does not support `--estimate-effect`.

Effect sizes are estimated from the dataset and then used for power calculations.
Only the selected roles determine complete-case inclusion; actual retained and
excluded source rows, variable types, labels and fitted design are preserved.
Numeric labels do not make predictors categorical: declare statistical roles
deliberately. Grouping variables are categorical; regression preserves existing
factor/character predictors and their fitted model-matrix rank.

Independent t-test effects are signed group-1 minus group-2 means divided by the
pooled SD; group order and membership are recorded. Paired effects use the SD of
the difference scores and sample size counts complete pairs. One-sample `--mu`
is the comparison value. Correlation estimation uses Pearson's r. ANOVA uses
one-way eta-squared and the observed number of groups; regression uses omnibus
R-squared and fitted rank minus one. Explicit `--groups`/`--u` must not contradict
the pilot model. Pilot uncertainty is disclosed but not propagated automatically.

## Numerical meaning and limits

- One-sided calculations preserve signed effects. An opposite-direction effect
  cannot attain an a priori target above alpha; it is not silently made positive.
- A priori output distinguishes the continuous solution, reported integer sample
  sizes, target power and recomputed attained power. For two groups, `n1` is
  rounded up and `n2 = ceiling(n1 * ratio)`; report the effective allocation.
- ANOVA power is a balanced one-way fixed-effects approximation. Unequal pilot
  groups do not turn it into exact unequal-group or repeated-measures power.
  When fixed total N is not divisible by k, the usable balanced allocation is
  rounded down and disclosed. Regression power is an omnibus linear-model F
  test, not logistic, incremental-block or multilevel power.
- SEM uses both RMSEA0 and RMSEA1. The noncentral chi-square null determines the
  cutoff, with noncentrality `(N - 1) * df * RMSEA^2`. A higher alternative uses
  the upper tail; a lower alternative uses the lower tail. Sensitivity solves a
  higher alternative RMSEA (upper-tail test). The saved `semPower` exact-fit
  reference is identified separately from the requested noncentral-null result.
  These distinctions follow the [semTools RMSEA documentation](https://search.r-project.org/CRAN/refmans/semTools/html/findRMSEApower.html)
  and [semPower manual](https://moshagen.github.io/semPower/); semTools is not a new runtime dependency.
- The module does not add clustered/survey designs, multiple-group RMSEA power,
  arbitrary SEM path power, automatic multiplicity/attrition adjustment or MI
  pooling. Existing method scope remains available with explicit limits.

### Package Requirements

- `pwr` is required for t-tests, ANOVA, correlations, and regression.
- `semPower` is required for SEM (RMSEA) power.

## Outputs

Every completed run publishes `.nlss/runs/<id>/request.json`, `result.json`, `output.md`
and exact template copies. Raw results include the summary, calculation engine
and package results, continuous and integer sample-size information, attained
power, effect-estimation evidence and explanatory notes. `output.md` is the
deterministic SPSS-like output, not a semantic final report. `--log FALSE` disables
only the optional standalone log; the run bundle remains mandatory.

- Both input modes use the current project's `.nlss/runs/<run-id>/` and automatic root `report_canonical.md`; `defaults.output_dir` applies to deliberately unmarked standalone execution.
- `report_canonical.md`: NLSS format report containing the power analysis table and narrative.
- `result.json`: Machine-readable results and options, always retained in the saved run.

## NLSS format Templates (YAML)

Use the Markdown template in `assets/power/default-template.md` when assembling power reports.

- Template path can be overridden via `templates.power.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders.

### Table Column Keys

Available column keys include:

`analysis`, `mode`, `effect_metric`, `effect_size`, `alpha`, `power`, `attained_power`, `n_total`, `n_per_group`, `n1`, `n2`, `groups`, `ratio`, `u`, `df`, `r2`, `rmsea0`, `rmsea1`, `t_type`, `alternative`, `effect_source`.

### Note Tokens

Available note tokens include:

`note_default`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`.

## NLSS format Reporting Guidance

- Report the analysis type, effect size metric/value, alpha, power target, and resulting sample size (or achieved power for post hoc).
- For sensitivity analyses, report the minimum detectable effect.
- When effect sizes are estimated from data, note that they are sample-based and used for planning.
- Explain the design/effect justification and uncertainty in context; templates do not establish scientific adequacy. In particular, data-derived post-hoc power is not independent evidence validating an observed result.
- Earlier one-sided, nonzero-null SEM, or pilot ANOVA/regression results affected by the corrected direction/null/design handling should be recomputed; replay compatibility alone is not a scientific endorsement of an older result.
