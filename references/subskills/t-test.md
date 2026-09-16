---
name: t-test
description: One-sample, independent, and paired t-tests with effect sizes (d), confidence intervals, variance/normality diagnostics, optional bootstrap CIs, and NLSS format tables/narratives.
license: Apache-2.0
---

# T-Tests (Base R, NLSS format)

## Overview

Run one-sample, independent-samples, or paired-samples t-tests in base R and generate NLSS format-ready tables and narratives. Outputs include means, standard deviations, mean differences, t, df, p, and Cohen's d. Optional bootstrap confidence intervals are available.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose the t-test type and specify variables (one-sample/independent) or paired measures (paired).
3. Run `scripts/R/t_test.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `result.json`) for NLSS format reporting.

## Script: `scripts/R/t_test.R`

Run with `Rscript`. Statistical estimates use base R `stats`; the shared workspace,
import and run infrastructure also requires its documented packages (including
`yaml`, `jsonlite`, `digest` and `arrow`; `haven` for SAV).

### One-Sample T-Test (CSV)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --vars age,score --mu 0
```

### Independent-Samples T-Test (CSV, Two Groups)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --vars stress --group condition
```

### Paired-Samples T-Test (CSV, Repeated Measures)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --x pre_score --y post_score
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/t_test.R> --rds <path to RDS file> --vars age,score
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/t_test.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet Input

```bash
Rscript <path to scripts/R/t_test.R> --parquet <path to parquet file> --vars age,score
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/t_test.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--vars` defaults to `modules.t_test.vars_default` (typically numeric columns).
- `--mu` uses `modules.t_test.mu` for one-sample tests only. Passing it with paired
  or independent tests is an error; those procedures test a zero mean difference.
- `--alternative` uses `modules.t_test.alternative` (`two.sided`, `greater`, `less`).
- `--var-equal` uses `modules.t_test.var_equal` for independent tests.
- `--conf-level` uses `modules.t_test.conf_level`.
- `--bootstrap` uses `modules.t_test.bootstrap` and `--bootstrap-samples` uses `modules.t_test.bootstrap_samples`.
- `--seed` overrides `modules.t_test.seed` (default 1). Bootstrap runs save the
  effective seed, RNG kind and initial/final state for deterministic replay.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` supplies the original AI prompt; storage follows the shared
  privacy configuration. Always supply the user's analysis request when running
  an analysis on their behalf.
- `--interactive FALSE` does not prompt.
- `--expect-two-groups` retains the legacy exit-0 negative-test convention when
  the grouping variable has other than two observed non-missing values. The run
  remains **failed**, has no normal output, releases its lock, and is not replayable.
- Invalid alternatives fail rather than silently becoming two-sided. Confidence
  levels must be strictly between 0 and 1; bootstrap samples must be an integer
  from 2 to R's integer limit, seeds a non-negative R integer, and digits 0–15.
  The test value must be finite.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Independent tests require a `--group` variable with exactly two non-missing levels.
- Group 1 is the first observed non-missing raw value; group 2 is the next.
  Unused factor levels do not count. Declared factor order and SPSS display labels
  do not silently change this direction. Inspect the saved order before interpreting
  signs or one-sided hypotheses.
- Run-local group IDs link results and diagnostics to the exact raw values and
  input rows in `design.group_order`. Numeric/date-time values also retain
  `value_hex` because JSON's ordinary representation can lose subsecond or
  adjacent-double precision. If distinct values or supplied labels have
  the same display text, the labels add `[group 1]` / `[group 2]`; display text
  must never be used to reconstruct group membership.
- Paired tests require `--x` and `--y` lists of equal length (paired by position).
- Paired tests cannot also select `--vars` or `--group`; repeated identical pairs
  and a variable paired with itself are errors. Repeated one-sample/independent
  variables are also errors, not silently deduplicated requests.
- Missing values are removed per test, jointly within each pair or dependent
  variable/group combination. Missing group values are excluded, not analysed as
  a third group. Saved requests identify exact retained/excluded input-snapshot rows,
  per-variable missing rows and independent-group/paired-row mappings.
- SPSS missing codes are normalized and labels preserved by the
  [import contract](../import-contract.md). Numeric labels do not turn scores into
  factors. No implicit numeric coercion is performed; non-numeric selected scores
  and infinite observations are rejected.
- Every requested test must be estimable. Fewer than two observations per sample,
  constant differences or other `stats::t.test` failures fail the entire run rather
  than publish a table with empty primary statistics. This also applies when
  default numeric selection includes an unusable variable; select the intended
  analysable measures explicitly instead of silently dropping them.

## Estimands, intervals and diagnostics

`t`, degrees of freedom, p values, standard errors and analytic `ci_low`/`ci_high`
come directly from [`stats::t.test`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/t.test.html).
`greater` and `less` retain the corresponding unbounded one-sided intervals.

- One-sample: `mean_diff = mean(x) - mu`; native analytic CI fields concern the
  **population mean**. Additional `diff_ci_low`/`diff_ci_high` subtract `mu` and
  concern the same null-relative quantity as `mean_diff` and the bootstrap CI.
- Independent: differences are group 1 minus group 2. `--var-equal FALSE` uses
  Welch inference; `TRUE` uses the pooled-variance t-test. Cohen's `d` always uses
  the pooled sample SD; it is not a Welch-specific or heteroscedastic effect size.
- Paired: differences are measure 1 minus measure 2 for the retained pairs. `d`
  denotes **d-z**, mean difference divided by the SD of differences, not the
  average of the separate measurement SDs.

Bootstrap intervals are percentile intervals using R quantile type 7, with the
same alternative as the test. Resampling is within each independent group, or
over single-sample observations/paired differences. Mean-difference and `d`
statistics use separate sequential resampling streams in recorded test order.
For one-sample tests, bootstrap means target `mean - mu`, not the raw mean.
Zero-SD standardized resamples are excluded with warnings and explicit valid/
discarded counts; fewer than two finite statistics yields an unavailable bootstrap
interval, while an otherwise valid analytic test remains available. This is not
a BCa/studentized interval, and a finite resample count alone does not establish
adequate bootstrap precision.

JSON records interval estimands, `d_definition`, availability and signed-infinity
statuses. A JSON `null` bound alone must not be interpreted as a missing CI: use
its accompanying status to distinguish a legitimate unbounded one-sided interval.

Shapiro–Wilk is computed per independent sample, on paired **differences**, or on
the one-sample values. Its documented 3–5000-observation restriction and constant
samples are recorded as unavailable with a reason; see
[`stats::shapiro.test`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html).
Independent samples additionally retain the two-sided normal-theory variance-ratio
test (`stats::var.test`), with explicit availability. Diagnostics do not automatically
switch the preselected test or establish its design assumptions.

## Outputs

This migrated module follows the [shared run contract](../run-contract.md):
project-local `.nlss/runs/<id>/request.json`, `result.json`, deterministic `output.md`
and frozen templates are mandatory. `--log FALSE` affects optional standalone logging only.
Only completed, published runs can be replayed; failed or pending directories
are not final statistical results. The working Parquet is not modified.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- Diagnostics (Shapiro-Wilk per sample and variance tests for independent samples) are included in the saved result.
- Failed dataset-backed analyses retain their failure context in `result.json`;
  they do not append normal canonical output or a success-shaped legacy log entry.

## NLSS format Templates

Use the Markdown template in `assets/t-test/default-template.md` when assembling t-test reports.

### YAML Template Controls

- Template path can be overridden via `templates.t_test.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`test_type`, `variable`, `measure_1`, `measure_2`, `group_1`, `group_2`, `n_1`, `n_2`, `mean_1`, `mean_2`,
`sd_1`, `sd_2`, `mean_diff`, `t`, `df`, `p`, `d`, `ci_low`, `ci_high`, `boot_ci_low`, `boot_ci_high`,
`boot_d_ci_low`, `boot_d_ci_high`.

Additional optional keys are `diff_ci_low`, `diff_ci_high`, `ci_estimand`,
`diff_ci_estimand` and `d_definition`.

Use `drop_if_empty: true` to remove columns that are unused (for example, `group_1` in one-sample tests).

### Note Tokens

Available note tokens include:

`note_default`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `test_type`, `variable`, `measure_1`, `measure_2`, `group_1`, `group_2`, `n_1`, `n_2`,
`mean_1`, `mean_2`, `sd_1`, `sd_2`, `mean_diff`, `t`, `df`, `p`, `d`, `ci`, `conf_level`, `mu`.

## NLSS format Reporting Guidance

- Report test type, sample sizes, means/SDs, t, df, p, and Cohen's d.
- Include confidence intervals for the mean difference or mean when available.
- For independent samples, report group labels and whether equal variances were assumed.
- For paired tests, report both measures and the mean difference.
- Interpret the scientific question, direction, measurement scale, uncertainty,
  missingness and design together. The deterministic table/narrative is evidence
  for a freely structured semantic final report, not its replacement.
