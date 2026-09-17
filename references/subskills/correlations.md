---
name: correlations
description: Pearson/Spearman/Kendall correlations as matrices or cross-sets, with partial controls, bootstrap CIs, Fisher r-to-z tests, p-adjustments, grouping, and NLSS format outputs.
license: Apache-2.0
---

# Correlations (Base R, NLSS format)

## Overview

Compute correlations for numeric variables (pairwise or matrix), with optional grouping, partial correlations, bootstrap CIs, Fisher r-to-z comparisons, and matrix-layout templates. The migrated entrypoint uses the [shared run contract](../run-contract.md): fixed input, resolved choices, numerical results and deterministic per-run Markdown are preserved alongside the automatic root protocol.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables: full matrix via `--vars` (or default numeric columns), or cross-correlation via `--x` and `--y`.
3. Choose correlation method (Pearson/Spearman/Kendall), missing-data handling, and any control variables.
4. Run the `correlations` operation through `run_nlss.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `result.json`) to craft the response.

## Execution: `correlations`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

Run with `Rscript`. Statistical estimation uses base R; shared import, configuration and audit dependencies still apply (`arrow`, `yaml`, `jsonlite`, `digest`; `haven` for SAV).

### CSV Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --vars age,score,stress
```

### Cross-Correlation Between Sets

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --x age,stress --y wellbeing,performance
```

### Grouped Correlations

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --vars age,score --group condition
```

### Fisher R-to-Z vs Theoretical Value

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --vars age,score --r0 0.3
```

### Fisher R-to-Z Between Groups

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --vars age,score --group condition --compare-groups TRUE
```

### Partial Correlations (Controls)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --csv <path to CSV file> --vars age,score --controls gender,education
```

### RDS Input (Data Frame)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --rds <path to RDS file> --vars age,score
```

### RData Input (Data Frame by Name)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --parquet <path to parquet file> --vars age,score
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" correlations --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.correlations.vars_default` (typically numeric columns) if omitted; automatic selection excludes grouping and control variables.
- `--x` and `--y` compute cross-correlations between two sets. Supply either these sets or `--vars`, not both. A single supplied set retains the legacy within-set matrix behavior. Duplicate names within a selection are errors; cross-set overlap excludes self-correlations and repeats each unordered pair only once.
- `--group` is optional and produces grouped correlations.
- `--method` selects `pearson`, `spearman`, or `kendall` (default: `modules.correlations.method`).
- `--missing` selects `pairwise` or `complete` (default: `modules.correlations.missing`).
- `--alternative` selects `two.sided`, `greater`, or `less` (default: `modules.correlations.alternative`).
- `--controls` enables partial correlations (default: `modules.correlations.controls`; not supported for Kendall). Controls must be distinct from correlated and grouping variables.
- `--p-adjust` adjusts p-values (`none`, `bonferroni`, `holm`, `hochberg`, `hommel`, `BH`, `BY`, `fdr`). Default: `modules.correlations.p_adjust`.
- `--conf-level` sets the Fisher z confidence level for Pearson/partial (default: `modules.correlations.conf_level`).
- `--bootstrap` enables bootstrap confidence intervals (default: `modules.correlations.bootstrap`).
- `--bootstrap-samples` sets bootstrap resamples (integer at least 2; default: `modules.correlations.bootstrap_samples`).
- `--seed` sets the random seed for bootstrap resampling; otherwise `modules.correlations.seed` (default 1) applies. The seed and RNG state are saved, including when the caller omitted `--seed`.
- `--r0` sets the Fisher r-to-z comparison value (optional; must be between -1 and 1).
- `--compare-groups` compares correlations between two independent groups (default: `modules.correlations.compare_groups`; requires `--group` with exactly two non-missing levels).
  - Fisher r-to-z comparisons are not supported for Kendall's tau.
  - `--p-adjust` applies to correlation p-values; Fisher r-to-z comparison p-values are unadjusted.
- `--coerce` explicitly converts non-numeric analysis variables to numeric (default: `modules.correlations.coerce`). Factor values are parsed from their text labels, never their internal level indices. Newly introduced missing values are warned about and their exact input rows are recorded. This legacy analysis option does not weaken strict import-type conversion.
- `--digits` controls rounding (default: `defaults.digits`).
- `--interactive` prompts for inputs.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` supplies the original AI prompt (agents should supply it); storage in the saved request context follows the shared privacy configuration.

Unknown methods/alternatives, invalid numeric domains, non-finite analysis values and ambiguous variable selections fail explicitly rather than silently reverting to defaults. Confidence levels must lie strictly between 0 and 1; digits must be an integer from 0 to 15. These checks also apply to configured defaults.

## Estimands, inference and missing cases

- Ordinary Pearson, Spearman and Kendall estimates and tests come from `stats::cor.test`. Pearson confidence intervals retain R's Fisher-z approximation, including the requested one-sided interval and exact boundary correlations. Unbootstrapped intervals for ordinary Spearman/Kendall are not implemented. Rank tests preserve R's automatic policy: ties use approximation, small untied samples can use exact inference; Spearman AS89 at larger samples is an Edgeworth approximation, not an exact test. The result distinguishes the actual inference method and whether inference was exact. See [R's correlation-test documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html).
- Partial Pearson is the correlation of OLS residuals after controlling for the intercept and selected numeric controls. Partial Spearman first computes average ranks of all selected variables on the actual analysis cases, then residualizes those ranks; its t/Fisher inference is an approximation. Let `k` be the effective control-design rank excluding the intercept. The partial test uses `df = n - k - 2`; the Fisher interval/comparison uses `1/sqrt(n - k - 3)`. These ranks/df are independently checked against the full `lm(y ~ x + controls)` coefficient test. Redundant controls are retained in the request and warned about; inference uses their effective rank, not their raw count. This follows the regression rank accounting exposed by [R's linear-model implementation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html).
- Numeric controls do not automatically become dummy-coded categorical effects merely because they carry SPSS labels. Explicitly prepare the intended contrasts/dummy variables when that is the scientific role. `--coerce` is not categorical-model specification. SAV labels and user-missing definitions use the [import contract](../import-contract.md).
- `pairwise` selects complete observations separately for each pair plus its controls. `complete` uses the same rows complete across all requested correlated variables and controls within a group. Diagnostics remain variablewise. Groups are processed in first-observed order, preserving Date/factor values; a missing group is separate from a literal `NA` category, with exact input-row indices retained. Integer `group_id` values identify the actual groups for adjustment, comparisons and matrix sections. Display text is never an identity key: close numeric values, subsecond timestamps or duplicate value labels may render alike and receive explicit `[group N]` suffixes. Original typed values and raw display text remain in the request; ordinary labels stay unchanged.
- A constant variable, exhausted residual variation, or insufficient residual degrees of freedom produces an explicit unavailable pair, not a fabricated coefficient. Other estimable pairs can still complete; a request with no estimable pair fails without publishing normal output. A valid point estimate can have an unavailable interval (for example, Pearson with only three cases). Pairwise deletion does not guarantee a positive-semidefinite joint correlation matrix; matrix output is descriptive and its diagonal is a layout convention. See [R's missing-case and rank-correlation definitions](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html).
- `--p-adjust` applies separately to estimable requested correlation tests within each group; unavailable pairs are excluded from the family size, preserving the legacy R behavior. Fisher comparisons remain unadjusted. This is an estimability rule, not permission to select a family retrospectively from significant results.

### Bootstrap and Fisher comparisons

Bootstrap samples complete rows of the current pair with replacement; controls are refitted (and Spearman ranks recomputed) on every draw. Intervals use percentile quantiles (`type = 7`): two-sided tail probabilities `(1-level)/2` and `(1+level)/2`; one-sided lower/upper intervals end at 1/-1 respectively. The deterministic default seed makes a run reproducible, not statistically adequate.

Draws with no residual variation or changed effective control rank are counted as failed. Results include requested/valid/failed counts and reasons. At least two valid draws are needed to form an interval; when any draws fail, an available interval is explicitly conditional on the valid draws. Two is a computational minimum, not a recommendation for inferential precision; use sufficiently many resamples and investigate failures rather than treating conditional intervals as guaranteed coverage.

Fisher r-to-z tests are asymptotic tests against `r0` or between two independent non-missing groups; independence must be justified by the research design, not inferred from group labels. Spearman Fisher comparisons retain the legacy approximation and are labeled accordingly; Kendall comparisons are unsupported. For partial comparisons, each group's own effective control rank enters its standard error. Boundary correlations (`abs(r) = 1`) or insufficient Fisher degrees of freedom yield explicit unavailable comparison status; coefficients are never clipped to invent a finite comparison. One-sided comparisons use the declared direction (group 1 minus group 2 in first-observed order).

## Outputs

Each completed run also publishes `.nlss/runs/<run-id>/request.json`, `result.json`, frozen templates and deterministic `output.md`. It is statistical output, not a semantic final research report; metaskill reports remain freely structured, evidence-backed syntheses.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.
  - When `--compare-groups` is enabled, an additional "Correlation comparisons" section is appended.
- The mandatory result preserves `summary_df`, `diagnostics_df` and `comparison_df`, including estimate/interval/comparison status, effective control rank/df, exact-versus-approximate inference and bootstrap diagnostics. `--log FALSE` does not disable this result bundle.
- The resolved request records original/analysis variable classes and factor levels, coercion losses, group partitions (`group_id`, `value`, `raw_group`, readable `group`/`group_label`) and complete/per-pair input rows, control-design columns/rank/aliases, multiplicity scope and bootstrap/RNG choices. Numeric/Date/timestamp groups additionally retain `value_hex`, the exact hexadecimal floating-point value: JSON's ordinary numeric/time rendering can lose distinctions between adjacent doubles or subseconds. Summary/diagnostic rows use `group_id`; comparisons also expose `group1_id`/`group2_id`. The working Parquet is unchanged. Replay verifies the shared contract and uses frozen input/configuration/templates; failed runs are not replayable results.

## NLSS format Templates

Use the Markdown templates in `assets/correlations` when assembling correlation reports. If the template exists, it must be used for `report_canonical.md`.

- Use `assets/correlations/default-template.md` for correlation matrices created from `--vars` (or default numeric columns).
- Use `assets/correlations/cross-correlation-template.md` for cross-correlations created from `--x` and `--y`.
- Use `assets/correlations/matrix-template.md` (key: `templates.correlations.matrix` or `--template matrix`) for a true matrix layout with correlations below the diagonal and p-values above the diagonal.
- Use `assets/correlations/comparison-template.md` (key: `templates.correlations.comparison`) for Fisher r-to-z group comparisons.
- For partial correlations, keep the same template as the matrix or cross-correlation output and include the control variables in the analysis flags and note.
  - When `--p-adjust` is enabled, matrix p-values use the adjusted values.
  - Matrix layout is intended for full `--vars` correlation matrices; cross-correlations remain row-based.
  - When `--r0` is used, the matrix layout is disabled in favor of row-based output so r0/z columns can be displayed.
  
### YAML Template Controls

- Template paths can be overridden via `templates.correlations.default`, `templates.correlations.cross`, `templates.correlations.matrix`, and `templates.correlations.comparison` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.
- For matrix layouts, set `table.layout: matrix` (optional: `table.diagonal` or `table.diag` to override the diagonal cell value).

### Table Column Keys

Available column keys for `table.columns` include:

`group`, `var1`, `var2`, `r`, `r0`, `z_r0`, `p_r0`, `ci`, `boot_ci`, `boot_ci_low`, `boot_ci_high`, `p`, `p_adj`, `n`.

Additional result columns can be selected directly, including `df`, `control_rank`, `control_design_rank`, `estimate_status`, `inference`, `exact`, `ties`, `ci_status`, `r0_status`, `bootstrap_valid`, `bootstrap_failed` and `bootstrap_status`.

For comparison templates (group differences), available keys include:

`group1`, `group2`, `var1`, `var2`, `r1`, `r2`, `n1`, `n2`, `z`, `p`.

Use `drop_if_empty: true` to remove a column if all values are blank (e.g., `group`, `p_adj`, `ci`).

### Note Tokens

Available note tokens include:

`note_default`, `ci_label`, `tail_note`, `missing_note`, `partial_note`, `p_adjust_note`, `ci_note`, `boot_note`, `r0_note`, `method_note`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `group`, `var1`, `var2`, `stat_text`, `r`, `r0`, `z_r0`, `p_r0`, `ci`, `ci_text`, `boot_ci`, `boot_ci_text`, `p`, `n`, `missing_n`, `missing_pct`, `missing_text`, `full_sentence`, `group1`, `group2`, `r1`, `r2`, `n1`, `n2`, `z`.

Per-pair rows also expose `estimate_status`, `inference`, `ci_status`, `bootstrap_status`, `bootstrap_valid`, `bootstrap_failed` and `availability_text`.

## NLSS format Reporting Guidance

- Report method-specific coefficients (Pearson's r, Spearman's rho, Kendall's tau) with p-values and sample size.
- If using partial correlations, state the control variables explicitly.
- Note missing-data handling (pairwise vs complete) and any p-value adjustment.
- When bootstrap CIs are enabled, report the bootstrap interval and resample count.
- Distinguish unavailable inference from a non-significant result; disclose rank reductions, failed bootstrap draws and approximations that affect interpretation.
- For Fisher r-to-z comparisons, report z, p, and the group pairing or r_0 value.
- Integrate effects, uncertainty, missing-case choices, design limitations and domain meaning in the final report. A table template neither establishes causality nor replaces the researcher's semantic interpretation.
