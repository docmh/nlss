---
name: correlations
description: Pearson/Spearman/Kendall correlations as matrices or cross-sets, with partial controls, bootstrap CIs, Fisher r-to-z tests, p-adjustments, grouping, and APA outputs.
---

# Correlations (Base R, APA 7)

## Overview

Compute correlations for numeric variables (pairwise or matrix), with optional grouping, partial correlations, bootstrap CIs, Fisher r-to-z comparisons, and matrix-layout templates. Outputs include an APA-ready report and JSONL logging with diagnostics.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables: full matrix via `--vars` (or default numeric columns), or cross-correlation via `--x` and `--y`.
3. Choose correlation method (Pearson/Spearman/Kendall), missing-data handling, and any control variables.
4. Run `scripts/R/correlations.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/correlations.R`

Run with `Rscript` and base R only.

### CSV input

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score,stress
```

### Cross-correlation between sets

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --x age,stress --y wellbeing,performance
```

### Grouped correlations

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --group condition
```

### Fisher r-to-z vs theoretical value

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --r0 0.3
```

### Fisher r-to-z between groups

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --group condition --compare-groups TRUE
```

### Partial correlations (controls)

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --controls gender,education
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/correlations.R> --rds <path to RDS file> --vars age,score
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/correlations.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet input

```bash
Rscript <path to scripts/R/correlations.R> --parquet <path to parquet file> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/correlations.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.correlations.vars_default` (typically numeric columns) if omitted.
- `--x` and `--y` compute cross-correlations between two sets.
- `--group` is optional and produces grouped correlations.
- `--method` selects `pearson`, `spearman`, or `kendall` (default: `modules.correlations.method`).
- `--missing` selects `pairwise` or `complete` (default: `modules.correlations.missing`).
- `--alternative` selects `two.sided`, `greater`, or `less` (default: `modules.correlations.alternative`).
- `--controls` enables partial correlations (not supported for Kendall).
- `--p-adjust` adjusts p-values (`none`, `bonferroni`, `holm`, `hochberg`, `hommel`, `BH`, `BY`, `fdr`). Default: `modules.correlations.p_adjust`.
- `--conf-level` sets the Fisher z confidence level for Pearson/partial (default: `modules.correlations.conf_level`).
- `--bootstrap` enables bootstrap confidence intervals (default: `modules.correlations.bootstrap`).
- `--bootstrap-samples` sets bootstrap resamples (default: `modules.correlations.bootstrap_samples`).
- `--seed` sets the random seed for bootstrap resampling (optional).
- `--r0` sets the Fisher r-to-z comparison value (optional; must be between -1 and 1).
- `--compare-groups` compares correlations between two independent groups (default: `modules.correlations.compare_groups`; requires `--group` with exactly two non-missing levels).
  - Fisher r-to-z comparisons are not supported for Kendall's tau.
  - `--p-adjust` applies to correlation p-values; Fisher r-to-z comparison p-values are unadjusted.
- `--coerce` coerces non-numeric columns to numeric (default: `modules.correlations.coerce`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--interactive` prompts for inputs.
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).

- `report_canonical.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).
  - When `--compare-groups` is enabled, an additional "Correlation comparisons" section is appended.

## APA 7 Templates

Use the Markdown templates in `nlss/assets/correlations` when assembling correlation reports. If the template exists, it must be used for `report_canonical.md`.

- Use `nlss/assets/correlations/default-template.md` for correlation matrices created from `--vars` (or default numeric columns).
- Use `nlss/assets/correlations/cross-correlation-template.md` for cross-correlations created from `--x` and `--y`.
- Use `nlss/assets/correlations/matrix-template.md` (key: `templates.correlations.matrix` or `--template matrix`) for a true matrix layout with correlations below the diagonal and p-values above the diagonal.
- Use `nlss/assets/correlations/comparison-template.md` (key: `templates.correlations.comparison`) for Fisher r-to-z group comparisons.
- For partial correlations, keep the same template as the matrix or cross-correlation output and include the control variables in the analysis flags and note.
  - When `--p-adjust` is enabled, matrix p-values use the adjusted values.
  - Matrix layout is intended for full `--vars` correlation matrices; cross-correlations remain row-based.
  - When `--r0` is used, the matrix layout is disabled in favor of row-based output so r0/z columns can be displayed.
  
### YAML template controls

- Template paths can be overridden via `templates.correlations.default`, `templates.correlations.cross`, `templates.correlations.matrix`, and `templates.correlations.comparison` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.
- For matrix layouts, set `table.layout: matrix` (optional: `table.diagonal` or `table.diag` to override the diagonal cell value).

### Table column keys

Available column keys for `table.columns` include:

`group`, `var1`, `var2`, `r`, `r0`, `z_r0`, `p_r0`, `ci`, `boot_ci`, `boot_ci_low`, `boot_ci_high`, `p`, `p_adj`, `n`.

For comparison templates (group differences), available keys include:

`group1`, `group2`, `var1`, `var2`, `r1`, `r2`, `n1`, `n2`, `z`, `p`.

Use `drop_if_empty: true` to remove a column if all values are blank (e.g., `group`, `p_adj`, `ci`).

### Note tokens

Available note tokens include:

`note_default`, `ci_label`, `tail_note`, `missing_note`, `partial_note`, `p_adjust_note`, `ci_note`, `boot_note`, `r0_note`, `method_note`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `group`, `var1`, `var2`, `stat_text`, `r`, `r0`, `z_r0`, `p_r0`, `ci`, `ci_text`, `boot_ci`, `boot_ci_text`, `p`, `n`, `missing_n`, `missing_pct`, `missing_text`, `full_sentence`, `group1`, `group2`, `r1`, `r2`, `n1`, `n2`, `z`.

## APA 7 Reporting Guidance

- Report method-specific coefficients (Pearson's r, Spearman's rho, Kendall's tau) with p-values and sample size.
- If using partial correlations, state the control variables explicitly.
- Note missing-data handling (pairwise vs complete) and any p-value adjustment.
- When bootstrap CIs are enabled, report the bootstrap interval and resample count.
- For Fisher r-to-z comparisons, report z, p, and the group pairing or r0 value.
