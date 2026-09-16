---
name: crosstabs
description: Contingency tables for categorical row/col pairs with chi²/Fisher tests, effect sizes, expected counts and residuals, optional grouping, and NLSS format outputs.
license: Apache-2.0
---

# Cross Tabulations (Base R, NLSS format)

## Overview

Generate cross-tabulations for categorical row/column pairs, optional stratification by a grouping variable, and common association statistics (chi², Fisher's exact test, Cramer's V, phi, contingency coefficient). Calculations use established base-R statistical methods. Each run preserves a resolved request, full-precision results and deterministic Markdown, alongside the automatic root canonical output.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive). Follow the [import contract](../import-contract.md) for variable/value labels and user-defined missing values.
2. Choose row and column variables, with optional grouping variable.
3. Run `scripts/R/crosstabs.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `result.json`).

## Script: `scripts/R/crosstabs.R`

Run with `Rscript`. Statistical tests use `stats`; the common workspace, JSON and configuration dependencies still apply, including `arrow` for the verified Parquet input.

### CSV Input

```bash
Rscript <path to scripts/R/crosstabs.R> --csv <path to CSV file> --row gender --col condition
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/crosstabs.R> --rds <path to RDS file> --row gender --col condition
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/crosstabs.R> --rdata <path to RData file> --df <data frame name> --row gender --col condition
```

### Parquet Input

```bash
Rscript <path to scripts/R/crosstabs.R> --parquet <path to parquet file> --row gender --col condition
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/crosstabs.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--row` or `--rows`: Row variable(s). `--rows` accepts a comma-separated list.
- `--col` or `--cols`: Column variable(s). `--cols` accepts a comma-separated list.
- `--group`: Optional grouping variable (stratified cross-tabs).
- `--percent`: Which percentages to include in `cells_df` (row, col, total, all, or a comma-separated combination). Default: `modules.crosstabs.percent`. Omitted percentage values are null, not zero.
- `--nlss-percent`: Percentage column(s) for the NLSS format table section (row, col, total, all, none). Default: `modules.crosstabs.nlss_percent`.
- `--chisq`: Run chi² test (default: `modules.crosstabs.chisq`).
- `--yates`: Apply Yates continuity correction for 2x2 tables (default: `modules.crosstabs.yates`).
- `--fisher`: Run Fisher's exact test (default: `modules.crosstabs.fisher`).
- `--fisher-simulate`: Request Monte Carlo simulation for Fisher's test (default: `modules.crosstabs.fisher_simulate`). As in `stats::fisher.test`, a 2x2 table still uses the exact calculation; `fisher_simulated` records the actual method used.
- `--fisher-b`: Positive integer Monte Carlo replications for Fisher's exact test (default: `modules.crosstabs.fisher_b`).
- `--seed`: Non-negative R integer seed. Simulated Fisher uses `modules.crosstabs.seed` (default 1) when omitted. The seed and RNG state are saved before iterating through groups and row/column pairs; the complete run is reproducible without resetting the seed for each table.
- `--fisher-conf-level`: Confidence level strictly between 0 and 1 for the 2x2 Fisher odds ratio (default: `modules.crosstabs.fisher_conf_level`). The reported confidence level matches this option.
- `--expected`: Include expected counts (default: `modules.crosstabs.expected`).
- `--residuals`: Include standardized/adjusted residuals (default: `modules.crosstabs.residuals`).
- `--digits`: Integer from 0 to 15 for display rounding (default: `defaults.digits`); machine-readable results remain unrounded, and p-values use their full precision before formatting.
- `--interactive`: Prompt for inputs.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log`: Control optional standalone logging; project evidence remains enabled (default: `defaults.log`).
- `--user-prompt`: Store the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Outputs

This migrated module also publishes `.nlss/runs/<run-id>/request.json`, `result.json`, frozen templates and deterministic `output.md`; see the [run/replay contract](../run-contract.md). `--log FALSE` disables only optional standalone logging, not the run bundle. `output.md` is statistical output, not a prescribed semantic final research report. Authored reports use freely chosen visible Markdown paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `result.json`: Contains `cells_df`, `tests_df`, `diagnostics_df`, and `report_cells_df`. The latter preserves the independently selected `--nlss-percent` values used by Markdown; a raw `--percent row` does not disable a requested column-percentage report. Expected-count and residual inclusion flags apply to both cell projections, while assumption diagnostics remain available.
- Each Fisher odds-ratio/CI field in `tests_df` has a matching `_status` field (`finite`, `positive_infinity`, `negative_infinity`, `not_available`). A valid unbounded odds ratio or confidence limit is distinguishable from an unavailable estimate even though JSON represents both numeric non-finite values as null; Markdown may show `Inf`.
- `request.json`: Records all resolved options, classes, factor levels, original-row indices for each group and complete-case indices for each table. Missing groups have `group_missing: true` in result tables; a distinct display label prevents conflating actual missing values with a real level named `NA`.

Requested inference that cannot be estimated (no complete observations, fewer than two observed row or column levels, or an R test error) fails explicitly and does not publish a completed analysis. To request descriptive counts for such a table, use `--chisq FALSE --fisher FALSE`. Zero-frequency factor levels are retained in descriptive cells but removed from the inferential contingency table, as documented in the saved design. Approximation warnings from chi-square tests are retained in the run result rather than suppressed.

## NLSS format Templates

Use the Markdown templates in `assets/crosstabs` when assembling cross-tabulation reports. If the template exists, `crosstabs.R` uses it for `report_canonical.md`.

### YAML Template Controls

- Template paths can be overridden via `templates.crosstabs.default` and `templates.crosstabs.grouped` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`row_var`, `col_var`, `group`, `row_level`, `col_level`, `n`, `pct_row`, `pct_col`, `pct_total`, `phi`, `cramers_v`, `contingency_c`, `expected`, `std_resid`, `adj_resid`.

Use `drop_if_empty: true` to remove a column if all values are blank (e.g., `group`, `expected`, `std_resid`, `adj_resid`).

### Note Tokens

Available note tokens include:

`note_default`, `percent_labels`, `missing_note`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `row_var`, `col_var`, `group`, `valid_n`, `missing_n`, `missing_pct`, `chisq_text`, `effect_text`, `fisher_text`, `tests_text`, `expected_text`, `missing_text`, `full_sentence`.

### Multiple Row/Column Pairs

When multiple `--rows`/`--cols` combinations are requested, all results are rendered in the same long-format table. Include `row_var` and `col_var` in `table.columns` if you want the table to identify which row/column pair each line belongs to.

## NLSS format Reporting Guidance

- Report chi² (or Fisher's exact test) with df, N, p-value, and effect size (Cramer's V or phi for 2x2).
- Note expected count diagnostics when assumptions are questionable (e.g., > 20% cells < 5).
- Missing values are excluded from valid counts and reported in the test outputs.
- Group-missing rows form their own stratum; they are never added as artificial missing rows to the other groups. All row/column combinations use pairwise-complete observations within the selected stratum.
- `std_resid` is the Pearson residual; `adj_resid` uses row and column marginal adjustments and agrees with `stats::chisq.test(..., correct = FALSE)$stdres`. Phi, Cramer's V and the contingency coefficient use the selected chi-square statistic (including Yates correction when enabled).
