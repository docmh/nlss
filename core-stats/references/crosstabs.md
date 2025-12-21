---
name: crosstabs-r
description: Contingency tables for categorical variables with chi-square/Fisher tests, effect sizes (Cramer's V/phi), residuals, optional grouping, and APA-ready outputs.
---

# Cross Tabulations (Base R, APA 7)

## Overview

Generate cross-tabulations for two categorical variables, optional stratification by a grouping variable, and common association statistics (chi-square, Fisher's exact test, Cramer's V, phi, contingency coefficient). Outputs include an APA 7-ready report and JSONL logging.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose row and column variables, with optional grouping variable.
3. Run `scripts/R/crosstabs.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`).

## Script: `scripts/R/crosstabs.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\crosstabs.R> --csv <path to CSV file> --row gender --col condition --out <working directory>\outputs\tmp
```

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\crosstabs.R> --csv <path to CSV file> --rows gender,handedness --cols condition
```

### CSV input

```bash
Rscript <path to scripts/R/crosstabs.R> --csv <path to CSV file> --row gender --col condition --out <working directory>/outputs/tmp
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/crosstabs.R> --rds <path to RDS file> --row gender --col condition
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/crosstabs.R> --rdata <path to RData file> --df <data frame name> --row gender --col condition
```

### Interactive prompts

```bash
Rscript <path to scripts/R/crosstabs.R> --interactive
```

### Options

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--row` or `--rows`: Row variable(s). `--rows` accepts a comma-separated list.
- `--col` or `--cols`: Column variable(s). `--cols` accepts a comma-separated list.
- `--group`: Optional grouping variable (stratified cross-tabs).
- `--percent`: Which percentages to include in the raw results (row, col, total, all). Default: `modules.crosstabs.percent`.
- `--apa-percent`: Percentage column(s) for the APA table section (row, col, total, all, none). Default: `modules.crosstabs.apa_percent`.
- `--chisq`: Run chi-square test (default: `modules.crosstabs.chisq`).
- `--yates`: Apply Yates continuity correction for 2x2 tables (default: `modules.crosstabs.yates`).
- `--fisher`: Run Fisher's exact test (default: `modules.crosstabs.fisher`).
- `--fisher-simulate`: Use Monte Carlo simulation for Fisher's exact test (default: `modules.crosstabs.fisher_simulate`).
- `--fisher-b`: Number of Monte Carlo replications for Fisher's exact test (default: `modules.crosstabs.fisher_b`).
- `--fisher-conf-level`: Confidence level for Fisher's exact test odds ratio (default: `modules.crosstabs.fisher_conf_level`).
- `--expected`: Include expected counts (default: `modules.crosstabs.expected`).
- `--residuals`: Include standardized/adjusted residuals (default: `modules.crosstabs.residuals`).
- `--digits`: Rounding digits for outputs (default: `defaults.digits`).
- `--out`: Output directory (default: `defaults.output_dir`).
- `--interactive`: Prompt for inputs.
- `--log`: Toggle JSONL logging (default: `defaults.log`).
- `--user-prompt`: Store the original AI prompt in the JSONL log (optional).

## Outputs

- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates

Use the Markdown templates in `core-stats/assets/crosstabs` when assembling cross-tabulation reports. If the template exists, `crosstabs.R` uses it for `apa_report.md`.

### YAML template controls

- Template paths can be overridden via `templates.crosstabs.default` and `templates.crosstabs.grouped` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`row_var`, `col_var`, `group`, `row_level`, `col_level`, `n`, `pct_row`, `pct_col`, `pct_total`, `expected`, `std_resid`, `adj_resid`.

Use `drop_if_empty: true` to remove a column if all values are blank (e.g., `group`, `expected`, `std_resid`, `adj_resid`).

### Note tokens

Available note tokens include:

`note_default`, `percent_labels`, `missing_note`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `row_var`, `col_var`, `group`, `valid_n`, `missing_n`, `missing_pct`, `chisq_text`, `effect_text`, `fisher_text`, `tests_text`, `expected_text`, `missing_text`, `full_sentence`.

### Multiple row/column pairs

When multiple `--rows`/`--cols` combinations are requested, all results are rendered in the same long-format table. Include `row_var` and `col_var` in `table.columns` if you want the table to identify which row/column pair each line belongs to.

## APA 7 Reporting Guidance

- Report chi-square (or Fisher's exact test) with df, N, p-value, and effect size (Cramer's V or phi for 2x2).
- Note expected count diagnostics when assumptions are questionable (e.g., > 20% cells < 5).
- Missing values are excluded from valid counts and reported in the test outputs.
