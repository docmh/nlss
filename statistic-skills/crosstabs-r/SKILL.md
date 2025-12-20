---
name: crosstabs-r
description: Create APA 7 style cross-tabulations (contingency tables) with chi-square or Fisher tests and effect sizes using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts. Use when Codex needs cross-tabs, chi-square tests, or association measures like Cramer's V.
---

# Cross Tabulations (Base R, APA 7)

## Overview

Generate cross-tabulations for two categorical variables, optional stratification by a grouping variable, and common association statistics (chi-square, Fisher's exact test, Cramer's V, phi, contingency coefficient). Outputs include machine-readable CSVs plus APA 7-ready tables and narrative text.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose row and column variables, with optional grouping variable.
3. Run `scripts/crosstabs.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`crosstabs_cells.csv`, `crosstabs_tests.csv`, `crosstabs_diagnostics.csv`, `apa_table.md`, `apa_text.txt`).

## Script: `scripts/crosstabs.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `../run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File ..\run_rscript.ps1 scripts\crosstabs.R --csv data.csv --row gender --col condition --out ..\outputs\tmp
```

```powershell
powershell -ExecutionPolicy Bypass -File ..\run_rscript.ps1 scripts\crosstabs.R --csv data.csv --rows gender,handedness --cols condition
```

### CSV input

```bash
Rscript scripts/crosstabs.R --csv data.csv --row gender --col condition --out ../outputs/tmp
```

### RDS input (data frame)

```bash
Rscript scripts/crosstabs.R --rds data.rds --row gender --col condition
```

### RData input (data frame by name)

```bash
Rscript scripts/crosstabs.R --rdata analysis.RData --df df --row gender --col condition
```

### Interactive prompts

```bash
Rscript scripts/crosstabs.R --interactive
```

### Options

- `--row` or `--rows`: Row variable(s). `--rows` accepts a comma-separated list.
- `--col` or `--cols`: Column variable(s). `--cols` accepts a comma-separated list.
- `--group`: Optional grouping variable (stratified cross-tabs).
- `--percent`: Which percentages to include in `crosstabs_cells.csv` (row, col, total, all). Default: `all`.
- `--apa-percent`: Percentage column(s) for `apa_table.md` (row, col, total, all, none). Default: `row`.
- `--chisq`: Run chi-square test (default: TRUE).
- `--yates`: Apply Yates continuity correction for 2x2 tables (default: FALSE).
- `--fisher`: Run Fisher's exact test (default: FALSE).
- `--fisher-simulate`: Use Monte Carlo simulation for Fisher's exact test (default: FALSE).
- `--fisher-b`: Number of Monte Carlo replications for Fisher's exact test (default: 2000).
- `--fisher-conf-level`: Confidence level for Fisher's exact test odds ratio (default: 0.95).
- `--expected`: Include expected counts (default: TRUE).
- `--residuals`: Include standardized/adjusted residuals (default: TRUE).
- `--digits`: Rounding digits for outputs (default: 2).
- `--out`: Output directory (default: `statistic-skills/outputs/tmp`, hard-coded when omitted).
- `--interactive`: Prompt for inputs.

## Outputs

- `crosstabs_cells.csv`: Cell-level counts, percentages, expected counts, and residuals.
- `crosstabs_tests.csv`: Table-level tests and effect sizes (chi-square, Fisher, phi, Cramer's V, contingency coefficient) plus missingness.
- `crosstabs_diagnostics.csv`: Expected count diagnostics (min expected, percent < 5, counts < 1/5).
- `apa_table.md`: APA 7-style long-format cross-tab table.
- `apa_text.txt`: APA 7-style narrative text including test results and assumptions.

## APA 7 Reporting Guidance

- Report chi-square (or Fisher's exact test) with df, N, p-value, and effect size (Cramer's V or phi for 2x2).
- Note expected count diagnostics when assumptions are questionable (e.g., > 20% cells < 5).
- Missing values are excluded from valid counts and reported in the test outputs.
