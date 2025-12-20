---
name: crosstabs-r
description: Create APA 7 style cross-tabulations (contingency tables) with chi-square or Fisher tests and effect sizes using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts. Use when Codex needs cross-tabs, chi-square tests, or association measures like Cramer's V.
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

- `--row` or `--rows`: Row variable(s). `--rows` accepts a comma-separated list.
- `--col` or `--cols`: Column variable(s). `--cols` accepts a comma-separated list.
- `--group`: Optional grouping variable (stratified cross-tabs).
- `--percent`: Which percentages to include in the raw results (row, col, total, all). Default: `all`.
- `--apa-percent`: Percentage column(s) for the APA table section (row, col, total, all, none). Default: `row`.
- `--chisq`: Run chi-square test (default: TRUE).
- `--yates`: Apply Yates continuity correction for 2x2 tables (default: FALSE).
- `--fisher`: Run Fisher's exact test (default: FALSE).
- `--fisher-simulate`: Use Monte Carlo simulation for Fisher's exact test (default: FALSE).
- `--fisher-b`: Number of Monte Carlo replications for Fisher's exact test (default: 2000).
- `--fisher-conf-level`: Confidence level for Fisher's exact test odds ratio (default: 0.95).
- `--expected`: Include expected counts (default: TRUE).
- `--residuals`: Include standardized/adjusted residuals (default: TRUE).
- `--digits`: Rounding digits for outputs (default: 2).
- `--out`: Output directory (default: `<working directory>/outputs/tmp`, hard-coded when omitted, relative to the working directory).
- `--interactive`: Prompt for inputs.
- `--log`: Toggle JSONL logging (default: TRUE).
- `--user-prompt`: Store the original AI prompt in the JSONL log (optional).

## Outputs

- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Reporting Guidance

- Report chi-square (or Fisher's exact test) with df, N, p-value, and effect size (Cramer's V or phi for 2x2).
- Note expected count diagnostics when assumptions are questionable (e.g., > 20% cells < 5).
- Missing values are excluded from valid counts and reported in the test outputs.
