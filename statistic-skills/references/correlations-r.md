---
name: correlations-r
description: Create APA 7 style correlation analyses using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts. Use when Codex needs Pearson/Spearman/Kendall correlations, correlation matrices, cross-correlations between variable sets, partial correlations with controls, optional grouping, and APA-ready tables/text.
---

# Correlations (Base R, APA 7)

## Overview

Compute correlations for numeric variables (pairwise or matrix), with optional grouping and partial correlations. Outputs include machine-readable CSVs, APA-ready table text, and diagnostics (normality checks).

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose variables: full matrix via `--vars` (or default numeric columns), or cross-correlation via `--x` and `--y`.
3. Choose correlation method (Pearson/Spearman/Kendall), missing-data handling, and any control variables.
4. Run `scripts/correlations.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`correlations_summary.csv`, `correlations_diagnostics.csv`, `apa_table.md`, `apa_text.txt`) to craft the response.

## Script: `scripts/correlations.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File scripts\run_rscript.ps1 scripts\correlations.R --csv data.csv --vars age,score --out .\outputs\tmp
```

### CSV input

```bash
Rscript scripts/correlations.R --csv data.csv --vars age,score,stress
```

### Cross-correlation between sets

```bash
Rscript scripts/correlations.R --csv data.csv --x age,stress --y wellbeing,performance
```

### Grouped correlations

```bash
Rscript scripts/correlations.R --csv data.csv --vars age,score --group condition
```

### Partial correlations (controls)

```bash
Rscript scripts/correlations.R --csv data.csv --vars age,score --controls gender,education
```

### RDS input (data frame)

```bash
Rscript scripts/correlations.R --rds data.rds --vars age,score
```

### RData input (data frame by name)

```bash
Rscript scripts/correlations.R --rdata analysis.RData --df df --vars age,score
```

### Interactive prompts

```bash
Rscript scripts/correlations.R --interactive
```

### Options

- `--vars` defaults to all numeric columns if omitted.
- `--x` and `--y` compute cross-correlations between two sets.
- `--group` is optional and produces grouped correlations.
- `--method` selects `pearson`, `spearman`, or `kendall` (default: `pearson`).
- `--missing` selects `pairwise` or `complete` (default: `pairwise`).
- `--alternative` selects `two.sided`, `greater`, or `less` (default: `two.sided`).
- `--controls` enables partial correlations (not supported for Kendall).
- `--p-adjust` adjusts p-values (`none`, `bonferroni`, `holm`, `hochberg`, `hommel`, `BH`, `BY`, `fdr`).
- `--conf-level` sets the Fisher z confidence level for Pearson/partial (default: 0.95).
- `--coerce` coerces non-numeric columns to numeric (default: FALSE).
- `--digits` controls rounding (default: 2).
- `--out` sets the output directory (default: `./outputs/tmp`, relative to the working directory).
- `--interactive` prompts for inputs.

## Outputs

- `correlations_summary.csv`: Long-format pairwise correlations with method, n, p-values, CIs, missingness, and adjustment info.
- `correlations_diagnostics.csv`: Per-variable diagnostics (missingness, skewness/kurtosis, Shapiro-Wilk).
- `apa_table.md`: APA 7-style correlation table (per group, if grouped).
- `apa_text.txt`: APA-style narrative text with r/rho/tau, p, n, CI, and missingness.

## APA 7 Reporting Guidance

- Report method-specific coefficients (Pearson's r, Spearman's rho, Kendall's tau) with p-values and sample size.
- If using partial correlations, state the control variables explicitly.
- Note missing-data handling (pairwise vs complete) and any p-value adjustment.
