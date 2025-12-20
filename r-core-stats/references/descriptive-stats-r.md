---
name: descriptive-stats-r
description: Create APA 7 style descriptive statistics using base R for CSV inputs, R data frames (via RDS or RData), or interactive prompts. Use when Codex needs to compute and report descriptive stats (means, SD, median, range, skewness/kurtosis, missingness, CIs) with APA 7 narrative and table output.
---

# Descriptive Stats (Base R, APA 7)

## Overview

Generate descriptive statistics in base R for psychology coursework or reports, and return APA 7-style tables and narrative text.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose numeric variables and optional grouping variable.
3. Run `scripts/R/descriptive_stats.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`descriptive_summary.csv`, `apa_table.md`, `apa_text.txt`) to craft the response.

## Script: `scripts/R/descriptive_stats.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\descriptive_stats.R> --csv <path to CSV file> --vars age,score --group condition --out <working directory>\outputs\tmp
```

### CSV input

```bash
Rscript <path to scripts/R/descriptive_stats.R> --csv <path to CSV file> --vars age,score --group condition
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/descriptive_stats.R> --rds <path to RDS file> --vars age,score
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/descriptive_stats.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/descriptive_stats.R> --interactive
```

### Options

- `--vars` defaults to all numeric columns if omitted.
- `--group` is optional and produces grouped summaries.
- `--digits` controls rounding (default: 2).
- `--out` sets the output directory (default: `<working directory>/outputs/tmp`, relative to the working directory).

## Outputs

- `descriptive_summary.csv`: Full statistics per variable (and group, if used), including missingness, CI, skewness, and kurtosis.
- `apa_table.md`: APA 7-style table with M, SD, Min, Max, and n.
- `apa_text.txt`: APA-style narrative text per variable (and group).

## APA 7 Reporting Guidance

- Use the narrative lines as the base: `Variable: M = x.xx, SD = x.xx, 95% CI [x.xx, x.xx], n = xx, missing = x (x.x%).`
- If `n < 2` or SD is missing, state that variability cannot be estimated.
- When grouped, report each group separately before any overall comparison.
