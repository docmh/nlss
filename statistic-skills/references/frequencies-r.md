---
name: frequencies-r
description: Create APA 7 style frequency tables using base R for CSV inputs, R data frames (via RDS or RData), or interactive prompts. Use when Codex needs to compute and report categorical frequencies or cross-tabs with optional grouping.
---

# Frequencies (Base R, APA 7)

## Overview

Generate frequency tables in base R for categorical variables and return APA 7-style tables and narrative text. Factor levels are preserved; non-factor variables are sorted by their unique values. Missing values are reported separately and excluded from valid percentages.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose variables for frequency tables and an optional grouping variable.
3. Run `scripts/frequencies.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`frequencies_summary.csv`, `apa_table.md`, `apa_text.txt`) to craft the response.

## Script: `scripts/frequencies.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\frequencies.R> --csv <path to CSV file> --vars gender,condition --group condition --out <working directory>\outputs\tmp
```

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\frequencies.R> --csv <path to CSV file> --vars gender --group condition
```

### CSV input

```bash
Rscript <path to scripts/frequencies.R> --csv <path to CSV file> --vars gender,condition --out <working directory>/outputs/tmp
```

### RDS input (data frame)

```bash
Rscript <path to scripts/frequencies.R> --rds <path to RDS file> --vars gender,condition
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/frequencies.R> --rdata <path to RData file> --df <data frame name> --vars gender,condition
```

### Interactive prompts

```bash
Rscript <path to scripts/frequencies.R> --interactive
```

### Options

- `--vars` defaults to all non-numeric columns. Use `--include-numeric` to include numeric columns when `--vars` is omitted.
- `--group` is optional and produces grouped frequency tables (one grouping variable).
- `--digits` controls rounding for percentages (default: 2).
- `--out` sets the output directory (default: `<working directory>/outputs/tmp`, relative to the working directory).

## Outputs

- `frequencies_summary.csv`: Long-format frequency table with counts and percentages per level, plus missingness per variable/group.
- `apa_table.md`: APA 7-style table with counts, percent of total, and valid percent.
- `apa_text.txt`: APA-style narrative text per variable (and group) including missingness.

## APA 7 Reporting Guidance

- Report each variable (and group, if used) with level counts and valid percentages.
- Note missing values in the narrative or use the table's "Missing" row.
- If no valid observations exist for a variable/group, state that explicitly.
