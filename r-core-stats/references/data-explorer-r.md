---
name: data-explorer-r
description: Explore a dataset and produce a variable overview with inferred scale levels plus value level tables, using base R for CSV inputs, R data frames (via RDS or RData), or interactive prompts. Use when Codex needs to inspect variable names, types, scale levels, and value levels before analysis.
---

# Data Explorer (Base R, APA 7)

## Overview

Create a data dictionary-style overview with variable names, inferred measurement levels, missingness, and value levels. Outputs include an APA-ready report and JSONL logging.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Optionally select variables; default is all columns.
3. Run `scripts/R/data_explorer.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/data_explorer.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\data_explorer.R> --csv <path to CSV file> --vars age,gender --out <working directory>\outputs\tmp
```

### CSV input

```bash
Rscript <path to scripts/R/data_explorer.R> --csv <path to CSV file> --vars age,gender
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/data_explorer.R> --rds <path to RDS file> --vars age,gender
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/data_explorer.R> --rdata <path to RData file> --df <data frame name> --vars age,gender
```

### Interactive prompts

```bash
Rscript <path to scripts/R/data_explorer.R> --interactive
```

### Options

- Defaults are loaded from `r-core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.data_explorer.vars_default` (typically all columns) if omitted.
- `--max-levels` controls when level tables are truncated (default: `modules.data_explorer.max_levels`). Categorical variables with more levels are summarized with top `--top-n` levels and an "Other (remaining)" row.
- `--top-n` controls how many levels to keep when truncating (default: `modules.data_explorer.top_n`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--out` sets the output directory (default: `defaults.output_dir`).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

- `apa_report.md`: APA 7 report containing analysis type, tables, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates

This module currently uses the built-in APA report format (no YAML template mapping in `templates.*`). To customize the APA layout, add a template under `r-core-stats/assets` and wire it through the template system.

## APA 7 Reporting Guidance

- Treat the measurement level as a heuristic; the script uses `interval/ratio` for numeric variables and cannot distinguish interval from ratio scales automatically.
- Use `Table 1` for a concise overview of variable types, missingness, and numeric summaries.
- Use `Table 2` to report value levels and valid percentages for categorical variables; mention when levels are truncated.
