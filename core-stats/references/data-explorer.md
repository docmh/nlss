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

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
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

## APA 7 Template (YAML)

Use the Markdown template at `core-stats/assets/data-explorer/default-template.md` when assembling a data exploration report. If the template exists, `data_explorer.R` uses it for `apa_report.md`.

- The template path can be overridden via `templates.data_explorer.default` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`) for the overview table.
  - `levels_table.columns`: ordered column definitions for the levels table.
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Overview table keys include:

`variable`, `class`, `storage`, `measurement_level`, `measurement_note`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `unique_n`, `example_values`, `mean`, `sd`, `min`, `max`, `median`, `q1`, `q3`, `levels_included`, `levels_truncated`, `levels_note`.

Levels table keys include:

`variable`, `level`, `n`, `pct_total`, `pct_valid`, `total_n`, `missing_n`, `missing_pct`.

### Table tokens

Table-level tokens include:

`overview_table_body`, `levels_table_body`, `table_number_next`.

### Note tokens

Overview note tokens:

`overview_note_default`, `overview_note_body`.

Levels note tokens:

`levels_note_default`, `levels_note_body`, `pct_total_note`, `pct_valid_note`, `missing_note`, `truncation_note`.

### Narrative tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`label`, `variable`, `class`, `storage`, `measurement_level`, `measurement_note`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `unique_n`, `example_values`, `mean`, `sd`, `min`, `max`, `median`, `q1`, `q3`, `levels_text`, `missing_text`, `levels_truncated`, `full_sentence`.

## APA 7 Reporting Guidance

- Treat the measurement level as a heuristic; the script uses `interval/ratio` for numeric variables and cannot distinguish interval from ratio scales automatically.
- Use `Table 1` for a concise overview of variable types, missingness, and numeric summaries.
- Use `Table 2` to report value levels and valid percentages for categorical variables; mention when levels are truncated.
