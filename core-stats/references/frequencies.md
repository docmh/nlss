---
name: frequencies-r
description: Frequency tables for categorical variables with counts, percentages, and optional grouping, producing APA-ready tables and narratives.
---

# Frequencies (Base R, APA 7)

## Overview

Generate frequency tables in base R for categorical variables and return an APA 7-style report (table + narrative). Factor levels are preserved; non-factor variables are sorted by their unique values. Missing values are reported separately and excluded from valid percentages.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables for frequency tables and an optional grouping variable.
3. Run `scripts/R/frequencies.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/frequencies.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\frequencies.R> --csv <path to CSV file> --vars gender,condition --group condition
```

### CSV input

```bash
Rscript <path to scripts/R/frequencies.R> --csv <path to CSV file> --vars gender,condition --group condition
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/frequencies.R> --rds <path to RDS file> --vars gender,condition
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/frequencies.R> --rdata <path to RData file> --df <data frame name> --vars gender,condition
```

### Parquet input

```bash
Rscript <path to scripts/R/frequencies.R> --parquet <path to parquet file> --vars gender,condition
```

### Interactive prompts

```bash
Rscript <path to scripts/R/frequencies.R> --interactive
```

### Options

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.frequencies.vars_default` (typically non-numeric columns). Use `--include-numeric` to include numeric columns when `--vars` is omitted.
- `--group` is optional and produces grouped frequency tables (one grouping variable).
- `--digits` controls rounding for percentages (default: `defaults.digits`).
- `--include-numeric` defaults to `modules.frequencies.include_numeric`.
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

- Outputs are always written to `defaults.output_dir` from `core-stats/scripts/config.yml` (not user-overridable).

- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates

Use the Markdown templates in `core-stats/assets/frequencies` when assembling frequency reports. If the template exists, `frequencies.R` uses it for `apa_report.md`.
When `--group` is provided, the grouped template is selected; otherwise the default template is used.

### YAML template controls

- Template paths can be overridden via `templates.frequencies.default` and `templates.frequencies.grouped` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`variable`, `group`, `level`, `n`, `pct_total`, `pct_valid`, `total_n`, `missing_n`, `missing_pct`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `group`).

### Note tokens

Available note tokens include:

`note_default`, `pct_total_note`, `pct_valid_note`, `missing_note`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `variable`, `group`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `missing_text`, `levels_text`, `full_sentence`.

## APA 7 Reporting Guidance

- Report each variable (and group, if used) with level counts and valid percentages.
- Note missing values in the narrative or use the table's "Missing" row.
- If no valid observations exist for a variable/group, state that explicitly.
