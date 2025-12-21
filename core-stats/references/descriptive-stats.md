---
name: descriptive-stats-r
description: Descriptive statistics for numeric variables (mean, SD, median, range, skewness/kurtosis, missingness, CI) with optional grouping and APA-ready tables/narratives.
---

# Descriptive Stats (Base R, APA 7)

## Overview

Generate descriptive statistics in base R for psychology coursework or reports, and return an APA 7-style report (table + narrative).

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose numeric variables and optional grouping variable.
3. Run `scripts/R/descriptive_stats.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/descriptive_stats.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\descriptive_stats.R> --csv <path to CSV file> --vars age,score --group condition
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

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.descriptive_stats.vars_default` (typically numeric columns) if omitted.
- `--group` is optional and produces grouped summaries.
- `--digits` controls rounding (default: `defaults.digits`).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

- Outputs are always written to `defaults.output_dir` from `core-stats/scripts/config.yml` (not user-overridable).

- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Template (YAML)

Use the Markdown template at `core-stats/assets/descriptive-stats/default-template.md` when assembling a descriptive statistics report. If the template exists, it must be used for `apa_report.md`.

- The template path can be overridden via `templates.descriptive_stats.default` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `min`, `max`, `median`, `se`, `ci_low`, `ci_high`, `skewness`, `kurtosis`, `total_n`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note tokens

Available note tokens include:

`note_default`, `note_abbrev`, `missing_note`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `min`, `max`, `ci_low`, `ci_high`, `full_sentence`.

## APA 7 Reporting Guidance

- Use the narrative lines as the base: `Variable: M = x.xx, SD = x.xx, 95% CI [x.xx, x.xx], n = xx, missing = x (x.x%).`
- If `n < 2` or SD is missing, state that variability cannot be estimated.
- When grouped, report each group separately before any overall comparison.
