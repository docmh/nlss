---
name: descriptive-stats
description: Descriptive statistics for numeric variables (mean, SD, median, range, robust metrics, percentiles, outliers, missingness, CI) with optional grouping and APA-ready tables/narratives.
---

# Descriptive Stats (Base R, APA 7)

## Overview

Generate descriptive statistics in base R for psychology coursework or reports, and return an APA 7-style report (table + narrative).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose numeric variables and optional grouping variable.
3. Run `scripts/R/descriptive_stats.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

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

### Parquet input

```bash
Rscript <path to scripts/R/descriptive_stats.R> --parquet <path to parquet file> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/descriptive_stats.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.descriptive_stats.vars_default` (typically numeric columns) if omitted.
- `--group` is optional and produces grouped summaries.
- `--digits` controls rounding (default: `defaults.digits`).
- `--trim` controls the trimmed mean proportion (default: `modules.descriptive_stats.trim`).
- `--iqr-multiplier` sets the Tukey IQR multiplier for outlier counts (default: `modules.descriptive_stats.iqr_multiplier`).
- `--outlier-z` sets the z-threshold for outlier counts (default: `modules.descriptive_stats.outlier_z`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).

- `report_canonical.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Template (YAML)

Use the Markdown templates under `nlss/assets/descriptive-stats` when assembling a descriptive statistics report. If a template exists, it must be used for `report_canonical.md`.
Available templates include `default-template.md`, `robust-template.md`, and `distribution-template.md`.

- The template path can be overridden via `templates.descriptive_stats.default`, `templates.descriptive_stats.robust`, and `templates.descriptive_stats.distribution` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `median`, `min`, `max`, `variance`, `range`, `q1`, `q3`, `iqr`, `mad`, `cv`, `trimmed_mean`, `p5`, `p10`, `p90`, `p95`, `outliers_tukey`, `outliers_z`, `mode`, `n_unique`, `se`, `ci_low`, `ci_high`, `skewness`, `kurtosis`, `total_n`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note tokens

Available note tokens include:

`note_default`, `note_abbrev`, `missing_note`, `trim_note`, `robust_note`, `percentile_note`, `outlier_note`, `cv_note`, `mode_note`.

Additional template tokens include `trim`, `trim_pct`, `iqr_multiplier`, and `outlier_z`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `median`, `min`, `max`, `variance`, `range`, `q1`, `q3`, `iqr`, `mad`, `cv`, `trimmed_mean`, `p5`, `p10`, `p90`, `p95`, `outliers_tukey`, `outliers_z`, `mode`, `n_unique`, `se`, `ci_low`, `ci_high`, `full_sentence`.

## APA 7 Reporting Guidance

- Use the narrative lines as the base: `Variable: M = x.xx, SD = x.xx, 95% CI [x.xx, x.xx], n = xx, missing = x (x.x%).`
- If `n < 2` or SD is missing, state that variability cannot be estimated.
- When grouped, report each group separately before any overall comparison.
- CV is computed as `SD / |M|` and is omitted when `M = 0`.
- Mode is only reported when a single most frequent value exists; ties are omitted.
- Outlier counts use Tukey fences (`iqr_multiplier` x IQR) and `|z| > outlier_z`.
