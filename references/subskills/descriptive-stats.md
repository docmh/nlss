---
name: descriptive-stats
description: Numeric descriptives with missingness, robust and percentile metrics, outlier counts (Tukey/z), CI/SE, skew/kurtosis, optional grouping, and NLSS format outputs.
---

# Descriptive Stats (Base R, NLSS format)

## Overview

Generate descriptive statistics in base R for psychology coursework or reports, and return an NLSS format report (table + narrative).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose numeric variables and optional grouping variable.
3. Run `scripts/R/descriptive_stats.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/descriptive_stats.R`

Run with `Rscript` and base R only.

### CSV Input

```bash
Rscript <path to scripts/R/descriptive_stats.R> --csv <path to CSV file> --vars age,score --group condition
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/descriptive_stats.R> --rds <path to RDS file> --vars age,score
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/descriptive_stats.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet Input

```bash
Rscript <path to scripts/R/descriptive_stats.R> --parquet <path to parquet file> --vars age,score
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/descriptive_stats.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.descriptive_stats.vars_default` (typically numeric columns) if omitted.
- `--group` is optional and produces grouped summaries.
- `--digits` controls rounding (default: `defaults.digits`).
- `--trim` controls the trimmed mean proportion (default: `modules.descriptive_stats.trim`).
- `--iqr-multiplier` sets the Tukey IQR multiplier for outlier counts (default: `modules.descriptive_stats.iqr_multiplier`).
- `--outlier-z` sets the z-threshold for outlier counts (default: `modules.descriptive_stats.outlier_z`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## NLSS format Template (YAML)

Use the Markdown templates under `assets/descriptive-stats` when assembling a descriptive statistics report. If a template exists, it must be used for `report_canonical.md`.
Available templates include `default-template.md`, `robust-template.md`, and `distribution-template.md`.

- The template path can be overridden via `templates.descriptive_stats.default`, `templates.descriptive_stats.robust`, and `templates.descriptive_stats.distribution` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `median`, `min`, `max`, `variance`, `range`, `q1`, `q3`, `iqr`, `mad`, `cv`, `trimmed_mean`, `p5`, `p10`, `p90`, `p95`, `outliers_tukey`, `outliers_z`, `mode`, `n_unique`, `se`, `ci_low`, `ci_high`, `skewness`, `kurtosis`, `total_n`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note Tokens

Available note tokens include:

`note_default`, `note_abbrev`, `missing_note`, `trim_note`, `robust_note`, `percentile_note`, `outlier_note`, `cv_note`, `mode_note`.

Additional template tokens include `trim`, `trim_pct`, `iqr_multiplier`, and `outlier_z`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `variable`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `median`, `min`, `max`, `variance`, `range`, `q1`, `q3`, `iqr`, `mad`, `cv`, `trimmed_mean`, `p5`, `p10`, `p90`, `p95`, `outliers_tukey`, `outliers_z`, `mode`, `n_unique`, `se`, `ci_low`, `ci_high`, `full_sentence`.

## NLSS format Reporting Guidance

- Use the narrative lines as the base: Variable: *M* = x.xx, *SD* = x.xx, 95% CI [x.xx, x.xx], *n* = xx, missing = x (x.x%).
- If `n < 2` or *SD* is missing, state that variability cannot be estimated.
- When grouped, report each group separately before any overall comparison.
- CV is computed as *SD* / |*M*| and is omitted when *M* = 0.
- Mode is only reported when a single most frequent value exists; ties are omitted.
- Outlier counts use Tukey fences (`iqr_multiplier` x IQR) and `|z| > outlier_z`.
