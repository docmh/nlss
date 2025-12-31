---
name: frequencies
description: Frequency tables for categorical variables with counts, valid/total percentages, missingness, optional grouping, and NLSS format outputs (optionally include numeric vars).
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2026 Mike Hammes"
  nlss.version: "1.0.0"
---

# Frequencies (Base R, NLSS format)

## Overview

Generate frequency tables in base R for categorical variables and return an NLSS format report (table + narrative). Factor levels are preserved; non-factor variables are sorted by their unique values. Missing values are reported separately and excluded from valid percentages.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables for frequency tables and an optional grouping variable.
3. Run `scripts/R/frequencies.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/frequencies.R`

Run with `Rscript` and base R only.

### CSV Input

```bash
Rscript <path to scripts/R/frequencies.R> --csv <path to CSV file> --vars gender,condition --group condition
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/frequencies.R> --rds <path to RDS file> --vars gender,condition
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/frequencies.R> --rdata <path to RData file> --df <data frame name> --vars gender,condition
```

### Parquet Input

```bash
Rscript <path to scripts/R/frequencies.R> --parquet <path to parquet file> --vars gender,condition
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/frequencies.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.frequencies.vars_default` (typically non-numeric columns). Use `--include-numeric` to include numeric columns when `--vars` is omitted.
- `--group` is optional and produces grouped frequency tables (one grouping variable).
- `--digits` controls rounding for percentages (default: `defaults.digits`).
- `--include-numeric` defaults to `modules.frequencies.include_numeric`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## NLSS format Templates

Use the Markdown templates in `assets/frequencies` when assembling frequency reports. If the template exists, `frequencies.R` uses it for `report_canonical.md`.
When `--group` is provided, the grouped template is selected; otherwise the default template is used.

### YAML Template Controls

- Template paths can be overridden via `templates.frequencies.default` and `templates.frequencies.grouped` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`variable`, `group`, `level`, `n`, `pct_total`, `pct_valid`, `total_n`, `missing_n`, `missing_pct`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `group`).

### Note Tokens

Available note tokens include:

`note_default`, `pct_total_note`, `pct_valid_note`, `missing_note`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `variable`, `group`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `missing_text`, `levels_text`, `full_sentence`.

## NLSS format Reporting Guidance

- Report each variable (and group, if used) with level counts and valid percentages.
- Note missing values in the narrative or use the table's "Missing" row.
- If no valid observations exist for a variable/group, state that explicitly.