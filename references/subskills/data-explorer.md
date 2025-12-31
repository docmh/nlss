---
name: data-explorer
description: Data dictionary overview with variable types/storage, inferred measurement levels, missingness, numeric summaries, and top-N value tables with truncation notes and NLSS format outputs.
license: Apache-2.0
---

# Data Explorer (Base R, NLSS format)

## Overview

Create a data dictionary-style overview with variable names, inferred measurement levels, missingness, and value levels. Outputs include an NLSS format-ready report and JSONL logging.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Optionally select variables; default is all columns.
3. Run `scripts/R/data_explorer.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/data_explorer.R`

Run with `Rscript` and base R only.

### CSV Input

```bash
Rscript <path to scripts/R/data_explorer.R> --csv <path to CSV file> --vars age,gender
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/data_explorer.R> --rds <path to RDS file> --vars age,gender
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/data_explorer.R> --rdata <path to RData file> --df <data frame name> --vars age,gender
```

### Parquet Input

```bash
Rscript <path to scripts/R/data_explorer.R> --parquet <path to parquet file> --vars age,gender
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/data_explorer.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.data_explorer.vars_default` (typically all columns) if omitted.
- `--max-levels` controls when level tables are truncated (default: `modules.data_explorer.max_levels`). Categorical variables with more levels are summarized with top `--top-n` levels and an "Other (remaining)" row.
- `--top-n` controls how many levels to keep when truncating (default: `modules.data_explorer.top_n`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).

- `report_canonical.md`: NLSS format report containing analysis type, tables, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## NLSS format Template (YAML)

Use the Markdown template at `assets/data-explorer/default-template.md` when assembling a data exploration report. If the template exists, `data_explorer.R` uses it for `report_canonical.md`.

- The template path can be overridden via `templates.data_explorer.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`) for the overview table.
  - `levels_table.columns`: ordered column definitions for the levels table.
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Overview table keys include:

`variable`, `class`, `storage`, `measurement_level`, `measurement_note`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `unique_n`, `example_values`, `mean`, `sd`, `min`, `max`, `median`, `q1`, `q3`, `levels_included`, `levels_truncated`, `levels_note`.

Levels table keys include:

`variable`, `level`, `n`, `pct_total`, `pct_valid`, `total_n`, `missing_n`, `missing_pct`.

### Table Tokens

Table-level tokens include:

`overview_table_body`, `levels_table_body`, `table_number_next`.

### Note Tokens

Overview note tokens:

`overview_note_default`, `overview_note_body`.

Levels note tokens:

`levels_note_default`, `levels_note_body`, `pct_total_note`, `pct_valid_note`, `missing_note`, `truncation_note`.

### Narrative Tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`label`, `variable`, `class`, `storage`, `measurement_level`, `measurement_note`, `total_n`, `valid_n`, `missing_n`, `missing_pct`, `unique_n`, `example_values`, `mean`, `sd`, `min`, `max`, `median`, `q1`, `q3`, `levels_text`, `missing_text`, `levels_truncated`, `full_sentence`.

## NLSS format Reporting Guidance

- Treat the measurement level as a heuristic; the script uses `interval/ratio` for numeric variables and cannot distinguish interval from ratio scales automatically.
- Use `Table 1` for a concise overview of variable types, missingness, and numeric summaries.
- Use `Table 2` to report value levels and valid percentages for categorical variables; mention when levels are truncated.