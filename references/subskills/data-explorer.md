---
name: data-explorer
description: Data dictionary overview with variable types/storage, inferred measurement levels, missingness, numeric summaries, and top-N value tables with truncation notes and NLSS format outputs.
license: Apache-2.0
---

# Data Explorer (Base R, NLSS format)

## Overview

Create a data dictionary-style overview with variable names, inferred measurement levels, missingness, and value levels. Outputs include an NLSS format-ready report and saved run results.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Optionally select variables; default is all columns.
3. Run the `data-explorer` operation through `run_nlss.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `result.json`) to craft the response.

## Execution: `data-explorer`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

Run with `Rscript` and base R only.

### CSV Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" data-explorer --csv <path to CSV file> --vars age,gender
```

### RDS Input (Data Frame)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" data-explorer --rds <path to RDS file> --vars age,gender
```

### RData Input (Data Frame by Name)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" data-explorer --rdata <path to RData file> --df <data frame name> --vars age,gender
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" data-explorer --parquet <path to parquet file> --vars age,gender
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" data-explorer --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.data_explorer.vars_default` (typically all columns) if omitted.
- `--max-levels` controls when level tables are truncated (default: `modules.data_explorer.max_levels`). Categorical variables with more levels are summarized with top `--top-n` levels and an "Other (remaining)" row.
- `--top-n` controls how many levels to keep when truncating (default: `modules.data_explorer.top_n`).
- Both level limits must be positive integers; `--digits` must be an integer from 0 through 15. Invalid values fail explicitly rather than being silently truncated.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Outputs

This migrated module appends to `report_canonical.md` and also publishes a
project-local `.nlss/runs/<run-id>/` bundle with resolved `request.json`, full-precision
`result.json` and deterministic `output.md`. See the [run contract](../run-contract.md)
for replay and failure handling. `--log FALSE` disables only optional standalone logging, not
the run bundle. Standalone semantic research reports remain agent-written.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis type, tables, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `result.json` retains `overview_df` and `levels_df`, including actual per-level
  counts and total/valid percentages. The request records input/dictionary hashes,
  variable classes, factor levels and that measurement levels are heuristic.
- `levels_df.level_kind` distinguishes `observed`, `remainder` and `no_valid_data`
  rows. A remainder row gets a distinct label if a real code/value label already
  uses `Other (remaining)`; synthetic rows are not assigned a user's value label.
  `levels_truncated` is false when top N retains every level.
- Run-local tables always begin at 1 and 2; root protocol numbering continues
  independently. Replaying a saved request uses its preserved input and template.

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
