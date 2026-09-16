---
name: frequencies
description: Frequency tables for categorical variables with counts, valid/total percentages, missingness, optional grouping, and NLSS format outputs (optionally include numeric vars).
license: Apache-2.0
---

# Frequencies (Base R, NLSS format)

## Overview

Generate frequency tables in base R for categorical variables and return an NLSS format report (table + narrative). Factor levels are preserved; non-factor variables are sorted by their unique values. Missing values are reported separately and excluded from valid percentages. Numeric value labels do not automatically turn a variable into a factor: select the variable explicitly or use `--include-numeric` when appropriate.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive); review [the import contract](../import-contract.md) for labels and user-defined missings.
2. Choose variables for frequency tables and an optional grouping variable.
3. Run `scripts/R/frequencies.R` with the correct flags.
4. Review the saved run, canonical Markdown, and its recorded status to craft a context-sensitive response. The statistical output is evidence, not a prescribed final-report narrative.

## Script: `scripts/R/frequencies.R`

Run with `Rscript`. Frequency calculations use base R; the shared runtime requires the installed NLSS import/configuration/report dependencies, including `arrow` for workspace Parquet and `haven` for SAV import.

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
- `--group` is optional and produces grouped frequency tables (one grouping variable). Groups follow first appearance, while factor category levels retain their original order, including unused levels when valid data exist. Missing grouping values form a separate group; they never add artificial missing observations to other groups.
- `--digits` controls display rounding for percentages, not saved numeric precision (integer 0–15; default: `defaults.digits`).
- `--include-numeric` defaults to `modules.frequencies.include_numeric`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles the optional standalone log (default: `defaults.log`, falling back to TRUE). It does not disable the mandatory run bundle.
- `--user-prompt` stores the original AI prompt when `logging.include_user_prompt` permits it; otherwise it is excluded from the saved request and staged log.

## Outputs

Frequencies uses the shared [run contract](../run-contract.md). Each completed analysis publishes a project-local `.nlss/runs/<run-id>/` bundle alongside the automatic root canonical report. This deterministic per-run output is not a standalone semantic research report; authored reports use freely chosen visible Markdown paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `.nlss/runs/<run-id>/request.json`: Resolved variables, grouping, display digits and numeric-selection policy; immutable dataset/dictionary references; variable classes and levels, missing counts, and each group's original row indices. Each group records its original `value`, `is_missing`, and unique presentation key `group`.
- `.nlss/runs/<run-id>/result.json`: Completion or failure state, warnings and full-precision `results.summary_df`, independent of legacy logging settings. The additive Boolean `group_missing` distinguishes true missing grouping values from literal category text.
- `.nlss/runs/<run-id>/output.md` and `templates/`: Deterministic statistical output and the exact template used. `replay_run.R --request <saved request.json>` verifies and repeats a completed run from its immutable input, configuration and template.

Missing groups display as `NA` only if that does not collide with an actual group code or label. Otherwise the label is `NA (missing)`, with a numeric suffix if necessary. A literal `NA` group remains a separate, ordinary category. This corrects the older grouping path that could insert artificial missing rows into every non-missing group and merge missing groups with literal `NA` in reports. Counts and percentages now use exactly the recorded source rows.

Unknown variables/groups, invalid display digits, and grouped inputs with no observed groups fail explicitly. All-missing analysis variables remain valid descriptive results with no valid observations. A failed run does not publish a normal `output.md` or append a successful result to the canonical report; see the run contract for pre-input failures and interrupted-run limitations.

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
