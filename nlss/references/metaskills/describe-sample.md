---
name: describe-sample
description: Describe the sample with missingness summaries, numeric descriptives, and categorical frequencies using existing subskills.
---

# Describe Sample (Agent-run, APA 7)

## Overview

This metaskill guides the agent to describe a sample by inspecting the dataset, clarifying key variables, and running the appropriate subskills. It logs the metaskill activation and produces APA-ready outputs via the underlying subskills.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer numeric vs categorical variables and candidate grouping variables.
5. Ask clarifying questions when needed (grouping variable, key demographics, Likert handling).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions and completion notes.
8. Log finalization, append a `# Synopsis` to `apa_report.md`, and generate `report_<YYYYMMDD>_describe-sample_<intent>.md`.

## Execution (Agent-run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta describe-sample --intent "describe the sample"
```

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\metaskill_runner.R> --csv <path to CSV file> --meta describe-sample
```

## Inputs and Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional grouping variable for comparisons.
- Optional list of key demographic variables to prioritize.

### Clarifying questions

- Which variable defines the groups or conditions (if any)?
- Which demographic variables should be highlighted?
- Should Likert/ordinal variables be treated as numeric or categorical?
- Should missingness be summarized only, or handled (imputation/drop/indicator)?

## Procedure (pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta describe-sample --intent <user intent>

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  group_candidates = categorical_vars that look like condition/group/site

if group_candidates not empty:
  ask user to confirm grouping variable (or none)

ask user to confirm key demographics and Likert handling

write plan to scratchpad.md

run descriptive-stats --vars <numeric_vars> [--group <group_var>]
run frequencies --vars <categorical_vars> [--group <group_var>]
if group_var and key categorical vars:
  run crosstabs --row <key categorical> --col <group_var>

if user requests missingness handling:
  run missings --vars all --method <chosen>

update scratchpad.md with decisions and completion
append # Synopsis to apa_report.md and write report_<YYYYMMDD>_describe-sample_<intent>.md
log metaskill finalization with metaskill-runner --phase finalization
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise (e.g., `defaults.digits`, `modules.descriptive_stats.vars_default`, `modules.frequencies.vars_default`).
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude obvious identifiers (for example `id`, `uuid`) from both numeric and categorical summaries unless explicitly requested.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.

## Outputs

- `apa_report.md`: APA-ready outputs from the subskills (descriptive stats, frequencies, and optional crosstabs) plus a final `# Synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_describe-sample_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).

## APA 7 Templates

This metaskill does not define its own APA template. It relies on the templates configured for the subskills it invokes:

- `descriptive-stats` uses `nlss/assets/descriptive-stats/default-template.md`.
- `frequencies` uses `nlss/assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `nlss/assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation/finalization logging.

## APA 7 Reporting Guidance

- Report sample size (N) and key demographics succinctly.
- Use descriptive statistics for continuous variables and frequency tables for categorical variables.
- If grouping is used, report group-wise summaries and note any differences descriptively.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
