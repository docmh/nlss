---
name: explore-data
description: Explore a dataset with variable summaries, missingness, distributions, and correlations to provide an APA-ready overview from vague requests.
---

# Explore Data (Agent-run, APA 7)

## Overview

This metaskill guides the agent to provide a structured overview of a dataset when the request is vague (for example, "give me an overview"). It inspects the data, asks clarifying questions, and runs exploratory subskills to summarize variables, distributions, missingness, and relationships.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks for exploratory overviews or dataset summaries, for example:

- "Please take this dataset and give me an overview of its content."
- "Explore the data and summarize what is inside."
- "Give me a broad look at distributions, missingness, and relationships."
- "Quick data audit before analysis."

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer variable types, candidate IDs, demographics, and grouping variables.
5. Ask clarifying questions for scope (variables to prioritize, grouping, Likert handling, sensitive fields).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions and completion notes.
8. Log finalization, append a `# Synopsis` to `report_canonical.md`, and generate `report_<YYYYMMDD>_explore-data_<intent>.md`.

## Execution (Agent-run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta explore-data --intent "explore the dataset"
```

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\metaskill_runner.R> --csv <path to CSV file> --meta explore-data
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional list of key variables or domains to prioritize.
- Optional grouping variable for comparisons.

### Clarifying questions

- Which variables or domains are most important to focus on?
- Is there a grouping variable (condition, site, cohort) to compare?
- Should ordinal/Likert variables be treated as numeric or categorical?
- Are there sensitive fields (PII) to exclude from summaries?
- Do you want visuals (histograms, boxplots, correlation heatmap), or tables only?

If unclear, propose a default: summarize all variables (excluding IDs), treat low-cardinality numeric as categorical, and provide distributions, missingness, and a correlation overview for numeric variables.

## Procedure (pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta explore-data --intent <user intent>

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  group_candidates = categorical_vars that look like condition/group/site
  id_candidates = names like id, uuid, subject, timestamp

ask user to confirm:
  priority variables, grouping variable, Likert handling, visuals

write plan to scratchpad.md

run data-explorer --vars <all non-id vars> [--max-levels <n>] [--top-n <n>]
run descriptive-stats --vars <numeric_vars> [--group <group_var>]
run frequencies --vars <categorical_vars> [--group <group_var>]
if group_var and key categorical vars:
  run crosstabs --row <key categorical> --col <group_var>
if numeric_vars length >= 2:
  run correlations --vars <numeric_vars> [--group <group_var>]
if visuals requested:
  run plot --type histogram/box/heatmap with priority vars

if user requests missingness handling:
  run missings --vars all --method <chosen>

update scratchpad.md with decisions and completion
append # Synopsis to report_canonical.md and write report_<YYYYMMDD>_explore-data_<intent>.md
log metaskill finalization with metaskill-runner --phase finalization
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise.
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude identifiers (for example `id`, `uuid`, `timestamp`) and free-text fields from summaries unless explicitly requested.
- When the request is ambiguous, default to a broad overview: data-explorer, descriptive stats, frequencies, and numeric correlations.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.
- If the dataset is very wide, ask to prioritize variables or cap outputs (for example top N variables).

## Outputs

- `report_canonical.md`: APA-ready outputs from `data-explorer`, `descriptive-stats`, `frequencies`, optional `correlations`/`plots`, plus a final `# Synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_explore-data_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

### Final report requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Synthesize results across subskills; include tables/figures with captions and in-text references.
- Keep the report APA 7-ready and suitable for journal submission.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Log completion with `metaskill-runner --phase finalization`.
- Append a `# Synopsis` section to `report_canonical.md`.
- Write `report_<YYYYMMDD>_explore-data_<intent>.md` using an ASCII slug for `<intent>`.

## APA 7 Templates

This metaskill does not define its own APA template. It relies on the templates configured for the subskills it invokes:

- `data-explorer` uses `nlss/assets/data-explorer/default-template.md`.
- `descriptive-stats` uses `nlss/assets/descriptive-stats/default-template.md`.
- `frequencies` uses `nlss/assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `nlss/assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `correlations` uses `nlss/assets/correlations/default-template.md`.
- `plot` uses `nlss/assets/plot/default-template.md`.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation/finalization logging.

## APA 7 Reporting Guidance

- Report sample size (N), missingness patterns, and variable types.
- Summarize distributions for numeric variables and frequency tables for categorical variables.
- If correlations are included, report method and note any notable associations descriptively.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages(\"arrow\")`).
