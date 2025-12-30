---
name: explore-data
description: Agent-run dataset overview using data-explorer, descriptives, frequencies, correlations, optional plots, and missingness summaries to produce an NLSS format-ready audit.
---

# Explore Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to provide a structured overview of a dataset when the request is vague (for example, "give me an overview"). It inspects the data, asks clarifying questions, and runs exploratory subskills to summarize variables, distributions, missingness, and relationships.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

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
8. Generate `report_<YYYYMMDD>_explore-data_<intent>.md` first, align it using `nlss/references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta explore-data --intent "explore the dataset"
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional list of key variables or domains to prioritize.
- Optional grouping variable for comparisons.

### Clarifying Questions

- Which variables or domains are most important to focus on?
- Is there a grouping variable (condition, site, cohort) to compare?
- Should ordinal/Likert variables be treated as numeric or categorical?
- Are there sensitive fields (PII) to exclude from summaries?
- Do you want visuals (histograms, boxplots, correlation heatmap), or tables only?

If unclear, propose a default: summarize all variables (excluding IDs), treat low-cardinality numeric as categorical, and provide distributions, missingness, and a correlation overview for numeric variables.

## Procedure (Pseudocode)

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
write report_<YYYYMMDD>_explore-data_<intent>.md
align report_<YYYYMMDD>_explore-data_<intent>.md using nlss/references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise.
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude identifiers (for example `id`, `uuid`, `timestamp`) and free-text fields from summaries unless explicitly requested.
- When the request is ambiguous, default to a broad overview: data-explorer, descriptive stats, frequencies, and numeric correlations.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.
- If the dataset is very wide, ask to prioritize variables or cap outputs (for example top N variables).

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from `data-explorer`, `descriptive-stats`, `frequencies`, optional `correlations`/`plots`, plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_explore-data_<intent>.md`: NLSS format-ready, journal-ready narrative report with ad hoc tables/plots as needed.

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `nlss/assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; integrate tables/figures with captions and in-text references.
- Craft tables and figures specifically for the report rather than copying them from `report_canonical.md`.
- Keep the report NLSS format-ready and suitable for journal submission.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_explore-data_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `nlss/references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `data-explorer` uses `nlss/assets/data-explorer/default-template.md`.
- `descriptive-stats` uses `nlss/assets/descriptive-stats/default-template.md`.
- `frequencies` uses `nlss/assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `nlss/assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `correlations` uses `nlss/assets/correlations/default-template.md`.
- `plot` uses `nlss/assets/plot/default-template.md`.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation and `nlss/assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Report sample size (*N*), missingness patterns, and variable types.
- Summarize distributions for numeric variables and frequency tables for categorical variables.
- If correlations are included, report method and note any notable associations descriptively.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages(\"arrow\")`).