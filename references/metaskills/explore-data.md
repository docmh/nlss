---
name: explore-data
description: Agent-run dataset overview using data-explorer, descriptives, frequencies, correlations, optional plots, and missingness summaries to produce an NLSS format-ready audit.
license: Apache-2.0
---

# Explore Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to provide a structured overview of a dataset when the request is vague (for example, "give me an overview"). It inspects the data, asks clarifying questions, and runs exploratory subskills to summarize variables, distributions, missingness, and relationships.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Intent/Triggers

Use this metaskill when the user asks for exploratory overviews or dataset summaries, for example:

- "Please take this dataset and give me an overview of its content."
- "Explore the data and summarize what is inside."
- "Give me a broad look at distributions, missingness, and relationships."
- "Quick data audit before analysis."

## Routing Guardrails

- If the request is specifically about **demographics or participant characteristics**, use `describe-sample`.
- If the request is explicitly about **diagnostics or screening** (outliers/normality/linearity), use `screen-data`.
- If the user wants **data cleaning or transformations**, use `prepare-data`.
- If the user wants **model-specific assumptions**, use `check-assumptions`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect the dataset to infer variable types, candidate IDs, demographics, and grouping variables.
5. Ask clarifying questions for scope (variables to prioritize, grouping, Likert handling, sensitive fields).
6. If the user asks for literature context around key variables or domains, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


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
select the current project and working dataset using the common workflow

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  group_candidates = categorical_vars that look like condition/group/site
  id_candidates = names like id, uuid, subject, timestamp

ask user to confirm:
  priority variables, grouping variable, Likert handling, visuals

write plan to scratchpad.md

optional:
  if the overview requires literature context for variables/domains:
    run research-academia (multiple query variants; curate sources)

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
explain the results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align <chosen-visible-report>.md using references/metaskills/format-document.md
  preserve the delivered report and selected evidence with project-report
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use config defaults for subskills unless the user specifies otherwise.
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude identifiers (for example `id`, `uuid`, `timestamp`) and free-text fields from summaries unless explicitly requested.
- When the request is ambiguous, default to a broad overview: data-explorer, descriptive stats, frequencies, and numeric correlations.
- Do not run `missings` unless the user requests missingness handling; it updates visible working data through the shared before/after version boundary.
- If the dataset is very wide, ask to prioritize variables or cap outputs (for example top N variables).

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from `data-explorer`, `descriptive-stats`, `frequencies`, optional `correlations`/`plots`.
- `.nlss/runs/`: saved requests/results/output from the underlying procedures; no additional lifecycle/JSONL journal.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `<chosen-visible-report>.md`: Only when requested; a freely authored report with useful tables/figures.

### Final Report Requirements

For a requested report, follow the shared [semantic synthesis and presentation
guidance](../../SKILL.md#semantic-answers-and-authored-reports). The manuscript
scaffold is optional; use the structure and depth appropriate to the question.

Use the common project destinations in `SKILL.md`: automatic root protocol,
run-local evidence, visible working data and freely chosen authored-report paths.

## Report delivery

Use [project-report](../utilities/project-report.md) as part of requested delivery.
The agent selects the actual evidence; the researcher does not manage internal
IDs or perform separate finalization. No required filename, synopsis append or
additional utility event. Preserve semantic interpretation beyond output templates.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `data-explorer` uses `assets/data-explorer/default-template.md`.
- `descriptive-stats` uses `assets/descriptive-stats/default-template.md`.
- `frequencies` uses `assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `correlations` uses `assets/correlations/default-template.md`.
- `plot` uses `assets/plot/default-template.md`.

## NLSS format Reporting Guidance

- Report sample size (N), missingness patterns, and variable types.
- Summarize distributions for numeric variables and frequency tables for categorical variables.
- If correlations are included, report method and note any notable associations descriptively.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages(\"arrow\")`).
