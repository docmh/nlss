---
name: describe-sample
description: Agent-run sample description that inspects data, infers demographics, runs descriptive-stats/frequencies (plus optional crosstabs/missings), and writes an NLSS format-ready report.
license: Apache-2.0
---

# Describe Sample (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to describe a sample by inspecting the dataset, clarifying key variables, and running the appropriate subskills. It is designed to handle vague requests like "describe the demographics" by inferring likely demographic variables, confirming assumptions, and producing NLSS format-ready outputs via the underlying subskills.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Intent/Triggers

Use this metaskill when the user asks for sample or demographic descriptions, for example:

- "Please take this dataset and describe the demographics of our sample."
- "Provide a sample description."
- "Summarize participant characteristics."
- "Describe age, gender, education, and employment."

## Routing Guardrails

- If the request is a **general dataset overview**, use `explore-data`.
- If the request is about **screening diagnostics** (outliers/normality), use `screen-data`.
- If the request is about **data cleaning or transformations**, use `prepare-data`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect the dataset to infer numeric vs categorical variables and candidate grouping variables.
5. Ask clarifying questions when needed (grouping variable, key demographics, Likert handling), and explicitly propose a demographic-first summary as the default.
6. If the user requests literature context for the sample (norms, benchmarks, or population comparisons), run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional grouping variable for comparisons.
- Optional list of key demographic variables to prioritize.
- Optional grouping variable for comparisons (condition, site, cohort).

### Clarifying Questions

- Which variable defines the groups or conditions (if any)?
- Which demographic variables should be highlighted or excluded?
- Do any variables represent outcomes or scale scores that should not be treated as demographics?
- Should ordinal demographics (education, income) be treated as categorical or numeric?
- Should missingness be summarized only, or handled (imputation/drop/indicator)?

If unclear, suggest a demographic-first summary (age, gender/sex, education, employment, income, marital status, ethnicity/race, country, language) as the default.

## Procedure (Pseudocode)

```
select the current project and working dataset using the common workflow

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  demographic_candidates = name patterns (age, gender, sex, edu, income, employ, marital, race, ethnicity, country, language)
  group_candidates = categorical_vars that look like condition/group/site

if group_candidates not empty:
  ask user to confirm grouping variable (or none)

ask user to confirm key demographics and ordinal handling
if request is vague:
  propose demographic-first summary as default (age, gender/sex, education, employment, income, marital status, ethnicity/race)
  if demographics are unclear:
    run data-explorer --vars <demographic_candidates> to summarize levels and ranges

write plan to scratchpad.md

optional:
  if demographic benchmarks or norms need citations:
    run research-academia (multiple query variants; curate sources)

run descriptive-stats --vars <numeric_demographics> [--group <group_var>]
run frequencies --vars <categorical_demographics> [--group <group_var>]
if group_var and key categorical vars:
  run crosstabs --row <key categorical> --col <group_var>

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
- Use config defaults for subskills unless the user specifies otherwise (e.g., `defaults.digits`, `modules.descriptive_stats.vars_default`, `modules.frequencies.vars_default`).
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude obvious identifiers (for example `id`, `uuid`, `timestamp`) and derived outcomes (`*_score`, `*_total`, `*_sum`, `*_mean`) from demographic summaries unless explicitly requested.
- If demographics are not specified, infer them by name patterns and value ranges, then confirm with the user; prioritize age, gender/sex, education, employment, income, marital status, ethnicity/race, country, and language.
- If ordinal demographics (education/income) are ambiguous, default to categorical reporting.
- When the request is ambiguous, default to a demographic-first summary and state this as the proposed focus.
- Do not run `missings` unless the user requests missingness handling; it updates visible working data through the shared before/after version boundary.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the subskills (descriptive stats, frequencies, and optional crosstabs).
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

- `descriptive-stats` uses `assets/descriptive-stats/default-template.md`.
- `frequencies` uses `assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `data-explorer` uses `assets/data-explorer/default-template.md` when requested.

## NLSS format Reporting Guidance

- Report sample size (N) and key demographics succinctly.
- Use descriptive statistics for continuous variables and frequency tables for categorical variables.
- If grouping is used, report group-wise summaries and note any differences descriptively.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
