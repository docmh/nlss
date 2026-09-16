---
name: screen-data
description: Agent-run data screening for outliers, normality, linearity, homoscedasticity, and multicollinearity using NLSS subskills with recommendations and NLSS format-ready reporting.
license: Apache-2.0
---

# Screen Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to screen a dataset before analysis. It focuses on distributional checks, outliers, linearity, homoscedasticity, and multicollinearity using existing NLSS subskills. By default it **flags** issues and recommends actions rather than modifying data; any transformations or case removals require explicit approval.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Intent/Triggers

Use this metaskill when the user asks for data screening or diagnostics prior to analysis, for example:

- "Screen the data for outliers and normality."
- "Check distributional assumptions before running models."
- "Do a data screening pass (outliers, linearity, homoscedasticity)."
- "Run diagnostics before analysis."

## Routing Guardrails

- If the request is **model-specific assumptions only**, use `check-assumptions`.
- If the request includes **data changes** (recode/transform/impute/drop), use `prepare-data`.
- If the request is a **general overview** without diagnostics, use `explore-data`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect the dataset to infer variable types, ID columns, candidate outcomes/predictors, and grouping variables.
5. Ask clarifying questions for scope and screening thresholds.
6. If the user requests citations for screening criteria, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional list of key outcomes, predictors, or grouping variables.
- Optional planned analysis type (ttest, anova, regression, mixed_models, sem).

### Clarifying Questions

- What analysis is planned (ttest/anova/regression/mixed/sem), and what are the outcomes/predictors or groups?
- Should ordinal/Likert variables be treated as numeric or categorical?
- Should outliers be **flagged only** or do you want exclusion/transform criteria?
- Do you want missingness **reported only** or handled (listwise/impute/indicator/drop)?
- Are there variables to exclude (IDs, PII, free-text)?
- Do you want screening plots (histogram/QQ/box/scatter), or tables only?

If unclear, propose a default: screen all non-ID variables, treat low-cardinality numeric as categorical, flag outliers only, and report missingness without handling.

## Procedure (Pseudocode)

```
select the current project and working dataset using the common workflow

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  id_candidates = names like id, uuid, subject, timestamp
  group_candidates = categorical variables indicating group/site/condition

ask user to confirm:
  analysis type, outcomes/predictors, grouping
  Likert handling, outlier policy, missingness handling, visuals

write plan to scratchpad.md

optional:
  if screening thresholds need literature grounding:
    run research-academia (multiple query variants; curate sources)

run data-explorer --vars <all non-id vars> [--max-levels <n>] [--top-n <n>]

run descriptive-stats --vars <numeric_vars> --template distribution
  (captures skew/kurtosis, CI/SE, and outlier counts)

if plots requested:
  run plot --type histogram/qq/box for numeric_vars
  if analysis involves relationships:
    run plot --type scatter --x <iv> --y <dv> [--smooth TRUE]

if multiple numeric predictors:
  run correlations --vars <numeric_vars> [--group <group_var>]

if analysis type specified:
  run assumptions --analysis <ttest|anova|regression|mixed_models|sem>
    with appropriate flags for dv/ivs/blocks/within/between/group

if missingness handling explicitly approved:
  run missings --vars <analysis vars> --method <chosen>

update scratchpad.md with decisions and completion
explain the results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align report using references/metaskills/format-document.md
  preserve the delivered report and selected evidence with project-report
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use config defaults for subskills unless the user specifies otherwise.
- Exclude obvious identifiers and timestamps from screening unless explicitly requested.
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Do not run `missings` or `data-transform` unless the user approves a handling strategy; these update the workspace parquet copy in place.
- Flag outliers and distributional issues rather than removing cases unless the user provides explicit exclusion criteria.
- If the analysis type is known and the user wants assumptions only, route to `check-assumptions`.
- If the analysis type is unknown, limit diagnostics to descriptive/plot-based screening and document that model-specific checks require DV/IV specification.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from `data-explorer`, `descriptive-stats`, optional `plot`/`correlations`/`assumptions`.
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
- `descriptive-stats` uses `assets/descriptive-stats/distribution-template.md` (default for screening).
- `plot` uses `assets/plot/default-template.md` (if visuals are requested).
- `correlations` uses `assets/correlations/default-template.md` (if used).
- `assumptions` uses `assets/assumptions/*-template.md` depending on analysis type.
- `missings` uses `assets/missings/default-template.md` (only if handling is approved).

## NLSS format Reporting Guidance

- Report sample size (N), missingness ranges, and variable types for screened variables.
- Summarize distributional shape, skew/kurtosis, and outlier counts for numeric variables.
- For linearity and homoscedasticity, note visual patterns and any diagnostic test results.
- If model-specific assumptions were checked, report the tests used and any violations with recommended remedies (transformations, robust tests, or alternative models).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
