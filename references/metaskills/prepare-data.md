---
name: prepare-data
description: Agent-run data preparation that audits variables, handles missingness (missings/impute), recodes/transforms/renames, optionally builds scales, documents changes, and produces NLSS format-ready reports.
license: Apache-2.0
---

# Prepare Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent through data preparation when the request is vague (for example, "make this data ready for analysis"). It emphasizes dataset inspection, clear decisions about missingness and coding, and transparent documentation of any transformations or exclusions.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Intent/Triggers

Use this metaskill when the user asks for data cleaning or preparation, for example:

- "Please make this data ready for analysis."
- "Clean this dataset before running models."
- "Prepare the data (missingness, recodes, transformations)."
- "Get the data into analysis-ready form."

## Routing Guardrails

- If the user wants **diagnostics only** (outliers/normality/linearity), use `screen-data`.
- If the user wants **model-specific assumption checks only**, use `check-assumptions`.
- If the user wants **hypothesis tests**, use `test-hypotheses`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect the dataset to infer variable types, IDs, suspicious codes, and candidate analysis variables; summarize candidates in `scratchpad.md`.
5. Ask clarifying questions on missingness handling, recodes, exclusions, and overwrites.
6. If preprocessing choices need literature support (imputation, transformations, exclusions), run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a step-by-step plan to `scratchpad.md`, then execute subskills in order.
8. Retain material preparation decisions and transformations in `scratchpad.md`.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Optional list of variables to prioritize or exclude.
- Optional planned analysis (outcomes, predictors, grouping variables).

### Clarifying Questions

- What analysis is planned (outcomes, predictors, grouping variables)?
- Are there known missing value codes (e.g., 99, -9, 999) that should be converted to NA?
- Which missingness strategy should be used (auto, listwise, impute, indicator, drop), and should we preserve originals?
- Are there required recodes (category merging, reverse coding, binary indicators)?
- Should any variables be renamed, standardized, or transformed (log, sqrt, z-score)?
- Are there exclusion criteria (range checks, invalid cases) or outliers to flag?
- Is it acceptable to overwrite variables or drop columns, or should new variables be created instead?

If unclear, propose a default: audit all variables first, convert known missing codes to NA, prefer new variables (no overwrite), and use `missings --method auto` only after explicit approval.

## Procedure (Pseudocode)

```
select the current project and working dataset using the common workflow

inspect dataset:
  numeric_vars = numeric columns minus IDs
  categorical_vars = factors/characters or low-cardinality numeric
  id_candidates = names like id, uuid, subject, timestamp
  suspicious_codes = common missing codes (e.g., 99, -9, 999)

write candidate summary to scratchpad.md

ask user to confirm:
  analysis variables, grouping, missingness strategy
  recodes/transforms, exclusions, and overwrite/drop rules

write plan to scratchpad.md

optional:
  if preprocessing choices require citations (e.g., imputation/transformations):
    run research-academia (multiple query variants; curate sources)

run data-explorer --vars <all non-id vars> [--max-levels <n>] [--top-n <n>]

if missingness handling approved:
  run missings --vars <analysis vars> --method <chosen>
  if user requests multiple imputation or new *_imp columns:
    run impute --vars <analysis vars> --method <chosen>

if recodes/transforms approved:
  run data-transform --recode/--transform/--standardize/--calc
  if dropping columns or overwriting variables:
    include --confirm-drop and/or --confirm-overwrite

if scale prep requested (reverse scoring, scores):
  run scale --vars <items> [--reverse <items>]
  run data-transform --calc <scale_score> if scores must be written

if post-clean checks requested:
  run descriptive-stats --vars <numeric_vars> [--template distribution|robust]
  run frequencies --vars <categorical_vars>

retain material preparation decisions and a concise completion summary in scratchpad.md
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
- Exclude obvious identifiers and timestamps from transformations and missingness handling unless explicitly requested.
- Do not overwrite variables or drop columns without explicit approval; prefer new variables with suffixes.
- Run `missings` only after the user approves a handling strategy; it updates the workspace parquet copy and creates a backup.
- Use `impute` when the user requests multiple imputation or new `_imp` columns; otherwise use `missings` for simple handling.
- For outliers, default to flagging or documenting them rather than removing cases unless the user specifies removal criteria.
- If the dataset is very wide, ask the user to prioritize variables or domains.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the subskills (data-explorer, missings, data-transform, optional descriptive/frequency checks).
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
- `missings` uses `assets/missings/default-template.md`.
- `data-transform` uses `assets/data-transform/default-template.md`.
- `impute` uses `assets/impute/default-template.md` (if used).
- `descriptive-stats` uses `assets/descriptive-stats/default-template.md` (or distribution/robust templates when requested).
- `frequencies` uses `assets/frequencies/default-template.md`.
- `scale` uses `assets/scale/default-template.md` (if used).
- `plot` uses `assets/plot/default-template.md` (if visuals are requested).

## NLSS format Reporting Guidance

- Report missingness patterns and the chosen handling strategy (listwise, impute, indicator, drop), including any thresholds.
- Document recodes, transformations, standardizations, and variable creation (with justification).
- Note any exclusions or outlier handling and whether cases were removed or only flagged.
- Summarize the resulting dataset readiness for the intended analysis (variables retained, scales computed, coding decisions).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
