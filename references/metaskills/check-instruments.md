---
name: check-instruments
description: Agent-run instrument checks that infer item groups, handle reverse scoring, run scale (alpha/omega) and reliability (ICC/kappa/test-retest), and report NLSS format-ready results.
license: Apache-2.0
---

# Check Instruments (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent through instrument quality checks for surveys or questionnaires, especially when the request is vague (for example, "check the quality of the instruments"). It focuses on item inspection, reverse scoring, scale construction, and reliability reporting using existing subskills.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Intent/Triggers

Use this metaskill when the user asks for instrument quality, reliability, or scale checks, for example:

- "Please take this dataset and figure out the quality of used instruments."
- "Check the reliability of our questionnaires."
- "Assess scale quality for these survey items."
- "Run item analysis and reliability."

## Routing Guardrails

- If the request is about **demographics or sample description**, use `describe-sample`.
- If the request is a **general dataset overview**, use `explore-data`.
- If the request is **screening diagnostics** (outliers/normality) not tied to instruments, use `screen-data`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect items to infer candidate instruments, item ranges, and missingness (use `data-explorer` if helpful).
5. Ask clarifying questions (scale definitions, reverse-coded items, scoring method, reliability type).
6. If scale definitions or reliability benchmarks need citations, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Scale definitions (items per scale) or naming conventions for item groups.
- Reverse-coded items and min/max for item scales.
- Scoring method (sum vs mean).
- Reliability type: internal consistency (alpha/omega) vs test-retest/ICC/kappa.
- Optional grouping variable (condition, cohort, site).

### Clarifying Questions

- Which instruments or scales are in the dataset, and which items belong to each scale?
- Do item names share prefixes that map to scales (for example PHQ9_1..PHQ9_9)?
- Which items are reverse-coded, and what are the min/max values?
- Should scale scores use sum or mean?
- Is reliability internal consistency (alpha/omega) only, or do you need ICC/kappa/test-retest?
- Is this cross-sectional, test-retest, or multi-rater data (any ID/time/rater variables)?
- Should analyses be grouped (e.g., by condition or cohort)?
- Should missingness be summarized only, or handled (listwise/impute/indicator/drop)?

If the request is vague, propose a default: infer item groups by prefix and Likert range, run internal consistency (alpha/omega), and report item diagnostics plus missingness summaries.

## Procedure (Pseudocode)

```
select the current project and working dataset using the common workflow

inspect dataset:
  confirm item ranges and missingness
  flag candidate scales/items from user list or naming patterns
  if unclear, run data-explorer to summarize candidate Likert items
  write candidate groups and assumptions to scratchpad.md

ask user to confirm:
  scale definitions, reverse-coded items, min/max
  scoring method (sum/mean)
  reliability type (alpha/omega vs ICC/kappa/test-retest)
  optional grouping variable

write plan to scratchpad.md

optional:
  if instrument documentation or reliability benchmarks need literature support:
    run research-academia (multiple query variants; curate sources)

for each scale:
  run scale --vars <items> [--reverse <items>] [--reverse-min <min>] [--reverse-max <max>] \
            [--score <sum|mean>] [--missing <pairwise|complete>] [--group <group_var>]

if test-retest or inter-rater reliability requested:
  run reliability --analysis <icc|kappa|correlation> --vars <measurements> [--format <wide|long>] \
                  [--group <group_var>]

optional:
  run data-explorer --vars <item_candidates> if item ranges or types are still unclear
  run correlations --vars <scale_scores> [--group <group_var>]
  run missings --vars <items> --method <chosen> (only if user requests handling)

update scratchpad.md with decisions and completion
explain the results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align <chosen-visible-report>.md using references/metaskills/format-document.md
  preserve the delivered report and selected evidence with project-report
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use config defaults for subskills unless the user specifies otherwise (for example `modules.scale.score`, `modules.scale.omega`).
- Treat Likert items as numeric by default unless the user prefers categorical handling.
- Exclude identifiers (for example `id`, `uuid`) and precomputed totals (for example `*_total`, `*_sum`, `*_mean`) from item lists unless requested.
- If scale definitions are missing, infer candidate groups by shared name prefixes and similar value ranges; require at least 3 items per group and mark them as provisional in `scratchpad.md`.
- If reverse-coded items are unknown, do not reverse by default; flag negative item-total correlations for review.
- Do not run `missings` unless the user requests missingness handling; it updates visible working data through the shared before/after version boundary.
- "Quality" here means reliability and item diagnostics; validity evidence (factor analysis, IRT) is out of scope unless the user requests it explicitly.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from `scale`, `reliability`, optional `data-explorer`/`correlations`.
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

- `scale` uses `assets/scale/default-template.md`.
- `reliability` uses `assets/reliability/default-template.md`.
- `correlations` uses `assets/correlations/default-template.md` when requested.
- `data-explorer` uses `assets/data-explorer/default-template.md` when requested.

## NLSS format Reporting Guidance

- Report scale reliability (alpha/omega) and item diagnostics where applicable.
- For test-retest or inter-rater analyses, report ICC/kappa with confidence intervals.
- Summarize missingness handling decisions when applied.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
