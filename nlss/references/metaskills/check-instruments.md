---
name: check-instruments
description: Agent-run instrument checks that infer item groups, handle reverse scoring, run scale (alpha/omega) and reliability (ICC/kappa/test-retest), and report APA-ready results.
---

# Check Instruments (Agent-run, APA 7)

## Overview

This metaskill guides the agent through instrument quality checks for surveys or questionnaires, especially when the request is vague (for example, "check the quality of the instruments"). It focuses on item inspection, reverse scoring, scale construction, and reliability reporting using existing subskills.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks for instrument quality, reliability, or scale checks, for example:

- "Please take this dataset and figure out the quality of used instruments."
- "Check the reliability of our questionnaires."
- "Assess scale quality for these survey items."
- "Run item analysis and reliability."

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect items to infer candidate instruments, item ranges, and missingness (use `data-explorer` if helpful).
5. Ask clarifying questions (scale definitions, reverse-coded items, scoring method, reliability type).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions and completion notes.
8. Generate `report_<YYYYMMDD>_check-instruments_<intent>.md` first, align it with `nlss/references/utilities/apa7-markdown.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta check-instruments --intent "check reliability"
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Scale definitions (items per scale) or naming conventions for item groups.
- Reverse-coded items and min/max for item scales.
- Scoring method (sum vs mean).
- Reliability type: internal consistency (alpha/omega) vs test-retest/ICC/kappa.
- Optional grouping variable (condition, cohort, site).

### Clarifying questions

- Which instruments or scales are in the dataset, and which items belong to each scale?
- Do item names share prefixes that map to scales (for example PHQ9_1..PHQ9_9)?
- Which items are reverse-coded, and what are the min/max values?
- Should scale scores use sum or mean?
- Is reliability internal consistency (alpha/omega) only, or do you need ICC/kappa/test-retest?
- Is this cross-sectional, test-retest, or multi-rater data (any ID/time/rater variables)?
- Should analyses be grouped (e.g., by condition or cohort)?
- Should missingness be summarized only, or handled (listwise/impute/indicator/drop)?
If the request is vague, propose a default: infer item groups by prefix and Likert range, run internal consistency (alpha/omega), and report item diagnostics plus missingness summaries.

## Procedure (pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta check-instruments --intent <user intent>

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
write report_<YYYYMMDD>_check-instruments_<intent>.md
align report_<YYYYMMDD>_check-instruments_<intent>.md with nlss/references/utilities/apa7-markdown.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise (for example `modules.scale.score`, `modules.scale.omega`).
- Treat Likert items as numeric by default unless the user prefers categorical handling.
- Exclude identifiers (for example `id`, `uuid`) and precomputed totals (for example `*_total`, `*_sum`, `*_mean`) from item lists unless requested.
- If scale definitions are missing, infer candidate groups by shared name prefixes and similar value ranges; require at least 3 items per group and mark them as provisional in `scratchpad.md`.
- If reverse-coded items are unknown, do not reverse by default; flag negative item-total correlations for review.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.
- "Quality" here means reliability and item diagnostics; validity evidence (factor analysis, IRT) is out of scope unless the user requests it explicitly.

## Outputs

- `report_canonical.md`: APA-ready outputs from `scale`, `reliability`, optional `data-explorer`/`correlations`, plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_check-instruments_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

### Final report requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `nlss/assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; integrate tables/figures with captions and in-text references.
- Craft tables and figures specifically for the report rather than copying them from `report_canonical.md`.
- Keep the report APA 7-ready and suitable for journal submission.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_check-instruments_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report with `nlss/references/utilities/apa7-markdown.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## APA 7 Templates

This metaskill does not define its own APA template. It relies on the templates configured for the subskills it invokes:

- `scale` uses `nlss/assets/scale/default-template.md`.
- `reliability` uses `nlss/assets/reliability/default-template.md`.
- `correlations` uses `nlss/assets/correlations/default-template.md` when requested.
- `data-explorer` uses `nlss/assets/data-explorer/default-template.md` when requested.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation and `nlss/assets/metaskill-runner/finalization-template.md` for finalization logging.

## APA 7 Reporting Guidance

- Report scale reliability (alpha/omega) and item diagnostics where applicable.
- For test-retest or inter-rater analyses, report ICC/kappa with confidence intervals.
- Summarize missingness handling decisions when applied.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
