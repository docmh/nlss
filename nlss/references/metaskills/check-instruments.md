---
name: check-instruments
description: Check survey instruments by inspecting items, handling reverse scoring, and reporting reliability via scale and reliability subskills.
---

# Check Instruments (Agent-run, APA 7)

## Overview

This metaskill guides the agent through instrument checks for surveys or questionnaires. It focuses on item inspection, reverse scoring, scale construction, and reliability reporting using existing subskills.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect items to confirm scales, item ranges, and missingness.
5. Ask clarifying questions (scale definitions, reverse-coded items, scoring method, reliability type).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions and completion notes.
8. Log finalization, append a `# Synopsis` to `report_canonical.md`, and generate `report_<YYYYMMDD>_check-instruments_<intent>.md`.

## Execution (Agent-run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta check-instruments --intent "check reliability"
```

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\metaskill_runner.R> --csv <path to CSV file> --meta check-instruments
```

## Inputs and Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Scale definitions (items per scale).
- Reverse-coded items and min/max for item scales.
- Scoring method (sum vs mean).
- Reliability type: internal consistency (alpha/omega) vs test-retest/ICC/kappa.

### Clarifying questions

- Which items belong to each scale?
- Which items are reverse-coded, and what are the min/max values?
- Should scale scores use sum or mean?
- Is reliability internal consistency (alpha/omega) only, or do you need ICC/kappa/test-retest?
- Should analyses be grouped (e.g., by condition or cohort)?
- Should missingness be summarized only, or handled (listwise/impute/indicator/drop)?

## Procedure (pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta check-instruments --intent <user intent>

inspect dataset:
  confirm item ranges and missingness
  flag candidate scales/items from user list

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
  run correlations --vars <scale_scores> [--group <group_var>]
  run missings --vars <items> --method <chosen> (only if user requests handling)

update scratchpad.md with decisions and completion
append # Synopsis to report_canonical.md and write report_<YYYYMMDD>_check-instruments_<intent>.md
log metaskill finalization with metaskill-runner --phase finalization
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise (for example `modules.scale.score`, `modules.scale.omega`).
- Treat Likert items as numeric by default unless the user prefers categorical handling.
- Exclude identifiers (for example `id`, `uuid`) from scale analyses unless explicitly requested.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.

## Outputs

- `report_canonical.md`: APA-ready outputs from `scale`, `reliability`, and optional `correlations`, plus a final `# Synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_check-instruments_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).

## APA 7 Templates

This metaskill does not define its own APA template. It relies on the templates configured for the subskills it invokes:

- `scale` uses `nlss/assets/scale/default-template.md`.
- `reliability` uses `nlss/assets/reliability/default-template.md`.
- `correlations` uses `nlss/assets/correlations/default-template.md` when requested.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation/finalization logging.

## APA 7 Reporting Guidance

- Report scale reliability (alpha/omega) and item diagnostics where applicable.
- For test-retest or inter-rater analyses, report ICC/kappa with confidence intervals.
- Summarize missingness handling decisions when applied.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
