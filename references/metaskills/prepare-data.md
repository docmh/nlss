---
name: prepare-data
description: Agent-run data preparation that audits variables, handles missingness (missings/impute), recodes/transforms/renames, optionally builds scales, documents changes, and produces NLSS format-ready reports.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2026 Mike Hammes"
  nlss.version: "1.0.0"
---

# Prepare Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent through data preparation when the request is vague (for example, "make this data ready for analysis"). It emphasizes dataset inspection, clear decisions about missingness and coding, and transparent documentation of any transformations or exclusions.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

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
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer variable types, IDs, suspicious codes, and candidate analysis variables; summarize candidates in `scratchpad.md`.
5. Ask clarifying questions on missingness handling, recodes, exclusions, and overwrites.
6. If preprocessing choices need literature support (imputation, transformations, exclusions), run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a step-by-step plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` after each step with progress, decisions, and transformations.
9. Generate `report_<YYYYMMDD>_prepare-data_<intent>.md` first, align it using `references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta prepare-data --intent "prepare data for analysis"
```

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
if workspace missing:
  run init-workspace

run metaskill-runner --meta prepare-data --intent <user intent>

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

update scratchpad.md after each step
finalize scratchpad.md with decisions and completion summary
write report_<YYYYMMDD>_prepare-data_<intent>.md
align report_<YYYYMMDD>_prepare-data_<intent>.md using references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
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

- `report_canonical.md`: NLSS format-ready outputs from the subskills (data-explorer, missings, data-transform, optional descriptive/frequency checks) plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_prepare-data_<intent>.md`: NLSS format-ready, journal-ready narrative report with tables/figures as needed.

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; integrate tables/figures with captions and in-text references.
- Craft tables and figures specifically for the report rather than copying them from `report_canonical.md`.
- Keep the report NLSS format-ready and suitable for journal submission.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_prepare-data_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

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
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Report missingness patterns and the chosen handling strategy (listwise, impute, indicator, drop), including any thresholds.
- Document recodes, transformations, standardizations, and variable creation (with justification).
- Note any exclusions or outlier handling and whether cases were removed or only flagged.
- Summarize the resulting dataset readiness for the intended analysis (variables retained, scales computed, coding decisions).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
