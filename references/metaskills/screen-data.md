---
name: screen-data
description: Agent-run data screening for outliers, normality, linearity, homoscedasticity, and multicollinearity using NLSS subskills with recommendations and NLSS format-ready reporting.
---

# Screen Data (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to screen a dataset before analysis. It focuses on distributional checks, outliers, linearity, homoscedasticity, and multicollinearity using existing NLSS subskills. By default it **flags** issues and recommends actions rather than modifying data; any transformations or case removals require explicit approval.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

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
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer variable types, ID columns, candidate outcomes/predictors, and grouping variables.
5. Ask clarifying questions for scope and screening thresholds.
6. If the user requests citations for screening criteria, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. Generate `report_<YYYYMMDD>_screen-data_<intent>.md` first, align it using `references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta screen-data --intent "screen the data"
```

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
if workspace missing:
  run init-workspace

run metaskill-runner --meta screen-data --intent <user intent>

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
write report_<YYYYMMDD>_screen-data_<intent>.md
align report using references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>"
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

- `report_canonical.md`: NLSS format-ready outputs from `data-explorer`, `descriptive-stats`, optional `plot`/`correlations`/`assumptions`, plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_screen-data_<intent>.md`: NLSS format-ready, journal-ready narrative report with tables/figures as needed.

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

- Write `report_<YYYYMMDD>_screen-data_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `data-explorer` uses `assets/data-explorer/default-template.md`.
- `descriptive-stats` uses `assets/descriptive-stats/distribution-template.md` (default for screening).
- `plot` uses `assets/plot/default-template.md` (if visuals are requested).
- `correlations` uses `assets/correlations/default-template.md` (if used).
- `assumptions` uses `assets/assumptions/*-template.md` depending on analysis type.
- `missings` uses `assets/missings/default-template.md` (only if handling is approved).
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Report sample size (*N*), missingness ranges, and variable types for screened variables.
- Summarize distributional shape, skew/kurtosis, and outlier counts for numeric variables.
- For linearity and homoscedasticity, note visual patterns and any diagnostic test results.
- If model-specific assumptions were checked, report the tests used and any violations with recommended remedies (transformations, robust tests, or alternative models).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
