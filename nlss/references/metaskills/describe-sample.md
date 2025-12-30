---
name: describe-sample
description: Agent-run sample description that inspects data, infers demographics, runs descriptive-stats/frequencies (plus optional crosstabs/missings), and writes an NLSS format-ready report.
---

# Describe Sample (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to describe a sample by inspecting the dataset, clarifying key variables, and running the appropriate subskills. It is designed to handle vague requests like "describe the demographics" by inferring likely demographic variables, confirming assumptions, and producing NLSS format-ready outputs via the underlying subskills.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

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
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer numeric vs categorical variables and candidate grouping variables.
5. Ask clarifying questions when needed (grouping variable, key demographics, Likert handling), and explicitly propose a demographic-first summary as the default.
6. If the user requests literature context for the sample (norms, benchmarks, or population comparisons), run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` with decisions and completion notes.
9. Generate `report_<YYYYMMDD>_describe-sample_<intent>.md` first, align it using `nlss/references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta describe-sample --intent "describe the sample"
```

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
if workspace missing:
  run init-workspace

run metaskill-runner --meta describe-sample --intent <user intent>

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
write report_<YYYYMMDD>_describe-sample_<intent>.md
align report_<YYYYMMDD>_describe-sample_<intent>.md using nlss/references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Use config defaults for subskills unless the user specifies otherwise (e.g., `defaults.digits`, `modules.descriptive_stats.vars_default`, `modules.frequencies.vars_default`).
- Treat factor/character variables as categorical; treat numeric variables with low cardinality (for example <= 10 unique values) as categorical unless the user prefers numeric summaries.
- Exclude obvious identifiers (for example `id`, `uuid`, `timestamp`) and derived outcomes (`*_score`, `*_total`, `*_sum`, `*_mean`) from demographic summaries unless explicitly requested.
- If demographics are not specified, infer them by name patterns and value ranges, then confirm with the user; prioritize age, gender/sex, education, employment, income, marital status, ethnicity/race, country, and language.
- If ordinal demographics (education/income) are ambiguous, default to categorical reporting.
- When the request is ambiguous, default to a demographic-first summary and state this as the proposed focus.
- Do not run `missings` unless the user requests missingness handling; it updates the workspace parquet copy in place and creates a backup.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the subskills (descriptive stats, frequencies, and optional crosstabs) plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_describe-sample_<intent>.md`: NLSS format-ready, journal-ready narrative report with ad hoc tables/plots as needed.

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

- Write `report_<YYYYMMDD>_describe-sample_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `nlss/references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `descriptive-stats` uses `nlss/assets/descriptive-stats/default-template.md`.
- `frequencies` uses `nlss/assets/frequencies/default-template.md` (or grouped template when `--group` is used).
- `crosstabs` uses `nlss/assets/crosstabs/default-template.md` (or grouped template when `--group` is used).
- `data-explorer` uses `nlss/assets/data-explorer/default-template.md` when requested.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation and `nlss/assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Report sample size (*N*) and key demographics succinctly.
- Use descriptive statistics for continuous variables and frequency tables for categorical variables.
- If grouping is used, report group-wise summaries and note any differences descriptively.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
