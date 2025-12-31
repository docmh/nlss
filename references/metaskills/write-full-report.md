---
name: write-full-report
description: End-to-end metaskill that turns a dataset plus research question/hypotheses into a defensible, NLSS format-ready, journal-ready report by orchestrating screening, preparation, analysis, and literature support.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2026 Mike Hammes"
  nlss.version: "1.0.0"
---

# Write Full Report (Agent-Run, NLSS format)

## Overview

This metaskill is the highest-level orchestration for NLSS. It starts with a dataset plus a research question or hypotheses, then uses the full skill set to screen and prepare the data, select and run analyses, and synthesize a final, journal-ready report. A well grounded theoretical introduction is mandatory for this metaskill and must be supported by academia research and web search.

## Intent/Triggers

Use this metaskill when the user asks for an end-to-end analysis and report, for example:

- "Analyze this dataset and write the full report."
- "Test these hypotheses and produce a publication-ready report."
- "End-to-end analysis, tables, and narrative with citations."
- "Turn this dataset and my research question into a complete report."

## Routing Guardrails

- If the user only wants **formatting of an existing report**, use `format-document`.
- If the user only wants **hypothesis tests**, use `test-hypotheses`.
- If the user only wants **data cleaning**, use `prepare-data`.
- If the user only wants **screening/diagnostics**, use `screen-data`.
- If the user only wants **sample description**, use `describe-sample`.
- If the user only wants **literature references**, use `research-academia`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer candidate variables, IDs, and data quality risks; summarize in `scratchpad.md`.
5. Ask clarifying questions on hypotheses, variable roles, design, and measurement details.
6. Always build a well grounded theoretical introduction using `research-academia` with web search enabled and curate sources before writing the report.
7. Write a step-by-step plan to `scratchpad.md`, then execute subskills in order.
8. Update `scratchpad.md` after each step with progress, decisions, and transformations.
9. Generate `report_<YYYYMMDD>_write-full-report_<intent>.md` first, align it using `references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta write-full-report --intent "full report for research question"
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Research question or hypotheses (H1, H2, H3, ...).
- Any research notes, codebook, or pre-registered plan.
- Optional target journal or formatting constraints.

### Clarifying Questions

- What is the primary outcome (DV) and key predictors (IVs)?
- Are the hypotheses directional or non-directional?
- What is the design (between, within, mixed, correlational)?
- Are there repeated measures or clusters (subject IDs, time, site)?
- Are there covariates, interactions, or subgroup analyses?
- Which variables represent demographics for the sample description?
- Are there known missing value codes and an approved missingness strategy?
- Are there scales or instruments that require reverse scoring or composite scores?
- Are there exclusion criteria or outlier handling rules?
- What alpha level and multiple-testing adjustments should be used?

If unclear, propose defaults: report two-tailed tests, alpha = .05, no multiplicity adjustment, no overwrites, and flag (do not remove) outliers.

## Procedure (Pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta write-full-report --intent <user intent>

inspect dataset:
  data-explorer to summarize types, levels, and missingness
  write candidate variables and risks to scratchpad.md

ask clarifying questions and confirm hypotheses/design
write plan to scratchpad.md

optional:
  run research-academia for key constructs, measures, and reporting standards

if data preparation required and approved:
  run missings (or impute if requested)
  run data-transform for recodes/standardization/derived variables
  run scale for item analysis and scoring (if applicable)

describe sample:
  run descriptive-stats for continuous demographics/outcomes
  run frequencies and crosstabs for categorical demographics/grouping

screen data:
  run screen-data for outliers/normality/linearity/multicollinearity
  run assumptions for planned analyses when applicable

test hypotheses:
  select and run appropriate analysis subskills
  t-test / anova / nonparametric / regression / correlations / mixed-models / sem

generate visuals as needed:
  run plot for key figures used in the report

update scratchpad.md with decisions, assumptions checks, and completion notes
write report_<YYYYMMDD>_write-full-report_<intent>.md
align report_<YYYYMMDD>_write-full-report_<intent>.md using references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Do not assume hypotheses, DVs, IVs, or design; request confirmation.
- Use config defaults for subskills unless the user specifies otherwise.
- Do not overwrite variables or drop columns without explicit approval; prefer new variables.
- Run `missings` only after the user approves a handling strategy (it updates the parquet copy and creates a backup).
- Prefer parametric tests when assumptions are met; switch to nonparametric alternatives only after discussion.
- Use effect sizes and confidence intervals as primary evidence; do not frame results as proof.
- If the dataset is large or very wide, ask the user to prioritize domains and outcomes.
- Always enable web search for the `research-academia` step and use it to ground the Introduction with citations.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the subskills plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation/finalization entries plus the underlying subskill logs.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_write-full-report_<intent>.md`: NLSS format-ready, journal-ready narrative report with tables/figures as needed.

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `assets/metaskills/report-template.md` as the default structure; **do not omit** the Introduction section (it is mandatory for this metaskill).
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; integrate tables/figures with captions and in-text references.
- Craft tables and figures specifically for the report rather than copying them from `report_canonical.md`.
- Keep the report NLSS format-ready and suitable for journal submission.
- The Introduction must be a well grounded theoretical synthesis and must cite sources obtained via academia research and web search.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_write-full-report_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `data-explorer`, `missings`, `impute`, `data-transform`, `scale`
- `descriptive-stats`, `frequencies`, `crosstabs`
- `screen-data`, `assumptions`
- `t-test`, `anova`, `nonparametric`, `regression`, `correlations`, `mixed-models`, `sem`
- `plot` for figures
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Tie each hypothesis or research question to its analysis, effect sizes, and confidence intervals.
- Report assumption checks, any departures, and the rationale for alternative tests.
- Summarize screening and data preparation decisions (missingness, recodes, exclusions).
- Integrate relevant citations when theoretical framing or measurement justification is needed.
- State limitations and the scope of inference clearly.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
