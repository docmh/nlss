---
name: check-assumptions
description: Agent-run, model-specific diagnostics (assumptions only) for t-tests, ANOVA, regression, mixed models, or SEM using the assumptions subskill; no inferential tests.
---

# Check Assumptions (Agent-Run, NLSS format)

## Overview

This metaskill runs **model-specific** assumptions and diagnostics without executing the inferential test itself. It is used when the user explicitly wants assumption checks (normality, homogeneity, linearity, multicollinearity, influence, etc.) for a specified analysis family. It does **not** modify data or run hypothesis tests.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks for assumption checks or diagnostics **for a specific analysis family**, for example:

- "Check assumptions for my regression model."
- "Run ANOVA assumptions on these variables."
- "Are the t-test assumptions met?"
- "Diagnostics for SEM before running the model."

## Non-Goals (Routing Guardrails)

- If the user asks to **test hypotheses** or run the actual model, use `test-hypotheses`.
- If the user wants **general screening** without a model (outliers/normality overview), use `screen-data`.
- If the user requests **data changes** (imputation, recodes, drops), use `prepare-data`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset to infer candidate outcomes, predictors, and grouping variables.
5. Ask clarifying questions to pin down the analysis family and variable roles.
6. Write a plan to `scratchpad.md`, then run `assumptions` with the appropriate flags.
7. Update `scratchpad.md` with decisions and completion notes.
8. Generate `report_<YYYYMMDD>_check-assumptions_<intent>.md` first, align it using `nlss/references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta check-assumptions --intent "check model assumptions"
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Planned analysis family: `ttest`, `anova`, `regression`, `mixed_models`, or `sem`.
- Variable roles: DV/IVs or group variables; repeated measures; model formula if mixed models.

### Clarifying Questions

- Which analysis family applies (t-test, ANOVA, regression, mixed models, SEM)?
- What are the DV/IVs or grouping variables?
- Is the design between-subjects, within-subjects, or mixed?
- Are there covariates or interactions?
- Should ordinal/Likert variables be treated as numeric or categorical?
- Do you want only tests, or also diagnostic plots (QQ, residuals, scatter)?

If the request is vague but still informative, propose a minimal menu:
- Two groups + numeric DV → `ttest`
- 3+ groups + numeric DV → `anova`
- Numeric DV + multiple numeric predictors → `regression`
- Repeated measures / nested IDs → `mixed_models`
- Latent variables / path model → `sem`

## Procedure (Pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta check-assumptions --intent <user intent>

inspect dataset:
  identify candidate DVs, IVs, group variables, ID/time columns

ask user to confirm:
  analysis family, variable roles, design details
  optional diagnostics/plots

write plan to scratchpad.md

run assumptions --analysis <ttest|anova|regression|mixed_models|sem> with required flags

if plots requested:
  run plot for QQ/residuals/scatter as appropriate

update scratchpad.md with decisions and completion
write report_<YYYYMMDD>_check-assumptions_<intent>.md
align report using nlss/references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>"
```

## Default Rules and Decision Logic

- Use config defaults for assumptions unless the user specifies thresholds.
- Do not run inferential tests; this metaskill is diagnostics only.
- Do not modify data (no `missings` or `data-transform`).
- If the analysis family cannot be determined, request clarification before running any checks.
- If the user asks for general screening only, route to `screen-data`.

## Outputs

- `report_canonical.md`: NLSS format-ready assumptions output plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries plus the `assumptions` subskill log.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `report_<YYYYMMDD>_check-assumptions_<intent>.md`: NLSS format-ready, journal-ready narrative report.

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `nlss/assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Synthesize diagnostics with recommendations; do not perform hypothesis testing.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_check-assumptions_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `nlss/references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill relies on the templates configured for the subskills it invokes:

- `assumptions` uses `nlss/assets/assumptions/*-template.md` based on analysis family.
- `plot` uses `nlss/assets/plot/default-template.md` when diagnostics are plotted.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` and `finalization-template.md`.

## NLSS format Reporting Guidance

- Report each diagnostic test, statistic, and *p*-value.
- Highlight violations and propose corrective options (transformations, robust methods, nonparametric alternatives).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
