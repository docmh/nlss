---
name: test-hypotheses
description: Map vague hypotheses to appropriate tests through clarifications, then run subskills and report APA-ready results.
---

# Test Hypotheses (Agent-run, APA 7)

## Overview

This metaskill helps the agent test hypotheses (H1, H2, H3, ...) when the initial request is vague. The agent gathers the missing details, selects the appropriate subskills, and logs the metaskill activation before running analyses.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Parse hypotheses and ask clarifying questions for each H (variables, direction, design).
5. Write a plan to `scratchpad.md`, then execute subskills in order.
6. Update `scratchpad.md` with decisions, assumptions checks, and completion notes.
7. Log finalization, append a `# Synopsis` to `report_canonical.md`, and generate `report_<YYYYMMDD>_test-hypotheses_<intent>.md`.

## Execution (Agent-run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta test-hypotheses --intent "test H1, H2, H3"
```

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\metaskill_runner.R> --csv <path to CSV file> --meta test-hypotheses
```

## Inputs and Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Hypotheses (H1, H2, H3, ...) even if vague.
- Any pre-registered analysis plan or alpha adjustments (if applicable).

### Clarifying questions (per hypothesis)

- What is the dependent variable (DV)?
- What are the independent variables (IVs) or predictors?
- Is the hypothesis directional or non-directional?
- What is the design: between-subjects, within-subjects, mixed, or correlational?
- Are there covariates or interactions to include?
- Should the test be parametric or nonparametric?
- Is there a grouping variable (and expected group levels)?
- Are repeated measures present (subject ID and time/condition variables)?

## Procedure (pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta test-hypotheses --intent <user intent>

for each hypothesis Hi:
  if DV/IVs/design not specified:
    ask clarifying questions
  record final hypothesis statement in scratchpad.md

  select test:
    if Hi is group difference with 2 groups -> t-test
    if Hi is group difference with >2 groups -> anova or nonparametric
    if Hi is association between continuous variables -> correlations
    if Hi is prediction with multiple predictors -> regression
    if Hi is repeated measures or mixed design -> anova or mixed-models
    if Hi is mediation/SEM -> sem

  run assumptions when applicable:
    run assumptions with matching analysis type

  run the analysis subskill with confirmed variables and options

optional:
  run missings if user requests handling (not default)
  run correlations among composite scores if needed

update scratchpad.md with results summary and decisions
append # Synopsis to report_canonical.md and write report_<YYYYMMDD>_test-hypotheses_<intent>.md
log metaskill finalization with metaskill-runner --phase finalization
```

## Default Rules and Decision Logic

- Do not assume DVs/IVs or test direction when hypotheses are vague; request clarification first.
- Use config defaults for subskills unless the user specifies other options.
- If the user wants "proof" of hypotheses, clarify that statistical evidence is reported rather than proof.
- Apply p-value adjustments only when the user requests them or multiple tests are planned.

## Outputs

- `report_canonical.md`: APA-ready outputs from the selected subskills (t-tests, ANOVA, regression, correlations, SEM, etc.) plus a final `# Synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Hypothesis clarifications, plan, assumptions, and completion notes.
- `report_<YYYYMMDD>_test-hypotheses_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).

## APA 7 Templates

This metaskill does not define its own APA template. It relies on the templates configured for the subskills it invokes:

- `t-test`, `anova`, `nonparametric`, `regression`, `correlations`, `mixed-models`, `sem`, and `assumptions`.
- `metaskill-runner` uses `nlss/assets/metaskill-runner/default-template.md` for activation/finalization logging.

## APA 7 Reporting Guidance

- Clearly tie each hypothesis to its test and report the corresponding effect sizes and confidence intervals.
- Document assumptions checks and any departures or alternative tests used.
- If multiple hypotheses are tested, note any p-value adjustment strategy.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
