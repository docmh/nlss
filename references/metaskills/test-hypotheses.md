---
name: test-hypotheses
description: Agent-run hypothesis testing that clarifies DV/IVs and design, selects appropriate subskills, runs assumptions checks, and produces NLSS format-ready results with a final report.
license: Apache-2.0
---

# Test Hypotheses (Agent-Run, NLSS format)

## Overview

This metaskill helps the agent test hypotheses (H1, H2, H3, ...) when the initial request is vague. The agent gathers the missing details, selects the appropriate subskills, and logs the metaskill activation before running analyses.

## Routing Guardrails

- If the user wants **assumption checks only**, use `check-assumptions`.
- If the user wants **general screening** without hypotheses, use `screen-data` or `explore-data`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Parse hypotheses and ask clarifying questions for each H (variables, direction, design).
5. If hypotheses or discussion need literature grounding, run the `research-academia` utility with query variants and curate sources (see utility guidance).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions, assumptions checks, and completion notes.
8. Generate `report_<YYYYMMDD>_test-hypotheses_<intent>.md` first, align it using `references/metaskills/format-document.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta test-hypotheses --intent "test H1, H2, H3"
```

## Inputs and Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset.
- Hypotheses (H1, H2, H3, ...) even if vague.
- Any pre-registered analysis plan or alpha adjustments (if applicable).

### Clarifying Questions (per Hypothesis)

- What is the dependent variable (DV)?
- What are the independent variables (IVs) or predictors?
- Is the hypothesis directional or non-directional?
- What is the design: between-subjects, within-subjects, mixed, or correlational?
- Are there covariates or interactions to include?
- Should the test be parametric or nonparametric?
- Is there a grouping variable (and expected group levels)?
- Are repeated measures present (subject ID and time/condition variables)?

## Procedure (Pseudocode)

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta test-hypotheses --intent <user intent>

for each hypothesis Hi:
  if DV/IVs/design not specified:
    ask clarifying questions
  record final hypothesis statement in scratchpad.md

  optional:
    if hypothesis framing or interpretation needs citations:
      run research-academia (multiple query variants; curate sources)

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
write report_<YYYYMMDD>_test-hypotheses_<intent>.md
align report_<YYYYMMDD>_test-hypotheses_<intent>.md using references/metaskills/format-document.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Do not assume DVs/IVs or test direction when hypotheses are vague; request clarification first.
- Use config defaults for subskills unless the user specifies other options.
- If the user wants "proof" of hypotheses, clarify that statistical evidence is reported rather than proof.
- Apply p-value adjustments only when the user requests them or multiple tests are planned.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the selected subskills (t-tests, ANOVA, regression, correlations, SEM, etc.) plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries from `metaskill-runner`, plus the underlying subskill logs.
- `scratchpad.md`: Hypothesis clarifications, plan, assumptions, and completion notes.
- `report_<YYYYMMDD>_test-hypotheses_<intent>.md`: NLSS format-ready, journal-alike narrative report with ad hoc tables/plots as needed.

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

- Write `report_<YYYYMMDD>_test-hypotheses_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/format-document.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `t-test`, `anova`, `nonparametric`, `regression`, `correlations`, `mixed-models`, `sem`, and `assumptions`.
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Clearly tie each hypothesis to its test and report the corresponding effect sizes and confidence intervals.
- Document assumptions checks and any departures or alternative tests used.
- If multiple hypotheses are tested, note any p-value adjustment strategy.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
