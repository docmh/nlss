---
name: test-hypotheses
description: Agent-run hypothesis testing that clarifies DV/IVs and design, selects appropriate subskills, runs assumptions checks, and produces NLSS format-ready results with scope-appropriate interpretation.
license: Apache-2.0
---

# Test Hypotheses (Agent-Run, NLSS format)

## Overview

This metaskill helps the agent test hypotheses (H1, H2, H3, ...) when the initial request is vague. The agent gathers the missing details, selects the appropriate subskills, and records analyses through the common project workflow.

## Routing Guardrails

- If the user wants **assumption checks only**, use `check-assumptions`.
- If the user wants **general screening** without hypotheses, use `screen-data` or `explore-data`.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Parse hypotheses and ask clarifying questions for each H (variables, direction, design).
5. If hypotheses or discussion need literature grounding, run the `research-academia` utility with query variants and curate sources (see utility guidance).
6. Write a plan to `scratchpad.md`, then execute subskills in order.
7. Update `scratchpad.md` with decisions, assumptions checks, and completion notes.
8. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


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
select the current project and working dataset using the common workflow

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
explain the results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align <chosen-visible-report>.md using references/metaskills/format-document.md
  preserve the delivered report and selected evidence with project-report
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Do not assume DVs/IVs or test direction when hypotheses are vague; request clarification first.
- Use config defaults for subskills unless the user specifies other options.
- If the user wants "proof" of hypotheses, clarify that statistical evidence is reported rather than proof.
- Apply p-value adjustments only when the user requests them or multiple tests are planned.

## Outputs

- `report_canonical.md`: NLSS format-ready outputs from the selected subskills (t-tests, ANOVA, regression, correlations, SEM, etc.).
- `.nlss/runs/`: saved requests/results/output from the underlying procedures; no additional lifecycle/JSONL journal.
- `scratchpad.md`: Hypothesis clarifications, plan, assumptions, and completion notes.
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

- `t-test`, `anova`, `nonparametric`, `regression`, `correlations`, `mixed-models`, `sem`, and `assumptions`.

## NLSS format Reporting Guidance

- Clearly tie each hypothesis to its test and report the corresponding effect sizes and confidence intervals.
- Document assumptions checks and any departures or alternative tests used.
- If multiple hypotheses are tested, note any p-value adjustment strategy.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
