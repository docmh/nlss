---
name: check-assumptions
description: Agent-run model-specific diagnostics for t-tests, ANOVA, regression, mixed models or SEM, followed by contextual interpretation; no substantive hypothesis analysis or data changes.
license: Apache-2.0
---

# Check Assumptions (Agent-Run, NLSS format)

## Overview

This metaskill runs **model-specific** assumptions and diagnostics when the user requests checks for a specified analysis family. Diagnostic tests and the model fits/refits needed to calculate them are in scope; substantive hypothesis testing and data changes are not. Its semantic interpretation is grounded in the recorded diagnostic evidence and research design, not a count of template passes.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

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

For interpretation of an explicitly selected completed run, first inspect that
run's request/result/output and integrity evidence; do not execute new diagnostics
or initialize another dataset automatically. If only a conversational explanation
is requested, answer directly without metaskill activation/finalization. For a
requested formal discussion, follow the report-delivery steps below while
referencing the existing evidence. Describe the historical model honestly; do
not certify a different model or case selection. Ask for the matching run when
the provided evidence and the request conflict. A new analysis/replay requires
the user's request to include execution, not merely interpretation.

For a new diagnostic execution:

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Use the selected current project and working data; creation is explicit, never implicit.
3. Follow the common project/report workflow in `SKILL.md`; no separate lifecycle activation.
4. Inspect the dataset to infer candidate outcomes, predictors, and grouping variables.
5. Ask clarifying questions to pin down the analysis family and variable roles.
6. If the user requests citations for diagnostic criteria or thresholds, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then run `assumptions` with the appropriate flags.
   Read its [family-specific scope and run contract](../subskills/assumptions.md),
   then inspect `result.json` and `output.md`: distinguish check availability,
   diagnostic flags and actual model/case selection. A completed run is not
   proof that the planned analysis satisfies every assumption.
8. Update `scratchpad.md` with decisions and completion notes.
9. When a report is requested, write it at a chosen visible Markdown path and preserve it with its actual evidence through `project-report`.

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs the existing subskills through the common project route.


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
- Repeated measures → clarify repeated-measures ANOVA versus a specified mixed model; do not switch an explicitly requested ANOVA automatically.
- Nested IDs with a specified random-effects model → `mixed_models`
- Latent variables / path model → `sem`

## Procedure (Pseudocode)

```
if interpreting an explicitly selected completed result:
  inspect saved request, results, output and integrity evidence
  check the actual model/design/case identities against the question
  if evidence mismatches the question: ask for the matching run, do not refit
  if conversational: explain and stop
  if formal discussion requested: write the contextual report and preserve its evidence with project-report
  stop

select the current project and working dataset using the common workflow

inspect dataset:
  identify candidate DVs, IVs, group variables, ID/time columns

ask user to confirm:
  analysis family, variable roles, design details
  optional diagnostics/plots

write plan to scratchpad.md

optional:
  if diagnostic thresholds need literature grounding:
    run research-academia (multiple query variants; curate sources)

run assumptions --analysis <ttest|anova|regression|mixed_models|sem> with required flags

if plots requested:
  run plot for QQ/residuals/scatter as appropriate

update scratchpad.md with decisions and completion
explain the results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align report using references/metaskills/format-document.md
  preserve the delivered report and selected evidence with project-report
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use config defaults for assumptions unless the user specifies thresholds.
- Run only diagnostic tests and necessary model refits, not the substantive hypothesis analysis.
- Match diagnostics to the actual model, estimator and cases. If a requested design is unsupported (for example standalone ANCOVA diagnostics), disclose the mismatch and propose a supported diagnostic route; never silently drop covariates or change the model.
- Interpret small-sample limits, multiplicity, effect/design relevance and visual evidence. Nonsignificant tests do not prove assumptions; unavailable/skipped checks never count as passes. Recommendations require reasoning, and changes to data or the planned analysis require user agreement.
- Do not modify data (no `missings` or `data-transform`).
- If the analysis family cannot be determined, request clarification before running any checks.
- If the user asks for general screening only, route to `screen-data`.

## Outputs

- `report_canonical.md`: NLSS format-ready assumptions output.
- `.nlss/runs/`: saved requests/results/output from the underlying procedures; no additional lifecycle/JSONL journal.
- Each assumptions run preserves its unrounded results, deterministic output and frozen templates. Utility lifecycle events are not statistical replay.
- `scratchpad.md`: Plan, clarifications, and completion notes.
- `<chosen-visible-report>.md`: Only when requested; a freely authored diagnostic report.

### Final Report Requirements

For a requested report, follow the shared [semantic synthesis and presentation
guidance](../../SKILL.md#semantic-answers-and-authored-reports). The manuscript
scaffold is optional; use the structure and depth appropriate to the question.

- Synthesize diagnostics with the research question, sampling/design, model and estimator. Reference the actual run; separate findings, unavailable evidence, limitations and justified options. Diagnostic p-values are permitted, but do not add substantive hypothesis tests or imply automatic model approval/rejection.

Use the common project destinations in `SKILL.md`: automatic root protocol,
run-local evidence, visible working data and freely chosen authored-report paths.

## Report delivery

Use [project-report](../utilities/project-report.md) as part of requested delivery.
The agent selects the actual evidence; the researcher does not manage internal
IDs or perform separate finalization. No required filename, synopsis append or
additional utility event. Preserve semantic interpretation beyond output templates.

## NLSS format Templates

This metaskill relies on the templates configured for the subskills it invokes:

- `assumptions` uses `assets/assumptions/*-template.md` based on analysis family.
- `plot` uses `assets/plot/default-template.md` when diagnostics are plotted.

## NLSS format Reporting Guidance

- Report each diagnostic test, statistic, and p-value.
- Highlight violations and propose corrective options (transformations, robust methods, nonparametric alternatives).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
