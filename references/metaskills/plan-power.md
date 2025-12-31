---
name: plan-power
description: Agent-run a priori power planning that clarifies design, effect size, alpha, and power (or estimates effect from pilot data) then runs the power subskill and reports NLSS format-ready results.
license: Apache-2.0
---

# Plan Power (Agent-Run, NLSS format)

## Overview

This metaskill guides the agent to turn a vague research design into a concrete, a priori power analysis. It focuses on clarifying the planned test family, effect size assumptions, alpha, and target power, then runs the `power` subskill to produce NLSS format-ready outputs.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks for minimal sample size or power planning, for example:

- "How many participants do I need?"
- "Minimum sample size for my study."
- "Run a priori power analysis for this design."
- "We have a vague plan; estimate sample size."
- "Power analysis for t-test/ANOVA/regression/correlation/SEM."

## Routing Guardrails

- If the request is about **diagnostics or assumptions**, use `check-assumptions` or `screen-data`.
- If the request is about **testing hypotheses**, use `test-hypotheses`.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, SAV, Parquet, or workspace context).
2. Ensure a dataset workspace exists (run `init-workspace` if missing).
3. Log activation with `metaskill-runner`.
4. Inspect the dataset (if available) and summarize candidate DV/IVs and group variables in `scratchpad.md`.
5. Ask clarifying questions to finalize analysis family, effect size assumptions, alpha, power, and design details.
6. If effect size justification needs literature support, run the `research-academia` utility with query variants and curate sources (see utility guidance).
7. Write a plan to `scratchpad.md`, then execute the `power` subskill (optionally estimating effect size from pilot data).
8. Update `scratchpad.md` with decisions and completion notes.
9. Generate `report_<YYYYMMDD>_plan-power_<intent>.md` first, align it using `references/metaskills/formatting/align-report.md`, then run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

## Execution (Agent-Run)

There is no dedicated script for this metaskill. The agent runs subskills and logs activation/finalization using `metaskill-runner`.

### Logging Activation

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta plan-power --intent "plan sample size"
```

## Inputs/Clarifications

### Inputs

- Data sources: CSV, SAV, RDS, RData, Parquet, or workspace dataset (pilot data optional).
- Vague or concrete research design and primary hypothesis.
- Planned analysis family (ttest, anova, correlation, regression, sem).
- Effect size information (from literature, pilot data, or minimum detectable effect).
- Alpha, power target, tails (one- vs two-sided), and group allocation ratio.

### Clarifying Questions

- What is the primary outcome and primary hypothesis?
- Which analysis family will you use (t-test, ANOVA, correlation, regression, SEM)?
- Is the design between-subjects, within-subjects, or paired? Any grouping variable and expected group sizes?
- What effect size metric and value will you assume (*d*, f, *r*, *f*², eta2, r2, RMSEA)?
- What alpha and target power should we use? One- or two-sided?
- Are there multiple primary outcomes or multiple primary tests (alpha adjustment needed)?
- Do you have pilot data so we can estimate effect size from the dataset?

If unclear, propose defaults: alpha = 0.05, power = 0.80, two-sided, equal group sizes, and small/medium/large effect size sensitivity.

## Procedure

```
if workspace missing:
  run init-workspace

run metaskill-runner --meta plan-power --intent <user intent>

if dataset available:
  inspect dataset and summarize candidate DV/IVs and group variables in scratchpad.md

ask clarifying questions to confirm:
  analysis family, design details, effect size metric/value, alpha, power, tails, group ratio
  whether to estimate effect size from pilot data

write plan to scratchpad.md

optional:
  if effect size needs literature grounding or user requests citations:
    run research-academia (multiple query variants; curate sources)

if pilot data and estimation approved:
  run power --analysis <analysis> --mode apriori --estimate-effect TRUE <vars/ivs/group>
else:
  run power --analysis <analysis> --mode apriori --effect-metric <metric> --effect-size <value> \
            --alpha <alpha> --power <power> [--t-type <type>] [--groups <k>] [--u <predictors>] \
            [--ratio <n2/n1>] [--df <df> --rmsea0 <val> --rmsea1 <val>]

optional:
  run additional power calls for small/medium/large effects or sensitivity analysis when effect size is uncertain

update scratchpad.md with decisions and completion
write report_<YYYYMMDD>_plan-power_<intent>.md
align report_<YYYYMMDD>_plan-power_<intent>.md using references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use a priori mode to compute minimal sample size; use sensitivity mode only when sample size is fixed.
- Use config defaults for alpha and power unless specified.
- If effect size is not provided and no pilot data exists, offer Cohen-style small/medium/large sensitivity runs and document the choice.
- Assume equal group sizes (ratio = 1) unless the design specifies otherwise.
- SEM power requires `--df`, `--rmsea0`, and `--rmsea1`; do not use `--estimate-effect` for SEM.
- If the planned analysis is outside supported families (e.g., logistic regression, survival, multilevel power), ask for permission before using `generate-r-script`.

## Outputs

- `report_canonical.md`: NLSS format-ready power analysis table and narrative from the `power` subskill plus a final `# Synopsis` recorded via `metaskill-runner --synopsis`.
- `analysis_log.jsonl`: Metaskill activation and finalization entries plus the `power` subskill log(s).
- `scratchpad.md`: Plan, clarifications, effect size rationale, and completion notes.
- `report_<YYYYMMDD>_plan-power_<intent>.md`: NLSS format-ready, journal-ready narrative report with tables/plots as needed.

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

- Write `report_<YYYYMMDD>_plan-power_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `power` uses `assets/power/default-template.md`.
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Report analysis family, effect size metric/value, alpha, target power, and resulting sample size.
- Note the effect size source (literature vs pilot data vs minimum detectable effect).
- If sensitivity analyses are run, summarize the range of required sample sizes across effect sizes.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
