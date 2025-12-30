---
name: generate-r-script
description: Last-resort, permissioned custom R script generation for analyses outside NLSS, saved to workspace scripts/ with documented rationale and NLSS format-aligned outputs.
---

# Generate R Script (Agent-Run)

## Overview

This metaskill is a last resort and is used only when the requested analysis is out of scope for existing NLSS subskills. The agent must first confirm no NLSS subskill can satisfy the request, then ask for explicit permission before generating any script. Generated R scripts are documented in the dataset workspace at `<workspace-root>/<dataset-name>/scripts/`.

The agent should use state-of-the-art methods in psychological and social science research (appropriate models, effect sizes, diagnostics, and NLSS format-aligned reporting conventions) when generating scripts.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when:

- The user asks for an analysis not covered by existing NLSS subskills.
- The analysis requires bespoke modeling, specialized packages, or nonstandard outputs.

Do not use this metaskill when an NLSS subskill already provides the requested analysis.

## Inputs and Clarifications

### Inputs

- Data source (CSV/SAV/RDS/RData/Parquet or workspace dataset).
- Analysis goal and research question.
- Variable roles (DV/IVs/covariates/grouping/repeated measures).
- Desired outputs (tables, plots, model diagnostics).

### Clarifying Questions

- Can the requested analysis be satisfied by an existing NLSS subskill? (Confirm with the user if unclear.)
- Which variables define outcomes, predictors, and groups?
- What assumptions or effect sizes are required?
- Which R packages are acceptable (base *r*, tidyverse, specialized packages)?
- Permission: “May I generate and save a custom R script in the dataset `scripts/` folder?”

## Procedure (Pseudocode)

```
inspect user request
if NLSS subskill covers request:
  explain which subskill(s) to use
  stop (do not generate a script)

ask permission to generate and save an R script
if permission not granted:
  stop and request direction

ensure workspace exists (init-workspace if missing)
log activation with metaskill-runner --meta generate-r-script --intent <user intent>

inspect dataset and confirm variable roles
write plan + decisions to scratchpad.md
record why the request is out of scope for NLSS
record NLSS subskills considered and why they were insufficient

generate R script using state-of-the-art methods:
  include required packages + version notes
  include clear data loading from workspace parquet copy
  include diagnostics, effect sizes, and NLSS format-aligned outputs
  avoid destructive writes unless explicitly requested

save script to <workspace-root>/<dataset-name>/scripts/custom_<YYYYMMDD>_<intent>.R
update scratchpad.md with script path and rationale
write report_<YYYYMMDD>_generate-r-script_<intent>.md
align report_<YYYYMMDD>_generate-r-script_<intent>.md using nlss/references/metaskills/formatting/align-report.md
run metaskill-runner --phase finalization --synopsis "<synopsis text>" (the runner fails if the report is missing; synopsis is appended to report_canonical.md)
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Always check NLSS subskills first; only generate a script when the request is out of scope.
- Always obtain explicit permission before generating the script.
- Use the workspace parquet copy for data access whenever possible.
- Prefer transparent, reproducible code (set seed when stochastic methods are used).
- Include assumptions checks and effect size reporting appropriate to the method.
- Record a short “out of scope” justification in `scratchpad.md` before generating the script.
- Use the naming convention `custom_<YYYYMMDD>_<intent>.R` for saved scripts (ASCII, no spaces).

## Minimum Diagnostics Checklist

Include the following where applicable:

- Assumption checks (normality, homoscedasticity, independence, multicollinearity).
- Effect sizes with confidence intervals.
- Influence/outlier diagnostics for regression-style models.
- Model fit or information criteria for model comparisons.
- Robustness or sensitivity checks when assumptions are violated.

## Outputs

- `analysis_log.jsonl`: Metaskill activation and finalization entries via `metaskill-runner`.
- `scratchpad.md`: Plan, clarifications, and the saved script path.
- `report_canonical.md`: Includes a final `# Synopsis` describing the generated script and rationale (via `metaskill-runner --synopsis`).
- `scripts/custom_<YYYYMMDD>_<intent>.R`: The generated script in the dataset workspace folder.
- `report_<YYYYMMDD>_generate-r-script_<intent>.md`: NLSS format-ready, journal-ready narrative report with ad hoc tables/plots as needed.

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `nlss/assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; integrate tables/figures with captions and in-text references.
- Craft tables and figures specifically for the report rather than copying them from `report_canonical.md`.
- Keep the report NLSS format-ready and suitable for journal submission.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).
All artifacts (reports, tables, figures, scripts) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_generate-r-script_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `nlss/references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. Any NLSS format-ready outputs should be produced by the generated script or by existing subskills if they are subsequently run.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).