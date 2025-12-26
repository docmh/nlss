---
name: generate-r-script
description: Generate a custom R script for analyses not covered by NLSS, with explicit permission and documentation in the workspace scripts folder.
---

# Generate R Script (Agent-run)

## Overview

This metaskill is a last resort and is used only when the requested analysis is out of scope for existing NLSS subskills. The agent must first confirm no NLSS subskill can satisfy the request, then ask for explicit permission before generating any script. Generated R scripts are documented in the dataset workspace at `<workspace-root>/<dataset-name>/scripts/`.

The agent should use state-of-the-art methods in psychological and social science research (appropriate models, effect sizes, diagnostics, and APA-aligned reporting conventions) when generating scripts.

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

### Clarifying questions

- Can the requested analysis be satisfied by an existing NLSS subskill? (Confirm with the user if unclear.)
- Which variables define outcomes, predictors, and groups?
- What assumptions or effect sizes are required?
- Which R packages are acceptable (base R, tidyverse, specialized packages)?
- Permission: “May I generate and save a custom R script in the dataset `scripts/` folder?”

## Procedure (pseudocode)

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
  include diagnostics, effect sizes, and APA-aligned outputs
  avoid destructive writes unless explicitly requested

save script to <workspace-root>/<dataset-name>/scripts/custom_<YYYYMMDD>_<intent>.R
update scratchpad.md with script path and rationale
append # Synopsis to apa_report.md and write report_<YYYYMMDD>_generate-r-script_<intent>.md
log metaskill finalization with metaskill-runner --phase finalization
```

## Default Rules and Decision Logic

- Always check NLSS subskills first; only generate a script when the request is out of scope.
- Always obtain explicit permission before generating the script.
- Use the workspace parquet copy for data access whenever possible.
- Prefer transparent, reproducible code (set seed when stochastic methods are used).
- Include assumptions checks and effect size reporting appropriate to the method.
- Record a short “out of scope” justification in `scratchpad.md` before generating the script.
- Use the naming convention `custom_<YYYYMMDD>_<intent>.R` for saved scripts (ASCII, no spaces).

## Minimum diagnostics checklist

Include the following where applicable:

- Assumption checks (normality, homoscedasticity, independence, multicollinearity).
- Effect sizes with confidence intervals.
- Influence/outlier diagnostics for regression-style models.
- Model fit or information criteria for model comparisons.
- Robustness or sensitivity checks when assumptions are violated.

## Outputs

- `analysis_log.jsonl`: Metaskill activation and finalization entries via `metaskill-runner`.
- `scratchpad.md`: Plan, clarifications, and the saved script path.
- `apa_report.md`: Includes a final `# Synopsis` describing the generated script and rationale.
- `scripts/custom_<YYYYMMDD>_<intent>.R`: The generated script in the dataset workspace folder.
- `report_<YYYYMMDD>_generate-r-script_<intent>.md`: APA 7-ready, journal-ready narrative report with ad hoc tables/plots as needed.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`).

## APA 7 Templates

This metaskill does not define its own APA template. Any APA-ready outputs should be produced by the generated script or by existing subskills if they are subsequently run.

## Parquet support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
