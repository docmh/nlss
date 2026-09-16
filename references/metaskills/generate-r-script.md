---
name: generate-r-script
description: Last-resort, permissioned custom R script generation for analyses outside NLSS, saved to workspace scripts/ with documented rationale and NLSS format-aligned outputs.
license: Apache-2.0
---

# Generate R Script (Agent-Run)

## Overview

This metaskill is a last resort and is used only when the requested analysis is out of scope for existing NLSS subskills. The agent must first confirm no NLSS subskill can satisfy the request, then ask for explicit permission before generating any script. Save generated R scripts at an agreed visible project path (for example `scripts/`) and document that path in the scratchpad.

The agent should use state-of-the-art methods in psychological and social science research (appropriate models, effect sizes, diagnostics, and NLSS format-aligned reporting conventions) when generating scripts.

## Assistant Researcher Model

Follow the shared [researcher interaction and reporting guidance](../../SKILL.md#semantic-answers-and-authored-reports).
Match the requested scope; use the analysis workflow below when analysis is
requested, and write a formal report only when requested. Preserve the scientific
decisions and permissions described here.

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
- Which R packages are acceptable (base r, tidyverse, specialized packages)?
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

select the current project; do not implicitly initialize or adopt a folder

inspect dataset and confirm variable roles
write plan + decisions to scratchpad.md
record why the request is out of scope for NLSS
record NLSS subskills considered and why they were insufficient

generate R script using state-of-the-art methods:
  include required packages + version notes
  include clear data loading from workspace parquet copy
  include diagnostics, effect sizes, and NLSS format-aligned outputs
  avoid destructive writes unless explicitly requested

save script at the chosen visible project path
update scratchpad.md with script path and rationale
explain the generated script and any actual results in the conversation
if a report is requested:
  write <chosen-visible-report>.md
  align <chosen-visible-report>.md using references/metaskills/format-document.md
  preserve the requested report with its actual supported evidence through project-report
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

- `scratchpad.md`: Plan, clarifications, and the saved script path.
- Root `report_canonical.md`: automatic output from existing NLSS procedures used; custom scripts do not acquire a saved-run contract merely by being generated.
- `scripts/custom_<YYYYMMDD>_<intent>.R`: The generated script at the chosen visible project path.
- `<chosen-visible-report>.md`: Only when requested; a freely authored report with useful tables/figures.

### Final Report Requirements

For a requested report, follow the shared [semantic synthesis and presentation
guidance](../../SKILL.md#semantic-answers-and-authored-reports). The manuscript
scaffold is optional; use the structure and depth appropriate to the question.

Use the common project locations from `SKILL.md`; keep generated scripts and
requested authored documents at chosen visible project paths.

## Report delivery

Follow [project-report](../utilities/project-report.md) for requested delivery
with actual supported evidence. No mandatory report filename, synopsis append or
lifecycle event. Do not invent replay/audit guarantees for a custom script.

## NLSS format Templates

This metaskill does not define its own NLSS format template. Any NLSS format-ready outputs should be produced by the generated script or by existing subskills if they are subsequently run.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
