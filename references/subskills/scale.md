---
name: scale
description: Scale item analysis with item stats, item-total and alpha-if-deleted, alpha/omega reliability, reverse scoring, scale scores, missing handling, grouping, and NLSS format outputs.
license: Apache-2.0
---

# Scale Analysis (Base R, NLSS format)

## Overview

Compute item-level statistics and scale reliability metrics for psychometric scales. Outputs include an NLSS format-ready table with item analysis and a narrative summary of internal consistency.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Choose item variables and an optional grouping variable.
3. (Optional) reverse-score items using `--reverse` plus `--reverse-min`/`--reverse-max`.
4. Run the `scale` operation through `run_nlss.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `result.json`) to craft the response.

## Execution: `scale`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

Run with `Rscript`. Statistical calculations use base R and `stats`; workspace
imports and audit output also require the shared NLSS dependencies (`arrow`,
`yaml`, `jsonlite`, and `haven` for SAV). Labels remain descriptive metadata,
not an instruction to replace numeric item codes with factor integers.

### CSV Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --csv <path to CSV file> --vars item1,item2,item3
```

### Grouped Scale Analysis

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --csv <path to CSV file> --vars item1,item2,item3 --group condition
```

### Reverse Scoring

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --csv <path to CSV file> --vars item1,item2,item3 --reverse item2,item3 --reverse-min 1 --reverse-max 5
```

### RDS Input (Data Frame)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --rds <path to RDS file> --vars item1,item2,item3
```

### RData Input (Data Frame by Name)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --rdata <path to RData file> --df <data frame name> --vars item1,item2,item3
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --parquet <path to parquet file> --vars item1,item2,item3
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" scale --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.scale.vars_default` (typically numeric columns) if omitted; use `--vars` for item selection.
- `--group` is optional and produces grouped scale analyses.
- `--reverse` lists unique selected items to reverse-score. Supply both
  `--reverse-min` and `--reverse-max` (defaults from `modules.scale.reverse_min`
  / `modules.scale.reverse_max`) or neither for observed bounds. Explicit bounds
  must be finite, increasing, and include every observed value of the selected
  reverse items. Unknown items, duplicate items, partial bounds and invalid text
  are errors. Observed bounds are item-specific across the entire dataset,
  **before** group splitting; they need not equal the instrument's possible
  response range. Prefer instrument-defined bounds when known.
- `--missing` selects `pairwise` or `complete` (default: `modules.scale.missing`);
  aliases `pair` and `listwise` remain supported. This selects observations for
  covariance/correlation, alpha and item-total statistics. Item descriptives
  always use each item's available values.
- `--score` selects `sum` or `mean` (aliases `total`, `average`, `avg`; default:
  `modules.scale.score`). Score summaries always use joint complete cases, even
  with pairwise reliability. No partial-item prorating is performed, and no
  score column is written into the working dataset.
- `--omega` toggles standardized omega total from a one-factor maximum-likelihood
  model fitted to the selected correlation matrix (default: `modules.scale.omega`).
  `stats::factanal` uses `factors=1`, `rotation="none"`, and deterministic
  `control=list(nstart=1)`; no RNG seed or random starts are needed. The complete
  case count is supplied as `n.obs`, including in pairwise mode; pairwise cell
  counts are separately recorded. This is not ordinal/polychoric omega, a
  dimensionality test, or evidence of instrument validity.
- `--coerce` explicitly converts non-numeric items to numeric (default:
  `modules.scale.coerce`). Factor text values, not internal factor codes, are
  converted; logical and date/time conversions retain the existing R semantics.
  Unparseable text becomes missing **only when explicitly requesting coercion**:
  a captured warning and exact introduced-missing row indices document the loss.
  This analysis-time opt-in does not relax the stricter import-type contract.
  Infinite numeric values are errors.
- `--digits` is an integer from 0 to 15 (default: `defaults.digits`); rounding is
  presentation-only. Unknown missing/score choices do not fall back silently.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.
- `--interactive FALSE` disables prompts; the normal noninteractive CLI remains
  compatible. User-prompt storage follows the shared privacy configuration.

## Outputs

Scale uses the [shared execution and replay contract](../run-contract.md).
Each completed analysis preserves `.nlss/runs/<id>/request.json`, `result.json`, copied
templates and deterministic `output.md`; `--log FALSE` only disables optional standalone logging.
The saved input is immutable; reverse scoring and coercion do not overwrite the
working Parquet. Standalone final `report_<YYYYMMDD>_<metaskill>_<intent>.md` files
are still authored semantically by metaskills, not prescribed by these tables.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing the item analysis table and narrative reliability summary.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `result.json` retains unrounded `item_df` and `reliability_df`, plus
  `diagnostics_df` for covariance/correlation matrix validity. Both statistical
  tables include `group_missing`; a literal group code or value label `NA` is
  never merged with actual missing grouping values.
- `request.json` records effective options, original/analysis classes, factor
  levels, reverse bounds actually applied to every selected item, coercion
  losses, original group identity, group row indices, variablewise valid rows,
  joint complete rows and pairwise observation counts.
- Unavailable alpha/standardized alpha/omega have explicit status fields and
  report explanations. One-item, all-missing, constant or insufficient-data cases
  may still produce valid descriptive output; they do not claim reliability was
  estimated. Standardized alpha requires the full selected-item correlation
  matrix. Singular correlation matrices make omega unavailable. Indefinite
  pairwise matrices yield a warning and no derived alpha/item-total estimates
  from the invalid matrix. Unexpected estimation errors fail the run instead of
  publishing an apparently successful missing coefficient.

## NLSS format Templates

Use the Markdown template at `assets/scale/default-template.md` when assembling scale reports. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.scale.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`item`, `group`, `group_missing`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `min`, `max`, `item_total_r`, `item_rest_r`, `alpha_if_deleted`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note Tokens

Available note tokens include:

`note_default`, `item_corr_note`, `alpha_note`, `reverse_note`, `missing_note`, `score_note`, `omega_note`.

### Narrative Tokens

Use `narrative.row_template` for per-group lines. Available row tokens include:

`group`, `group_label`, `n_items`, `n_total`, `n_complete`, `missing_n`, `missing_pct`, `alpha`, `alpha_std`, `omega_total`, `r_bar`, `r_min`, `r_max`, `score_method`, `score_mean`, `score_sd`, `score_min`, `score_max`, `missing_text`, `full_sentence`.

Availability tokens: `alpha_status`, `alpha_std_status`, `omega_status`,
`availability_text`. The default `full_sentence` includes unavailable-estimate
reasons; custom templates should preserve those limitations.

## NLSS format Reporting Guidance

- Report item means, SDs, corrected item-total correlations, and alpha-if-deleted alongside overall reliability.
- Include Cronbach's alpha and (when available) omega total; note missing-data handling and any reverse-scored items.
- If reliability cannot be estimated (e.g., too few items or insufficient variance), state that explicitly.
