---
name: scale
description: Scale item analysis and reliability (alpha/omega), item-total correlations, reverse scoring, and grouped summaries with APA-ready outputs.
---

# Scale Analysis (Base R, APA 7)

## Overview

Compute item-level statistics and scale reliability metrics for psychometric scales. Outputs include an APA-ready table with item analysis and a narrative summary of internal consistency.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose item variables and an optional grouping variable.
3. (Optional) reverse-score items using `--reverse` plus `--reverse-min`/`--reverse-max`.
4. Run `scripts/R/scale.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/scale.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\scale.R> --csv <path to CSV file> --vars item1,item2,item3
```

### CSV input

```bash
Rscript <path to scripts/R/scale.R> --csv <path to CSV file> --vars item1,item2,item3
```

### Grouped scale analysis

```bash
Rscript <path to scripts/R/scale.R> --csv <path to CSV file> --vars item1,item2,item3 --group condition
```

### Reverse scoring

```bash
Rscript <path to scripts/R/scale.R> --csv <path to CSV file> --vars item1,item2,item3 --reverse item2,item3 --reverse-min 1 --reverse-max 5
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/scale.R> --rds <path to RDS file> --vars item1,item2,item3
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/scale.R> --rdata <path to RData file> --df <data frame name> --vars item1,item2,item3
```

### Parquet input

```bash
Rscript <path to scripts/R/scale.R> --parquet <path to parquet file> --vars item1,item2,item3
```

### Interactive prompts

```bash
Rscript <path to scripts/R/scale.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.scale.vars_default` (typically numeric columns) if omitted; use `--vars` for item selection.
- `--group` is optional and produces grouped scale analyses.
- `--reverse` lists items to reverse-score. Use `--reverse-min`/`--reverse-max` (defaults from `modules.scale.reverse_min` / `modules.scale.reverse_max`, otherwise observed min/max).
- `--missing` selects `pairwise` or `complete` (default: `modules.scale.missing`).
- `--score` selects `sum` or `mean` for scale scores (default: `modules.scale.score`).
- `--omega` toggles omega total estimation (default: `modules.scale.omega`).
- `--coerce` coerces non-numeric columns to numeric (default: `modules.scale.coerce`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).

- `report_canonical.md`: APA 7 report containing the item analysis table and narrative reliability summary.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates

Use the Markdown template at `nlss/assets/scale/default-template.md` when assembling scale reports. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.scale.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`item`, `group`, `n`, `missing_n`, `missing_pct`, `mean`, `sd`, `min`, `max`, `item_total_r`, `item_rest_r`, `alpha_if_deleted`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note tokens

Available note tokens include:

`note_default`, `item_corr_note`, `alpha_note`, `reverse_note`, `missing_note`, `score_note`, `omega_note`.

### Narrative tokens

Use `narrative.row_template` for per-group lines. Available row tokens include:

`group`, `group_label`, `n_items`, `n_total`, `n_complete`, `missing_n`, `missing_pct`, `alpha`, `alpha_std`, `omega_total`, `r_bar`, `r_min`, `r_max`, `score_method`, `score_mean`, `score_sd`, `score_min`, `score_max`, `missing_text`, `full_sentence`.

## APA 7 Reporting Guidance

- Report item means, SDs, corrected item-total correlations, and alpha-if-deleted alongside overall reliability.
- Include Cronbach's alpha and (when available) omega total; note missing-data handling and any reverse-scored items.
- If reliability cannot be estimated (e.g., too few items or insufficient variance), state that explicitly.
