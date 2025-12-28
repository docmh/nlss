---
name: missings
description: Missingness summaries and pattern tables with auto or manual handling (listwise, impute, indicator, drop), threshold rules, and in-place parquet updates with backups plus APA outputs.
---

# Missings (Base R, APA 7)

## Overview

Analyze missing-data patterns for selected variables, choose a handling strategy, and export a transformed dataset alongside APA-ready tables and narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables for missingness analysis.
3. Run `scripts/R/missings.R` with the correct flags (or use the PowerShell wrapper on Windows).
4. Use outputs (workspace `<workspace-root>/<dataset-name>/<dataset-name>.parquet`, `report_canonical.md`, `analysis_log.jsonl`) in your response.

## Script: `scripts/R/missings.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\missings.R> --csv <path to CSV file> --vars age,score
```

### CSV input

```bash
Rscript <path to scripts/R/missings.R> --csv <path to CSV file> --vars age,score
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/missings.R> --rds <path to RDS file> --vars age,score
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/missings.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet input

```bash
Rscript <path to scripts/R/missings.R> --parquet <path to parquet file> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/missings.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.missings.vars_default` (typically all columns).
- `--method` controls handling strategy (`auto`, `listwise`, `impute`, `indicator`, `drop`; default: `modules.missings.method`).
- `auto` selection uses the maximum missingness across selected variables to choose listwise (<= low), impute (<= moderate), indicator (<= high), or drop (otherwise).
- Thresholds are proportions in `[0, 1]`:
  - `--low-threshold` (`modules.missings.low_threshold`)
  - `--moderate-threshold` (`modules.missings.moderate_threshold`)
  - `--high-threshold` (`modules.missings.high_threshold`)
  - `--drop-threshold` (`modules.missings.drop_threshold`)
  - `--indicator-threshold` (`modules.missings.indicator_threshold`)
- `--indicator-suffix` sets the suffix for missingness indicators (default: `modules.missings.indicator_suffix`).
- `--skew-threshold` controls mean vs. median imputation for numeric variables (default: `modules.missings.skew_threshold`).
- `--max-patterns` caps the number of patterns shown (default: `modules.missings.max_patterns`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).

- `<workspace-root>/<dataset-name>/<dataset-name>.parquet`: Workspace dataset copy updated in place (preferred; backup created before overwrite).
- `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet`: Backup of the previous parquet before overwrites.
- `missing_handled_data.rds`: Fallback output only if no workspace `.parquet` copy is available (written in the dataset workspace).
- `report_canonical.md`: APA 7 report containing analysis type, tables, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

Undo: replace `<workspace-root>/<dataset-name>/<dataset-name>.parquet` with the most recent backup in `<workspace-root>/<dataset-name>/backup/`.

## APA 7 Templates

Use the Markdown template at `nlss/assets/missings/default-template.md` when assembling missingness reports. If the template exists, `missings.R` uses it for `report_canonical.md`.

### YAML template controls

- The template path can be overridden via `templates.missings.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `patterns_table.columns`: ordered column definitions for the pattern table.
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Summary table column keys

Available column keys for `table.columns` include:

`variable`, `type`, `total_n`, `missing_n`, `missing_pct`, `decision`, `impute_method`, `impute_value`, `indicator`.

### Pattern table column keys

Available column keys for `patterns_table.columns` include:

`pattern`, `missing_vars`, `missing_count`, `n`, `pct_total`.

### Note tokens

Available note tokens include:

`summary_note_body`, `patterns_note_body`.

### Other tokens

Additional tokens available for custom templates include:

`summary_table_body`, `patterns_table_body`, `pattern_limit`.

### Narrative tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`variable`, `missing_n`, `missing_pct`, `decision`, `impute_method`, `impute_value`, `indicator`, `full_sentence`.

## APA 7 Reporting Guidance

- Report the overall missingness range and complete-case percentage.
- Describe the selected handling method and any dropped variables or indicator columns.
- For each variable, report missingness and the imputation approach used (mean/median/mode) where applicable.
