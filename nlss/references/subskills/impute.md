---
name: impute
description: Impute missing values into new _imp columns using simple methods or mice/VIM kNN, with per-variable method maps, optional indicators, parquet updates with backups, and APA outputs.
---

# Impute (Base R with optional mice/VIM, APA 7)

## Overview

Impute missing values into new columns (suffix `_imp`) while preserving the original variables. Supports base R methods (mean/median/mode/random/constant) and optional `mice` or `VIM::kNN` engines when installed. Indicator columns are created only when `--indicator TRUE` is passed.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables and an imputation engine (`simple`, `mice`, `knn`, or `auto`).
3. Run `scripts/R/impute.R` with the correct flags (or use the PowerShell wrapper on Windows).
4. Use outputs (workspace parquet, `report_canonical.md`, `analysis_log.jsonl`) in your response.

## Script: `scripts/R/impute.R`

Run with `Rscript` in base R. Optional engines require R packages `mice` or `VIM` when selected. Parquet I/O requires the `arrow` package.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\impute.R> --csv <path to CSV file> --vars age,gender
```

### CSV input

```bash
Rscript <path to scripts/R/impute.R> --csv <path to CSV file> --vars age,gender
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/impute.R> --rds <path to RDS file> --vars age,gender
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/impute.R> --rdata <path to RData file> --df <data frame name> --vars age,gender
```

### Parquet input

```bash
Rscript <path to scripts/R/impute.R> --parquet <path to parquet file> --vars age,gender
```

### Interactive prompts

```bash
Rscript <path to scripts/R/impute.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--vars` defaults to `modules.impute.vars_default` (typically `all`).
- `--engine` defaults to `modules.impute.engine` (`auto` chooses `mice` if installed, else `VIM`, else `simple`).
- `--numeric-method` defaults to `modules.impute.numeric_method` (for `simple` engine).
- `--categorical-method` defaults to `modules.impute.categorical_method` (for `simple` engine).
- `--method-map` overrides per-variable methods (e.g., `age=median|income=mean`).
- `--value-map` overrides per-variable constants (e.g., `income=0|status=missing`).
- `--constant` supplies a global constant for `constant` method when `value-map` is not set.
- `--suffix` sets the imputed column suffix (default: `modules.impute.suffix`); original variables are never overwritten.
- `--indicator` toggles missingness indicators (default: `modules.impute.indicator`).
- `--indicator-suffix` sets indicator suffix (default: `modules.impute.indicator_suffix`).
- `--skew-threshold` controls mean vs. median when `numeric-method` is `auto` (default: `modules.impute.skew_threshold`).
- `--m` and `--maxit` control `mice` multiple imputation (default: `modules.impute.m` and `modules.impute.maxit`); when using `mice`, `m` is coerced to at least 2.
- `--k` controls `VIM::kNN` neighbors (default: `modules.impute.k`).
- `--seed` sets random seed for random/engine methods.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).

- `<workspace-root>/<dataset-name>/<dataset-name>.parquet`: Workspace dataset copy updated in place (preferred; backup created before overwrite).
- `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet`: Backup of the previous parquet before overwrites.
- `imputed_data.rds`: Fallback output only if no workspace `.parquet` copy is available (written in the dataset workspace).
- `report_canonical.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

Undo: replace `<workspace-root>/<dataset-name>/<dataset-name>.parquet` with the most recent backup in `<workspace-root>/<dataset-name>/backup/`.

## APA 7 Templates

Use the Markdown template at `nlss/assets/impute/default-template.md` when assembling imputation reports. If the template exists, `impute.R` uses it for `report_canonical.md`.

### YAML template controls

- The template path can be overridden via `templates.impute.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}` if omitted).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`variable`, `type`, `missing_n`, `missing_pct`, `engine`, `method`, `impute_value`, `imputed_n`, `target`, `indicator`, `note`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `indicator` or `note`).

### Note tokens

Available note tokens include:

`note_default`, `engine_note`, `indicator_note`, `pool_note`, `seed_note`, `skipped_note`, `map_note`.

### Narrative tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`variable`, `type`, `missing_n`, `missing_pct`, `engine`, `method`, `impute_value`, `imputed_n`, `target`, `indicator`, `note`, `full_sentence`.

## APA 7 Reporting Guidance

- Report missingness range and the imputation engine used (including `m`/`maxit` for `mice` or `k` for `kNN`).
- State that imputed values were written to new `_imp` columns and originals were preserved.
- When indicators are requested, note the indicator suffix and variables that received indicators.
