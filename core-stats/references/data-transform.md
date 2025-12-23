---
name: data-transform-r
description: Create or modify variables (calculate, standardize, recode, rename, drop) with safe prompts, updated datasets, and APA-ready change logs.
---

# Data Transformations (Base R, APA 7)

## Overview

Create or modify variables in a data frame: derive new variables, transform or standardize numeric variables, recode values, rename columns, or drop columns. Outputs include the transformed dataset plus an APA-ready change report.

## Core Workflow

1. Identify input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Define transformations:
   - `--calc` for new variables using expressions.
   - `--transform` for standard transforms (log, sqrt, scale).
   - `--standardize` for z-scores into new variables.
   - `--recode` for value mapping.
   - `--rename` and `--drop` for column management.
3. Run `scripts/R/data_transform.R` with appropriate flags or the Windows wrapper.
4. Use outputs (workspace `<workspace-root>/<dataset-name>/<dataset-name>.parquet`, `apa_report.md`, `analysis_log.jsonl`) in your response.

## Script: `scripts/R/data_transform.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\data_transform.R> --csv <path to CSV file> --calc "bmi=weight/(height^2)" --standardize age,score
```

### CSV input

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --calc "bmi=weight/(height^2)|ratio=var1/var2"
```

### Standard transforms and standardization

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --transform "income=log|stress=sqrt" --standardize age,score
```

### Percentile bins (e.g., quartiles)

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --percentile-bins "score=4|income=5"
```

### Custom bins

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --bins "age=0,18,30,45,65|score=0,50,75,100"
```

### Recode values

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --recode "gender=1:0,2:1|status=low:0,high:1"
```

### Rename and drop columns

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --rename old:new,old2:new2 --drop temp1,temp2 --confirm-drop
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/data_transform.R> --rds <path to RDS file> --calc "delta=post-pre"
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/data_transform.R> --rdata <path to RData file> --df <data frame name> --standardize score
```

### Parquet input

```bash
Rscript <path to scripts/R/data_transform.R> --parquet <path to parquet file> --standardize score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/data_transform.R> --interactive
```

## Options

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--calc` defines new variables as `newvar=expression`, separated by `|`.
- `--transform` uses `var=log|var2=sqrt|var3=scale` (supported: `log`, `log10`, `sqrt`, `exp`, `abs`, `center`, `scale`).
- `--standardize` z-standardizes numeric variables into new variables (default suffix: `modules.data_transform.standardize_suffix`).
- `--percentile-bins` creates percentile bins (e.g., `var=4` for quartiles). Output bins are numeric 1..k.
- `--bins` creates custom bins from numeric breakpoints (values outside the range become `NA`).
- `--recode` maps values using `var=old:new,old2:new2` (use quotes for strings).
- `--rename` maps `old:new` pairs (comma-separated).
- `--drop` removes columns (comma-separated).
- `--transform-into`, `--standardize-into`, `--percentile-into`, `--bins-into`, `--recode-into` override output names as `var=newname|var2=newname2`.
- `--percentile-suffix` sets the percentile bin suffix (default: `modules.data_transform.percentile_suffix`).
- `--bins-suffix` sets the custom bin suffix (default: `modules.data_transform.bins_suffix`).
- `--recode-suffix` sets the recode suffix (default: `modules.data_transform.recode_suffix`).
- `--coerce` allows coercing non-numeric variables to numeric for transforms/standardization (default: `modules.data_transform.coerce`).
- `--overwrite-vars` allows overwriting existing variables (default: `modules.data_transform.overwrite_vars`); use with `--confirm-overwrite`.
- `--confirm-overwrite` or `--interactive` is required when overwriting existing variables (default: `modules.data_transform.confirm_overwrite`).
- `--confirm-drop` or `--interactive` is required when dropping variables (default: `modules.data_transform.confirm_drop`).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Safety confirmations

- Always ask the user before overwriting variables or dropping columns.
- Use `--overwrite-vars` with `--confirm-overwrite` to replace existing variables.
- Use `--confirm-drop` to delete variables. Input files are not modified; outputs are written to the dataset workspace folder.

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).

- `<workspace-root>/<dataset-name>/<dataset-name>.parquet`: Workspace dataset copy updated in place (preferred; backup created before overwrite).
- `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet`: Backup of the previous parquet before overwrites.
- `transformed_data.rds`: Fallback output only if no workspace `.parquet` copy is available (written in the dataset workspace).
- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

Undo: replace `<workspace-root>/<dataset-name>/<dataset-name>.parquet` with the most recent backup in `<workspace-root>/<dataset-name>/backup/`.

## APA 7 Templates

Use the Markdown template at `core-stats/assets/data-transform/default-template.md` when assembling data transformation reports. If the template exists, `data_transform.R` uses it for `apa_report.md`.

### YAML template controls

- The template path can be overridden via `templates.data_transform.default` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`step`, `action`, `action_code`, `variable`, `new_variable`, `details`, `note`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `note`).

### Note tokens

Available note tokens include:

`note_default`, `action_codes`, `note_details`.

### Summary tokens

Summary tokens available for custom templates include:

`steps_total`, `actions_present`, `calc_vars`, `transform_vars`, `standardize_vars`, `recode_vars`, `percentile_bin_vars`, `bin_vars`, `rename_pairs`, `drop_vars`, `action_codes`.

### Narrative tokens

Use `narrative.row_template` for per-step lines. Available row tokens include:

`step`, `action`, `action_label`, `variable`, `new_variable`, `details`, `note`, `full_sentence`.

## APA 7 Reporting Guidance

- Report derived variables and transformation types, noting any standardization or recoding.
- If variables were dropped or renamed, document those changes in the narrative.
*** End Patch"}>}
