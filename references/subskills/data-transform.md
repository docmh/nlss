---
name: data-transform
description: Create/modify variables via calculations, transforms, standardization, recodes, bins, renames, and drops with confirmation safeguards; updates workspace parquet with backups and logs NLSS format changes.
license: Apache-2.0
---

# Data Transformations (Base R, NLSS format)

## Overview

Create or modify variables in a data frame: derive new variables, transform or standardize numeric variables, recode values, rename columns, or drop columns. Outputs include the transformed dataset plus an NLSS format-ready change report.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Define transformations:
   - `--calc` for new variables using expressions.
   - `--transform` for standard transforms (log, sqrt, scale).
   - `--standardize` for z-scores into new variables.
   - `--recode` for value mapping.
   - `--rename` and `--drop` for column management.
3. Run `scripts/R/data_transform.R` with appropriate flags.
4. Use outputs (workspace `<workspace-root>/<dataset-name>/<dataset-name>.parquet`, `report_canonical.md`, `analysis_log.jsonl`) in your response.

## Script: `scripts/R/data_transform.R`

Run with `Rscript` and base R only.

### CSV Input

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --calc "bmi=weight/(height^2)|ratio=var1/var2"
```

### Standard Transforms and Standardization

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --transform "income=log|stress=sqrt" --standardize age,score
```

### Percentile Bins (E.G., Quartiles)

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --percentile-bins "score=4|income=5"
```

### Custom Bins

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --bins "age=0,18,30,45,65|score=0,50,75,100"
```

### Recode Values

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --recode "gender=1:0,2:1|status=low:0,high:1"
```

### Rename and Drop Columns

```bash
Rscript <path to scripts/R/data_transform.R> --csv <path to CSV file> --rename old:new,old2:new2 --drop temp1,temp2 --confirm-drop
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/data_transform.R> --rds <path to RDS file> --calc "delta=post-pre"
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/data_transform.R> --rdata <path to RData file> --df <data frame name> --standardize score
```

### Parquet Input

```bash
Rscript <path to scripts/R/data_transform.R> --parquet <path to parquet file> --standardize score
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/data_transform.R> --interactive
```

## Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--calc` defines new variables as `newvar=expression`, separated by `|`.
- `--transform` uses `var=log|var2=sqrt|var3=scale` (supported: `log`, `log10`, `sqrt`, `exp`, `abs`, `center`, `scale`).
- `--standardize` z-standardizes numeric variables into new variables (default suffix: `modules.data_transform.standardize_suffix`).
- `--standardize-suffix` sets the z-score suffix (default: `modules.data_transform.standardize_suffix`).
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
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Safety Confirmations

- Always ask the user before overwriting variables or dropping columns.
- Use `--overwrite-vars` with `--confirm-overwrite` to replace existing variables.
- Use `--confirm-drop` to delete variables. Input files are not modified; outputs are written to the dataset workspace folder.

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).

- `<workspace-root>/<dataset-name>/<dataset-name>.parquet`: Workspace dataset copy updated in place (preferred; backup created before overwrite).
- `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet`: Backup of the previous parquet before overwrites.
- `transformed_data.rds`: Fallback output only if no workspace `.parquet` copy is available (written in the dataset workspace).
- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

Undo: replace `<workspace-root>/<dataset-name>/<dataset-name>.parquet` with the most recent backup in `<workspace-root>/<dataset-name>/backup/`.

## NLSS format Templates

Use the Markdown template at `assets/data-transform/default-template.md` when assembling data transformation reports. If the template exists, `data_transform.R` uses it for `report_canonical.md`.

### YAML Template Controls

- The template path can be overridden via `templates.data_transform.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`step`, `action`, `action_code`, `variable`, `new_variable`, `details`, `note`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `note`).

### Note Tokens

Available note tokens include:

`note_default`, `action_codes`, `note_details`.

### Summary Tokens

Summary tokens available for custom templates include:

`steps_total`, `actions_present`, `calc_vars`, `transform_vars`, `standardize_vars`, `recode_vars`, `percentile_bin_vars`, `bin_vars`, `rename_pairs`, `drop_vars`, `action_codes`.

### Narrative Tokens

Use `narrative.row_template` for per-step lines. Available row tokens include:

`step`, `action`, `action_label`, `variable`, `new_variable`, `details`, `note`, `full_sentence`.

## NLSS format Reporting Guidance

- Report derived variables and transformation types, noting any standardization or recoding.
- If variables were dropped or renamed, document those changes in the narrative.
