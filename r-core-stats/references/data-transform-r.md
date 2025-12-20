---
name: data-transform-r
description: Transform and manage data frames using base R, including calculated variables, transforms/standardization, recoding, renaming, and dropping columns. Use when Codex needs to modify variables or create derived variables and produce updated datasets plus APA-ready change logs with confirmation before overwriting or deleting data.
---

# Data Transformations (Base R, APA 7)

## Overview

Create or modify variables in a data frame: derive new variables, transform or standardize numeric variables, recode values, rename columns, or drop columns. Outputs include the transformed dataset plus an APA-ready change report.

## Core Workflow

1. Identify input type (CSV, SAV, RDS, RData data frame, or interactive).
2. Define transformations:
   - `--calc` for new variables using expressions.
   - `--transform` for standard transforms (log, sqrt, scale).
   - `--standardize` for z-scores into new variables.
   - `--recode` for value mapping.
   - `--rename` and `--drop` for column management.
3. Run `scripts/R/data_transform.R` with appropriate flags or the Windows wrapper.
4. Use outputs (`transformed_data.rds`, `apa_report.md`, `analysis_log.jsonl`) in your response.

## Script: `scripts/R/data_transform.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\data_transform.R> --csv <path to CSV file> --calc "bmi=weight/(height^2)" --standardize age,score --out <working directory>\outputs\tmp
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

### Interactive prompts

```bash
Rscript <path to scripts/R/data_transform.R> --interactive
```

## Options

- `--calc` defines new variables as `newvar=expression`, separated by `|`.
- `--transform` uses `var=log|var2=sqrt|var3=scale` (supported: `log`, `log10`, `sqrt`, `exp`, `abs`, `center`, `scale`).
- `--standardize` z-standardizes numeric variables into new variables (default suffix `_z`).
- `--percentile-bins` creates percentile bins (e.g., `var=4` for quartiles). Output bins are numeric 1..k.
- `--bins` creates custom bins from numeric breakpoints (values outside the range become `NA`).
- `--recode` maps values using `var=old:new,old2:new2` (use quotes for strings).
- `--rename` maps `old:new` pairs (comma-separated).
- `--drop` removes columns (comma-separated).
- `--transform-into`, `--standardize-into`, `--percentile-into`, `--bins-into`, `--recode-into` override output names as `var=newname|var2=newname2`.
- `--percentile-suffix` sets the percentile bin suffix (default `_pct`).
- `--bins-suffix` sets the custom bin suffix (default `_bin`).
- `--coerce` allows coercing non-numeric variables to numeric for transforms/standardization.
- `--overwrite-vars` allows overwriting existing variables; use with `--confirm-overwrite`.
- `--confirm-overwrite` or `--interactive` is required when overwriting existing variables.
- `--confirm-drop` or `--interactive` is required when dropping variables.
- `--out` sets the output directory (default: `<working directory>/outputs/tmp`, relative to the working directory).
- `--log` toggles JSONL logging (default: TRUE).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Safety confirmations

- Always ask the user before overwriting variables or dropping columns.
- Use `--overwrite-vars` with `--confirm-overwrite` to replace existing variables.
- Use `--confirm-drop` to delete variables. Input files are not modified; outputs are written to the output directory.

## Outputs

- `transformed_data.rds`: Updated dataset in RDS format.
- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Reporting Guidance

- Report derived variables and transformation types, noting any standardization or recoding.
- If variables were dropped or renamed, document those changes in the narrative.
*** End Patch"}>}
