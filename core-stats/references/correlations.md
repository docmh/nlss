---
name: correlations-r
description: Create APA 7 style correlation analyses using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts. Use when Codex needs Pearson/Spearman/Kendall correlations, correlation matrices, cross-correlations between variable sets, partial correlations with controls, optional grouping, and APA-ready tables/text.
---

# Correlations (Base R, APA 7)

## Overview

Compute correlations for numeric variables (pairwise or matrix), with optional grouping and partial correlations. Outputs include an APA-ready report and JSONL logging with diagnostics.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, or interactive).
2. Choose variables: full matrix via `--vars` (or default numeric columns), or cross-correlation via `--x` and `--y`.
3. Choose correlation method (Pearson/Spearman/Kendall), missing-data handling, and any control variables.
4. Run `scripts/R/correlations.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`apa_report.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/correlations.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\correlations.R> --csv <path to CSV file> --vars age,score --out <working directory>\outputs\tmp
```

### CSV input

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score,stress
```

### Cross-correlation between sets

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --x age,stress --y wellbeing,performance
```

### Grouped correlations

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --group condition
```

### Partial correlations (controls)

```bash
Rscript <path to scripts/R/correlations.R> --csv <path to CSV file> --vars age,score --controls gender,education
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/correlations.R> --rds <path to RDS file> --vars age,score
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/correlations.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/correlations.R> --interactive
```

### Options

- Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.correlations.vars_default` (typically numeric columns) if omitted.
- `--x` and `--y` compute cross-correlations between two sets.
- `--group` is optional and produces grouped correlations.
- `--method` selects `pearson`, `spearman`, or `kendall` (default: `modules.correlations.method`).
- `--missing` selects `pairwise` or `complete` (default: `modules.correlations.missing`).
- `--alternative` selects `two.sided`, `greater`, or `less` (default: `modules.correlations.alternative`).
- `--controls` enables partial correlations (not supported for Kendall).
- `--p-adjust` adjusts p-values (`none`, `bonferroni`, `holm`, `hochberg`, `hommel`, `BH`, `BY`, `fdr`). Default: `modules.correlations.p_adjust`.
- `--conf-level` sets the Fisher z confidence level for Pearson/partial (default: `modules.correlations.conf_level`).
- `--coerce` coerces non-numeric columns to numeric (default: `modules.correlations.coerce`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--out` sets the output directory (default: `defaults.output_dir`).
- `--interactive` prompts for inputs.
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

- `apa_report.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Templates

Use the Markdown templates in `core-stats/assets/correlations` when assembling correlation reports. If the template exists, it must be used for `apa_report.md`.

- Use `core-stats/assets/correlations/default-template.md` for correlation matrices created from `--vars` (or default numeric columns).
- Use `core-stats/assets/correlations/cross-correlation-template.md` for cross-correlations created from `--x` and `--y`.
- For partial correlations, keep the same template as the matrix or cross-correlation output and include the control variables in the analysis flags and note.
  
### YAML template controls

- Template paths can be overridden via `templates.correlations.default` and `templates.correlations.cross` in `core-stats/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`group`, `var1`, `var2`, `r`, `ci`, `p`, `p_adj`, `n`.

Use `drop_if_empty: true` to remove a column if all values are blank (e.g., `group`, `p_adj`, `ci`).

### Note tokens

Available note tokens include:

`note_default`, `ci_label`, `tail_note`, `missing_note`, `partial_note`, `p_adjust_note`, `ci_note`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`label`, `group`, `var1`, `var2`, `stat_text`, `r`, `ci`, `ci_text`, `p`, `n`, `missing_n`, `missing_pct`, `missing_text`, `full_sentence`.

## APA 7 Reporting Guidance

- Report method-specific coefficients (Pearson's r, Spearman's rho, Kendall's tau) with p-values and sample size.
- If using partial correlations, state the control variables explicitly.
- Note missing-data handling (pairwise vs complete) and any p-value adjustment.
