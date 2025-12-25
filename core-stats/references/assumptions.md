---
name: assumptions
description: Assumption checks for t-tests, ANOVA, and regression with APA-ready tables, narratives, and diagnostics.
---

# Assumptions Checks (Base R, APA 7)

## Overview

Run assumption and diagnostic checks for t-tests, ANOVA (between, within, mixed), and regression (including multiple and hierarchical models). The script outputs APA-ready tables/narratives plus a machine-readable JSONL log.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose the analysis family (`ttest`, `anova`, or `regression`) and specify variables.
3. Run `scripts/R/assumptions.R` with the correct flags, or use the PowerShell wrapper on Windows.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`) for reporting or downstream modules.

## Script: `scripts/R/assumptions.R`

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\assumptions.R> --csv <path to CSV file> --analysis ttest --vars score --group condition
```

### t-test assumptions (independent samples)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --vars score --group condition
```

### t-test assumptions (paired)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --x pre_score --y post_score
```

### ANOVA assumptions (between-subjects)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --dv score --between group,gender
```

### ANOVA assumptions (within or mixed; wide format)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --within pre,mid,post --between group
```

### Regression assumptions (multiple regression)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --ivs age,stress,trait
```

### Regression assumptions (hierarchical blocks)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --blocks age,gender;stress,trait
```

### Interactive prompts

```bash
Rscript <path to scripts/R/assumptions.R> --interactive
```

## Options

Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--analysis` defaults to `modules.assumptions.analysis` (`auto`, `ttest`, `anova`, `regression`).
- `--vars` defaults to `modules.assumptions.vars_default` (numeric columns).
- `--normality` defaults to `modules.assumptions.normality` (`shapiro` or `none`).
- `--homogeneity` defaults to `modules.assumptions.homogeneity` (`levene`, `bartlett`, `fligner`, `f`, `all`, `none`).
- `--linearity` defaults to `modules.assumptions.linearity` (TRUE/FALSE).
- `--homoscedasticity` defaults to `modules.assumptions.homoscedasticity` (TRUE/FALSE).
- `--vif` defaults to `modules.assumptions.vif` (TRUE/FALSE).
- `--durbin-watson` defaults to `modules.assumptions.durbin_watson` (TRUE/FALSE).
- `--outliers` defaults to `modules.assumptions.outliers` (TRUE/FALSE).
- `--influence` defaults to `modules.assumptions.influence` (TRUE/FALSE).
- `--alpha` defaults to `modules.assumptions.alpha`.
- Thresholds can be overridden via:
  - `--vif-warn` (`modules.assumptions.vif_warn`)
  - `--vif-high` (`modules.assumptions.vif_high`)
  - `--outlier-z` (`modules.assumptions.outlier_z`)
  - `--cook-multiplier` (`modules.assumptions.cook_multiplier`)
  - `--max-shapiro-n` (`modules.assumptions.max_shapiro_n`)
- `--digits` uses `defaults.digits`.
- `--log` uses `defaults.log`.

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- `ttest`:
  - One-sample/independent: use `--vars` and optional `--group`.
  - Paired: use `--x` and `--y` lists of equal length.
- `anova`:
  - Between-subjects: use `--dv` and `--between`.
  - Within/mixed: use `--within` (wide format, one column per repeated measure) and optional `--between`.
- `regression`:
  - Multiple regression: use `--dv` and `--ivs`.
  - Hierarchical regression: use `--blocks` (semicolon-separated blocks; blocks are cumulative).
- Missing values are handled listwise per test or model.

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).
- `apa_report.md`: APA 7 report containing analysis flags, table, and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).
- Diagnostics include normality tests, homogeneity tests, sphericity (when applicable), and regression diagnostics (VIF, Breusch-Pagan, Durbin-Watson, outliers, influence).

## APA 7 Templates (YAML)

Templates are stored under `core-stats/assets/assumptions/` and mapped in `core-stats/scripts/config.yml`:

- `templates.assumptions.ttest`: `assumptions/ttest-template.md`
- `templates.assumptions.anova`: `assumptions/anova-template.md`
- `templates.assumptions.regression`: `assumptions/regression-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys include:

`analysis_type`, `model`, `assumption`, `test`, `target`, `group`, `n`, `statistic`, `df`, `p`, `value`, `decision`, `note`.

Use `drop_if_empty: true` to hide columns with all empty values.

### Note tokens

Available note tokens include:

`note_default`, `alpha`, `homogeneity_tests`, `vif_warn`, `vif_high`, `outlier_z`, `cook_threshold`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `analysis_type`, `model`, `assumption`, `test`, `target`, `group`, `statistic`, `df`, `p`, `value`, `decision`, `n`.

## APA 7 Reporting Guidance

- Report the assumption tests used (e.g., Shapiro-Wilk, Levene, Mauchly, Breusch-Pagan) with statistics and p-values.
- For regression, report VIF values and any influence/outlier flags.
- Note any violations and consider corrections or robust alternatives when assumptions are not met.
