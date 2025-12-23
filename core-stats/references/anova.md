---
name: anova-r
description: Between-subjects, within-subjects, and mixed ANOVA with APA-ready tables, post-hoc comparisons, and assumptions logging.
---

# ANOVA (Base R, APA 7)

## Overview

Run between-subjects, within-subjects (repeated measures), or mixed ANOVA in base R and produce APA 7-ready tables and narratives. Optional covariates (ANCOVA) and post-hoc comparisons are supported. Outputs include sums of squares, df, F, p, and effect sizes.

Post-hoc behavior: Tukey HSD is used for between-subjects factors; paired t-tests are used for within-subjects comparisons (and for mixed designs, within comparisons are computed separately per between-group combination).

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose a design (between/within/mixed) and specify variables.
3. Run `scripts/R/anova.R` with the correct flags, or use the PowerShell wrapper on Windows.
4. Use outputs (`apa_report.md`, `analysis_log.jsonl`) for APA reporting and diagnostics.

## Script: `scripts/R/anova.R`

Run with `Rscript` and base R only. Type II/III sums of squares require `car` (optional); if not installed, the script falls back to Type I.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\anova.R> --csv <path to CSV file> --dv outcome --between group
```

### Between-subjects ANOVA (CSV)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group
```

### Factorial between-subjects ANOVA + covariates

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group,gender --covariates age
```

### Within-subjects ANOVA (wide format)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --within pre,mid,post --subject-id id
```

### Mixed ANOVA (within + between)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --within pre,mid,post --between group --subject-id id
```

### Parquet input

```bash
Rscript <path to scripts/R/anova.R> --parquet <path to parquet file> --dv outcome --between group
```

### Interactive prompts

```bash
Rscript <path to scripts/R/anova.R> --interactive
```

## Options

Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--dv` required for between-subjects designs.
- `--between` is optional, but at least one of `--between` or `--within` is required.
- `--within` uses wide-format repeated measures (one column per measurement).
- `--subject-id` is required when `--within` is present.
- `--covariates` is optional (numeric).
- `--type` uses `modules.anova.type` (`I`, `II`, `III`); Type II/III requires `car` and apply to between-subjects models only.
- `--effect-size` uses `modules.anova.effect_size` (`eta_sq`, `partial_eta`).
- `--posthoc` uses `modules.anova.posthoc` (`none`, `tukey`, `pairwise`).
- `--p-adjust` uses `modules.anova.p_adjust` (e.g., `holm`, `bonferroni`).
- `--conf-level` uses `modules.anova.conf_level`.
- `--sphericity` uses `modules.anova.sphericity` (`auto`, `none`).
- `--bootstrap` uses `modules.anova.bootstrap` (TRUE/FALSE).
- `--bootstrap-samples` uses `modules.anova.bootstrap_samples` (default: 1000).
- `--digits` uses `defaults.digits`.
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Between-subjects factors are coerced to factors.
- Covariates are coerced to numeric where possible.
- Within-subjects designs must use wide format with one column per repeated measure.
- Missing values are removed listwise across all required columns per analysis.

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).
- `apa_report.md`: APA 7 report containing the ANOVA table and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `summary_df`, `posthoc_df`, and `assumptions_df`.
- When `--bootstrap TRUE`, `summary_df` includes `boot_ci_low` and `boot_ci_high` for the selected effect size.
- Assumption diagnostics (Shapiro-Wilk residual normality, homogeneity tests, and Mauchly for sphericity when applicable) are recorded in `analysis_log.jsonl`.

## APA 7 Templates (YAML)

Templates are stored under `core-stats/assets/anova/` and mapped in `core-stats/scripts/config.yml`:

- `templates.anova.default`: `anova/default-template.md`
- `templates.anova.posthoc`: `anova/posthoc-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table column keys (omnibus ANOVA)

Available column keys include:

`model`, `term`, `df1`, `df2`, `ss`, `ms`, `f`, `p`, `eta_sq`, `partial_eta_sq`, `boot_ci_low`, `boot_ci_high`, `df1_gg`, `df2_gg`, `p_gg`, `df1_hf`, `df2_hf`, `p_hf`.

### Table column keys (post-hoc)

`term`, `group`, `group_1`, `group_2`, `contrast`, `mean_diff`, `se`, `t`, `df`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`.

### Note tokens

Available note tokens include:

`note_default`, `assumption_note`.

### Narrative tokens

Omnibus narrative row tokens include:

`full_sentence`, `model`, `term`, `df1`, `df2`, `f`, `p`, `effect_size_label`, `effect_size_value`, `boot_ci`, `boot_ci_low`, `boot_ci_high`.

Post-hoc narrative row tokens include:

`full_sentence`, `term`, `group`, `group_1`, `group_2`, `mean_diff`, `t`, `df`, `p`, `p_adj`, `ci`.

## APA 7 Reporting Guidance

- Report F, df, p, and effect sizes for each omnibus effect (include bootstrap CIs when enabled).
- Indicate post-hoc method and p-value adjustment when reported.
- For repeated measures, report sphericity results (Mauchly) and note any violations.

## Dependencies

- Parquet input requires the R package `arrow`.
- Type II/III sums of squares require the R package `car` (optional).
