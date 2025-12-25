---
name: mixed-models
description: Linear mixed-effects models with random effects, marginal means, diagnostics, and APA-ready outputs.
---

# Mixed Models (LMM, APA 7)

## Overview

Fit linear mixed-effects models (LMM) for clustered or longitudinal data using `lme4`. Outputs include fixed effects, random-effects variance components, model fit statistics, optional estimated marginal means/contrasts, and diagnostics. This subskill is for observed-variable mixed-effects modeling only.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Specify the model using `--formula` or `--dv` + `--fixed` + `--random`.
3. Optionally request estimated marginal means (`--emmeans`) and pairwise contrasts.
4. Run `scripts/R/mixed_models.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`apa_report.md`, `analysis_log.jsonl`) for APA reporting.

## Script: `scripts/R/mixed_models.R`

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\mixed_models.R> --csv <path to CSV file> --formula "score ~ time + (1|id)"
```

### Formula-based model (CSV)

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time + (1|id)"
```

### Build from dv + fixed + random

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --dv score --fixed time,group --random "1|id,time|id"
```

### Marginal means + contrasts

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time*group + (1|id)" --emmeans time*group --contrasts pairwise
```

### Parquet input

```bash
Rscript <path to scripts/R/mixed_models.R> --parquet <path to parquet file> --formula "score ~ time + (1|id)"
```

### Interactive prompts

```bash
Rscript <path to scripts/R/mixed_models.R> --interactive
```

## Options

Defaults are loaded from `core-stats/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--formula` full lme4 formula (overrides `--dv`, `--fixed`, `--random`).
- `--dv` dependent variable (required without `--formula`).
- `--fixed` comma-separated fixed effects (omit for intercept-only models when not using `--formula`).
- `--random` required without `--formula` (comma-separated random terms in `term|group` syntax).
- `--reml` uses `modules.mixed_models.reml` (TRUE/FALSE).
- `--type` uses `modules.mixed_models.type` (`I`, `II`, `III`) for omnibus tests when available (`lmerTest` or `car` required for Type II/III).
- `--df-method` uses `modules.mixed_models.df_method` (`satterthwaite`, `kenward-roger`, `none`); df/p-values require `lmerTest`.
- `--standardize` uses `modules.mixed_models.standardize` (`none`, `predictors`).
- `--emmeans` uses `modules.mixed_models.emmeans` (`none` or a factor term such as `time*group`).
- `--contrasts` uses `modules.mixed_models.contrasts` (`none`, `pairwise`) and requires `--emmeans`.
- `--p-adjust` uses `modules.mixed_models.p_adjust`.
- `--conf-level` uses `modules.mixed_models.conf_level`.
- `--optimizer` uses `modules.mixed_models.optimizer`.
- `--maxfun` uses `modules.mixed_models.maxfun`.
- `--diagnostics` uses `modules.mixed_models.diagnostics` (TRUE/FALSE).
- `--max-shapiro-n` uses `modules.mixed_models.max_shapiro_n`.
- `--digits` uses `defaults.digits`.
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Data must be in long format (one row per observation). Use `data-transform` to reshape wide data beforehand.
- Categorical predictors and grouping variables are coerced to factors.
- Missing values are removed listwise across all variables referenced in the model.
- `--formula` is recommended for complex random-effects structures.
- If `--emmeans` is requested but the `emmeans` package is unavailable, marginal means are skipped and noted.
- `--contrasts` is ignored when `--emmeans` is not set.

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).
- `apa_report.md`: APA 7 report containing fixed effects and narrative summaries.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `fixed_effects_df`, `random_effects_df`, `fit_df`, `r2_df`, `icc_df`, `anova_df`, `emmeans_df`, `contrasts_df`, and `diagnostics_df`.

## APA 7 Templates (YAML)

Templates are stored under `core-stats/assets/mixed-models/` and mapped in `core-stats/scripts/config.yml`:

- `templates.mixed_models.default`: `mixed-models/default-template.md`
- `templates.mixed_models.emmeans`: `mixed-models/emmeans-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys (fixed effects)

Available column keys include:

`model`, `term`, `b`, `se`, `df`, `t`, `p`, `ci_low`, `ci_high`, `std_beta`.

### Table column keys (emmeans/contrasts)

Available column keys include:

`term`, `level`, `contrast`, `emmean`, `estimate`, `se`, `df`, `t`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`.

### Note tokens

Available note tokens include:

`note_default`, `random_effects_note`, `fit_note`, `icc`, `r2_marginal`, `r2_conditional`, `convergence_note`, `optimizer`, `reml`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `term`, `b`, `se`, `df`, `t`, `p`, `ci`, `ci_low`, `ci_high`, `std_beta`.

## APA 7 Reporting Guidance

- Report fixed effects with b, SE, df, t, p, and confidence intervals.
- Report random-effects variance components and ICC when relevant.
- Report model fit indices (AIC/BIC/logLik) and R2 (marginal/conditional) when available.
- For marginal means and contrasts, report adjusted p-values and confidence intervals.

## Dependencies

- Parquet input requires the R package `arrow`.
- Mixed models require the R package `lme4`.
- Optional: `lmerTest` for df/p-values, `emmeans` for marginal means/contrasts, `performance` for R2/ICC, and `car` for Type II/III omnibus tests.
