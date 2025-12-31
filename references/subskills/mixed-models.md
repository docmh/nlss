---
name: mixed-models
description: Linear mixed-effects models (lme4) with formula or dv/fixed/random specs, random-effects reporting, optional emmeans/contrasts (including custom JSON), diagnostics, and NLSS format outputs.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2025 Mike Hammes"
  nlss.version: "1.0.0"
---

# Mixed Models (LMM, NLSS format)

## Overview

Fit linear mixed-effects models (LMM) for clustered or longitudinal data using `lme4`. Outputs include fixed effects, random-effects variance components, model fit statistics, optional estimated marginal means/contrasts (pairwise, built-in methods, or custom JSON), and diagnostics. This subskill is for observed-variable mixed-effects modeling only.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Specify the model using `--formula` or `--dv` + `--fixed` + `--random`.
3. Optionally request estimated marginal means (`--emmeans`) and contrasts (`--contrasts`, `--contrast-file` for custom JSON).
4. Run `scripts/R/mixed_models.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for NLSS format reporting.

## Script: `scripts/R/mixed_models.R`

### Formula-Based Model (CSV)

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time + (1|id)"
```

### Build From Dv + Fixed + Random

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --dv score --fixed time,group --random "1|id,time|id"
```

### Marginal Means + Contrasts

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time*group + (1|id)" --emmeans time*group --contrasts pairwise
```

### Planned Contrasts (Custom JSON)

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time*group + (1|id)" --emmeans group3 --contrasts custom --contrast-file contrasts.json
```

### Built-in Contrast Method

```bash
Rscript <path to scripts/R/mixed_models.R> --csv <path to CSV file> --formula "score ~ time*group + (1|id)" --emmeans group3 --contrasts trt.vs.ctrl
```

### Parquet Input

```bash
Rscript <path to scripts/R/mixed_models.R> --parquet <path to parquet file> --formula "score ~ time + (1|id)"
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/mixed_models.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--formula` full lme4 formula (overrides `--dv`, `--fixed`, `--random`).
- `--dv` dependent variable (required without `--formula`).
- `--fixed` comma-separated fixed effects (omit for intercept-only models when not using `--formula`).
- `--random` required without `--formula` (comma-separated random terms in `term|group` syntax).
- `--reml` uses `modules.mixed_models.reml` (TRUE/FALSE).
- `--type` uses `modules.mixed_models.type` (`I`, `II`, `III`) for omnibus tests when available (`lmerTest` or `car` required for Type II/III).
- `--df-method` uses `modules.mixed_models.df_method` (`satterthwaite`, `kenward-roger`, `none`); df/*p*-values require `lmerTest`.
- `--standardize` uses `modules.mixed_models.standardize` (`none`, `predictors`).
- `--emmeans` uses `modules.mixed_models.emmeans` (`none` or a factor term such as `time*group`).
- `--contrasts` uses `modules.mixed_models.contrasts` (`none`, `pairwise`, `custom`, or any `emmeans` method string) and requires `--emmeans` unless the contrast JSON specifies a `term`.
- `--contrast-file` provides a JSON contrast spec (custom weights or a method plus optional arguments).
- `--p-adjust` uses `modules.mixed_models.p_adjust`.
- `--conf-level` uses `modules.mixed_models.conf_level`.
- `--optimizer` uses `modules.mixed_models.optimizer`.
- `--maxfun` uses `modules.mixed_models.maxfun`.
- `--diagnostics` uses `modules.mixed_models.diagnostics` (TRUE/FALSE).
- `--max-shapiro-n` uses `modules.mixed_models.max_shapiro_n`.
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Data must be in long format (one row per observation). Use `data-transform` to reshape wide data beforehand.
- Categorical predictors and grouping variables are coerced to factors.
- Missing values are removed listwise across all variables referenced in the model.
- `--formula` is recommended for complex random-effects structures.
- If `--emmeans` is requested but the `emmeans` package is unavailable, marginal means are skipped and noted.
- `--contrasts` is ignored when `--emmeans` is not set (unless the contrast JSON provides `term`).

### Contrast JSON Format

Custom contrasts can be specified as named weight vectors. Weights may be ordered numeric arrays (matching the `emmeans` row order) or named weights keyed by level labels. For multi-factor terms, labels are rendered like `factor=level, factor2=level2` in the `emmeans` output.

Example (custom weights):

```json
{
  "term": "group3",
  "contrasts": {
    "A_vs_B": {"A": 1, "B": -1, "C": 0},
    "A_vs_C": [1, 0, -1]
  }
}
```

Example (built-in method with args):

```json
{
  "term": "group3",
  "method": "trt.vs.ctrl",
  "args": {"ref": "A"}
}
```

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).
- `report_canonical.md`: NLSS format report containing fixed effects and narrative summaries.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `fixed_effects_df`, `random_effects_df`, `fit_df`, `r2_df`, `icc_df`, `anova_df`, `emmeans_df`, `contrasts_df`, and `diagnostics_df`.

## NLSS format Templates (YAML)

Templates are stored under `assets/mixed-models/` and mapped in `scripts/config.yml`:

- `templates.mixed_models.default`: `mixed-models/default-template.md`
- `templates.mixed_models.emmeans`: `mixed-models/emmeans-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys (Fixed Effects)

Available column keys include:

`model`, `term`, `b`, `se`, `df`, `t`, `p`, `ci_low`, `ci_high`, `std_beta`.

### Table Column Keys (Emmeans/Contrasts)

Available column keys include:

`term`, `level`, `contrast`, `emmean`, `estimate`, `se`, `df`, `t`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`.

### Note Tokens

Available note tokens include:

`note_default`, `random_effects_note`, `fit_note`, `icc`, `r2_marginal`, `r2_conditional`, `convergence_note`, `optimizer`, `reml`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `term`, `b`, `se`, `df`, `t`, `p`, `ci`, `ci_low`, `ci_high`, `std_beta`.

## NLSS format Reporting Guidance

- Report fixed effects with *b*, *SE*, *df*, *t*, *p*, and confidence intervals.
- Report random-effects variance components and ICC when relevant.
- Report model fit indices (AIC/BIC/logLik) and *R*² (marginal/conditional) when available.
- For marginal means and contrasts, report adjusted *p*-values and confidence intervals.

## Dependencies

- Parquet input requires the R package `arrow`.
- Mixed models require the R packages `lme4` and `performance` (*R*²/ICC).
- Optional: `lmerTest` for *df*/*p*-values, `emmeans` for marginal means/contrasts, and `car` for Type II/III omnibus tests.