---
name: mixed-models
description: Linear mixed-effects models (lme4) with formula or dv/fixed/random specs, random-effects reporting, optional emmeans/contrasts (including custom JSON), diagnostics, and NLSS format outputs.
license: Apache-2.0
---

# Mixed Models (LMM, NLSS format)

## Overview

Fit linear mixed-effects models (LMM) for clustered or longitudinal data using `lme4`. Outputs include fixed effects, random-effects variance components, model fit statistics, optional estimated marginal means/contrasts (pairwise, built-in methods, or custom JSON), and diagnostics. This subskill is for observed-variable mixed-effects modeling only.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Specify the model using `--formula` or `--dv` + `--fixed` + `--random`.
3. Optionally request estimated marginal means (`--emmeans`) and contrasts (`--contrasts`, `--contrast-file` for custom JSON).
4. Run the `mixed-models` operation through `run_nlss.R` with the correct flags.
5. Review the recorded estimation method, retained cases, singularity/convergence and unavailable estimates before interpretation. Use the immutable run's values and diagnostics as evidence for a semantic research report, not as a mandatory report outline.

This adapter follows the [import contract](../import-contract.md) and
[run/replay contract](../run-contract.md). Imported labels do not determine variable roles.

## Execution: `mixed-models`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

### Formula-Based Model (CSV)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --csv <path to CSV file> --formula "score ~ time + (1|id)"
```

### Build From Dv + Fixed + Random

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --csv <path to CSV file> --dv score --fixed time,group --random "1|id,time|id"
```

### Marginal Means + Contrasts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --csv <path to CSV file> --formula "score ~ time*group + (1|id)" --emmeans time*group --contrasts pairwise
```

### Planned Contrasts (Custom JSON)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --csv <path to CSV file> --formula "score ~ time*group3 + (1|id)" --emmeans group3 --contrasts custom --contrast-file contrasts.json
```

### Built-in Contrast Method

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --csv <path to CSV file> --formula "score ~ time*group3 + (1|id)" --emmeans group3 --contrasts trt.vs.ctrl
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --parquet <path to parquet file> --formula "score ~ time + (1|id)"
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" mixed-models --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--formula` full lme4 formula (overrides `--dv`, `--fixed`, `--random`).
- `--dv` dependent variable (required without `--formula`).
- `--fixed` comma-separated fixed effects (omit for intercept-only models when not using `--formula`).
- `--random` required without `--formula` (comma-separated random terms in `term|group` syntax).
- `--reml` uses `modules.mixed_models.reml` (TRUE/FALSE).
- `--type` uses `modules.mixed_models.type` (`I`, `II`, `III`); the requested type is passed explicitly to the selected inference engine, never replaced with a fallback type.
- `--df-method` uses `modules.mixed_models.df_method` (`satterthwaite`, `kenward-roger`, `none`). Satterthwaite and Kenward–Roger require `lmerTest`; Kenward–Roger also requires `pbkrtest` and REML. An explicit incompatible request fails.
- `--standardize` uses `modules.mixed_models.standardize` (`none`, `predictors`).
- `--emmeans` uses `modules.mixed_models.emmeans` (`none` or a factor term such as `time*group`).
- `--contrasts` uses `modules.mixed_models.contrasts` (`none`, `pairwise`, `custom`, or any `emmeans` method string) and requires `--emmeans` unless the contrast JSON specifies a `term`.
- `--contrast-file` provides a JSON contrast spec (custom weights or a method plus optional arguments).
- `--p-adjust` uses `modules.mixed_models.p_adjust`.
- `--conf-level` uses `modules.mixed_models.conf_level`.
- `--optimizer` uses `modules.mixed_models.optimizer`.
- `--maxfun` uses `modules.mixed_models.maxfun`.
- `--seed` uses `modules.mixed_models.seed` (default 1) when marginal means/contrasts are requested. This freezes stochastic multivariate-t adjustments; seed/RNG state are recorded.
- `--diagnostics` uses `modules.mixed_models.diagnostics` (TRUE/FALSE).
- `--max-shapiro-n` uses `modules.mixed_models.max_shapiro_n`.
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Data must be in long format (one row per observation). If reshaping is needed, prepare it explicitly with R (for example, `stats::reshape`) and retain the transformation script and resulting input version; the current `data-transform` CLI does not expose a reshape operation.
- Nonnumeric model predictors become factors; numeric predictors retain their numeric role even if they carry value labels. Use an explicit formula such as `factor(condition)` for a numeric categorical code.
- Group membership follows the actual fitted `lme4` grouping factors, including nested/crossed/random-slope designs. Numeric IDs that ordinary `factor()` would collapse due to display rounding receive collision-free, run-local grouping aliases; their original numeric role in any fixed effect is preserved.
- Ordinary missing cases are excluded jointly over model variables, then again if a formula transformation produces missing values. Infinite values in selected source variables, non-finite fitted model frames and a nonvarying response are rejected; infinity is not silently treated as ordinary missingness. Unselected columns do not affect case selection. Original and transformed exclusions, actual model-frame values/classes, factor coding and grouping-row maps are saved.
- The response is one numeric column. Prepare a transformed response explicitly before fitting; predictor transformations remain available in `--formula`.
- `--formula` is recommended for complex random-effects structures.
- Requested marginal means require `emmeans`; a missing dependency or invalid contrast is an error, not an omitted analysis. Contrasts require `--emmeans` or a `term` in the JSON.
- Custom contrast JSON is resolved and preserved in `request.json`; replay uses that content even if the original file is changed or removed.

### Inference and interpretation

| Requested df method | Fixed effects and intervals | Omnibus tests | Marginal means/contrasts |
| --- | --- | --- | --- |
| Satterthwaite | `lmerTest` t/df/p, t-Wald intervals | Explicit Type I/II/III `lmerTest` F | Explicit Satterthwaite |
| Kenward–Roger | `lmerTest` KR t/df/p, t-Wald intervals | Explicit Type I/II/III KR F; REML only | Explicit Kenward–Roger |
| none | No coefficient p or denominator df; normal-Wald intervals | Type I `lme4` sequential F without denominator df/p; Type II/III `car` Wald chi-squared | Explicit asymptotic z/normal inference |

Factor contrasts remain the recorded R/data coding; Type III is not an automatic
recoding to sum-to-zero contrasts. Interpret the tested hypotheses with that coding,
especially for interactions. Requested and effective methods are stored. The adapter
does not let `emmeans` silently switch methods because of its usual sample-size limit.
See [lmerTest's ANOVA methods](https://search.r-project.org/CRAN/refmans/lmerTest/html/anova.lmerModLmerTest.html)
and [emmeans mixed-model support](https://rvlenth.github.io/emmeans/articles/models.html#group-l--lmermod-models).

`--standardize predictors` retains the descriptive legacy coefficient
`b * SD(x) / SD(y)` for directly named numeric main effects only. It does not refit
standardized data and does not pretend to standardize factor coefficients or interactions.
R² and adjusted/unadjusted ICC use `performance`'s model-based variance definitions;
unavailable or partial values, including singular-model cases, are explicit.

The legacy `fit_df.deviance` value is retained, but its `criterion` distinguishes
ML deviance from a REML criterion. Do not compare REML criteria across different
fixed-effect designs. This adapter does not supply model-comparison tests or pooled
multiple-imputation LMM inference.

Singularity and optimizer convergence are different conditions and are always
reported, even with `--diagnostics FALSE`. Optimizer return codes, evaluation counts,
effective control limits and `lme4` messages are saved; `maxfun` maps to `maxeval`
for `nloptwrap` and `maxit` for optim-style optimizers. A singular fit is retained
with a warning, not automatically rejected or interpreted as optimizer failure.
See [lme4 singularity guidance](https://lme4.github.io/lme4/reference/isSingular.html).
Optional Shapiro–Wilk checks concern conditional residuals, not random-effect
normality or residual independence; invalid sample sizes produce explicit unavailable rows.

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

Every completed dataset-backed run writes `.nlss/runs/<run-id>/request.json`, `result.json`
and deterministic `output.md`, plus copies of the used templates. The root canonical Markdown is the automatic readable projection. `--log FALSE` disables only optional standalone logging,
not the auditable run bundle. Failed resolved analyses have a failed bundle and no
normal output; existing canonical report/log/manifest bytes are preserved.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format fixed-effect, variance-component and diagnostic tables, plus requested marginal means/contrasts.
- `result.json`: Machine-readable results and options, always retained in the saved run. Logged results include `fixed_effects_df`, `random_effects_df`, `fit_df`, `r2_df`, `icc_df`, `anova_df`, `emmeans_df`, `contrasts_df`, and `diagnostics_df`.
- Residual variance is included in `random_effects_df`; the legacy `icc` column is explicitly the adjusted ICC, alongside both named ICC variants. Non-estimable fixed effects/emmeans/contrast rows are retained with statuses, not silently deleted.
- Contrast adjustment metadata records the requested and actual `emmeans` method and explanatory messages, including different simultaneous-CI adjustments. The legacy `t` column contains the z ratio for explicitly asymptotic marginal-mean/contrast inference.

## NLSS format Templates (YAML)

Templates are stored under `assets/mixed-models/` and mapped in `scripts/config.yml`:

- `templates.mixed_models.default`: `mixed-models/default-template.md`
- `templates.mixed_models.tests`: `mixed-models/tests-of-fixed-effects-template.md`
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

`term`, `level`, `contrast`, `emmean`, `estimate`, `se`, `df`, `t`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`, `status`.

### Note Tokens

Available note tokens include:

`note_default`, `random_effects_note`, `fit_note`, `icc`, `r2_marginal`, `r2_conditional`, `convergence_note`, `optimizer`, `reml`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `term`, `b`, `se`, `df`, `t`, `p`, `ci`, `ci_low`, `ci_high`, `std_beta`.

## NLSS format Reporting Guidance

- Report fixed effects with b, SE, df, t, p, and confidence intervals.
- Report random-effects variance components and ICC when relevant.
- Report model fit indices (AIC/BIC/logLik) and R² (marginal/conditional) when available.
- For marginal means and contrasts, report adjusted p-values and confidence intervals.
- The deterministic output is a statistical evidence sheet, not a final semantic report. Synthesize the research question, design, uncertainty, diagnostic limits and substantive implications freely; do not merely fill or copy these templates.

## Dependencies

- Parquet input requires the R package `arrow`.
- Mixed models require `lme4`, its installed formula dependency `reformulas`, and `performance` (R²/ICC).
- Method-dependent: `lmerTest` for finite-df inference, `pbkrtest` for Kenward–Roger, `emmeans` for requested marginal means/contrasts, and `car` for Type II/III Wald tests when `--df-method none`.
