---
name: mi-regression
description: Fit supported linear or generalized linear models to every preserved mice imputation and pool estimates and uncertainty, with verified input provenance and replayable outputs.
license: Apache-2.0
---

# Multiple-imputation regression

Use for inferential regression from a Phase-1 `mice` artifact. Do not analyze the
averaged `_imp` columns as a replacement for multiple-imputation inference.
Read [MI scope and scientific contract](../mi-pooling.md) before choosing models
and interpreting coefficients. Read [run contract](../run-contract.md) for saved
requests, errors and replay.

## Inputs and workflow

Clarify the outcome, predictors, coding/reference levels, link and scientific
purpose. Check how the imputations were generated and their diagnostics. The
artifact must contain every model variable and preserve its input snapshot.

```bash
Rscript scripts/R/mi_regression.R \
  --mids /path/to/project/survey/imputations/mice-<sha256>/mids.rds \
  --formula 'outcome ~ age + condition' --family gaussian --conf-level 0.95
```

Supported families: Gaussian/identity (`lm`); Binomial/logit, probit or cloglog;
Poisson/log (`glm`). Existing other NLSS models remain available, but this adapter
does not claim pooled inference for them. Unsupported model syntax or failed
per-imputation fits fail explicitly, without falling back to single completions.

## Options

- `--mids`: preserved `mids.rds` or its artifact directory, with adjacent metadata.
- `--formula`: explicit named-variable model; `+`, `-`, `*`, `:`, parentheses,
  intercept `0`/`1`, and backticked variable names are supported.
- `--family`, `--link`, `--conf-level`, `--maxit`: override `modules.mi_regression`
  defaults. `maxit` controls GLM fitting, not regeneration of imputations.
- `--digits`: display precision, independent of stored unrounded values.
- `--template`: template key or Markdown path; copied into the run for replay.
- `--log`: optional standalone logging toggle; project run evidence and root protocol remain mandatory.
- `--user-prompt`: researcher intent/context under normal NLSS logging rules.

No raw CSV/SAV input, interactive model selection, data transformation, weight,
offset, subset or mixed-model syntax is accepted here. These boundaries constrain
the supported adapter, not the statistical scope of NLSS as a whole.

## Outputs and template

The current project receives a run under `.nlss/runs/<id>/` with `request.json`, `result.json`, `output.md`,
`fits.rds` and `pooled.rds`. Results include the unrounded coefficient table,
pooled uncertainty, `inference_pooled: true`, per-fit diagnostics and original
imputation provenance. Existing root canonical Markdown remains automatic; no project JSONL journal is added.

The default template is `assets/mi-regression/default-template.md`, registered at
`templates.mi_regression.default`. Tokens are `analysis_flags`, `table_number`,
`table_body`, `note_default`, and `narrative_default`. The computed table contains
term, estimate, standard error, test statistic, degrees of freedom, p-value and
confidence limits. Binomial/Poisson coefficients are on their declared link scale.

Explain the research finding semantically, including uncertainty, missingness
assumptions, diagnostic limitations and the meaning of its scale. A successful
pooling operation alone is not evidence for a sound imputation model.
