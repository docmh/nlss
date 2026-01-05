# Assumptions Deliberate Test Plan

Goal: exercise assumptions coverage across t-tests, ANOVA, regression, mixed models, and SEM with clean, edge, and negative cases.

## Scope

- Cover analysis families: `ttest`, `anova`, `regression`, `mixed_models`, `sem` (including cfa/path/mediation/invariance routes).
- Exercise input modes (csv/parquet/rds/rdata, sav when available) plus help and interactive prompts.
- Validate normality/homogeneity behavior, regression diagnostics (linearity, homoscedasticity, VIF, Durbin-Watson, outliers, influence), mixed-model diagnostics (random-effects normality, singularity, convergence, DHARMa/performance when available), and SEM diagnostics (Mardia, Mahalanobis, collinearity, Heywood, convergence).
- Confirm numeric thresholds and option normalization (alpha, VIF thresholds, outlier/Cook, max_shapiro_n, SEM thresholds, maxfun/reml/optimizer).
- Validate auto-analysis routing and logging behaviors (`--log`, `--user-prompt`).
- Template override checks for all assumptions templates (ttest/anova/regression/mixed_models/sem).
- Negative cases for invalid inputs (missing variables, invalid group levels, invalid model specs, missing random effects, invalid SEM inputs).
- Numeric value tests: compare `analysis_log.jsonl` outputs against `tests/values/assumptions_golden.csv` (regenerate with `tests/values/assumptions_compute_golden.R` when datasets or methods change).

## Prereqs

- Run from repo root.
- Rscript available.
- R packages installed: `arrow`, `yaml`, `jsonlite`, `lme4`, `lavaan`.
- Optional: `haven` for `.sav` input coverage; `performance`, `DHARMa`, `influence.ME`, `MVN` for optional diagnostics.
- Interactive coverage uses `NLSS_PROMPT_FILE` to feed prompts during automation.

## Run

Deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

Run only assumptions deliberate tests:

```bash
bash tests/smoke/run_assumptions_tests.sh
```

## Expected Outputs

- `outputs/test-runs/<timestamp>/assumptions_test.log`
- `outputs/test-runs/<timestamp>/assumptions_workspace/golden_dataset/analysis_log.jsonl`
- `outputs/test-runs/<timestamp>/assumptions_workspace/golden_dataset/report_canonical.md`
- `outputs/test-runs/<timestamp>/assumptions_workspace/mixed_models_long/analysis_log.jsonl`
- `outputs/test-runs/<timestamp>/assumptions_workspace/mixed_models_long/report_canonical.md`

## Notes

- The suite requires `lme4` and `lavaan`; it fails fast if either is missing.
- SAV input tests require `haven` to write the `.sav` file; otherwise they are skipped.
- Optional diagnostics (DHARMa/performance/influence.ME/MVN) are asserted only when the package is available.
