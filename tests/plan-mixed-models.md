# Mixed Models Deliberate Test Plan

Goal: stress mixed models feature coverage with clean, edge, and negative cases for robust behavior.

## Scope

- Formula-based and dv/fixed/random model construction.
- Random term normalization and required random effects (including formula-only checks).
- Type I/II/III tests and df-method fallbacks (including Kenward-Roger when available).
- Standardization of predictors and standardized betas, plus invalid standardize fallbacks.
- Diagnostics toggle behavior plus max_shapiro_n gating (Shapiro present/absent).
- *R*² (marginal/conditional) and ICC reporting via `performance` (required).
- Optimizer/maxfun configuration (including invalid maxfun fallback).
- emmeans + contrasts + p-adjust and conf-level handling (including custom JSON contrasts, built-in methods, emmeans-only, and contrasts-without-emmeans).
- Input formats: parquet, CSV (custom sep/header), RDS, RData (df selection), SAV when available.
- Template overrides for default and emmeans outputs (emmeans tests require the package).
- Logging controls (`--log FALSE`, `--digits`, `--user-prompt`) in automated coverage; interactive mode is manual-only.
- Negative cases for missing inputs, missing variables, nonnumeric dv, invalid formula, missing random effects, missing contrast file for custom contrasts, RDS non-data-frame, RData missing *df*, and no complete cases.

## Data

- Base: `tests/data/golden_dataset.csv`
- Long mixed models data: generated via `tests/mixed_models_prep.R` into the test run tmp folder.
- Format variants created in tmp: CSV (semicolon), RDS, RData, and SAV (if `haven` is installed).

## Run

Deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

Run only mixed models deliberate tests:

```bash
bash tests/run_mixed_models_tests.sh
```

## Expected Outputs

- `outputs/test-runs/<timestamp>/mixed_models_test.log`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/report_canonical.md`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/analysis_log.jsonl`

## Notes

- The suite requires `lme4` and `performance`. It will fail fast if either is missing.
- `emmeans` and `lmerTest` are optional; checks adapt based on package availability.
- Kenward-Roger coverage requires `pbkrtest` when `lmerTest` is installed; otherwise the suite expects a controlled failure for that case.
- SAV input tests require `haven` to write the .sav file; otherwise they are skipped.