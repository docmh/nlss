# Mixed Models Deliberate Test Plan

Goal: stress mixed models feature coverage with clean, edge, and negative cases for robust behavior.

## Scope

- Formula-based and dv/fixed/random model construction.
- Random term normalization and required random effects (including formula-only checks).
- Type I/II/III tests and df-method fallbacks (including Kenward-Roger when available).
- Standardization of predictors and standardized betas, plus invalid standardize fallbacks.
- Diagnostics toggle behavior plus max_shapiro_n gating (Shapiro present/absent).
- Optimizer/maxfun configuration (including invalid maxfun fallback).
- emmeans + contrasts + p-adjust and conf-level handling (including emmeans-only and contrasts-without-emmeans).
- Input formats: parquet, CSV (custom sep/header), RDS, RData (df selection), SAV when available.
- Template overrides for default and emmeans outputs (emmeans tests require the package).
- Logging controls (`--log FALSE`, `--digits`, `--user-prompt`) in automated coverage; interactive mode is manual-only.
- Negative cases for missing inputs, missing variables, nonnumeric dv, invalid formula, missing random effects, RDS non-data-frame, RData missing df, and no complete cases.

## Data

- Base: `outputs/tests/data/golden_dataset.csv`
- Long mixed models data: generated via `outputs/tests/mixed_models_prep.R` into the test run tmp folder.
- Format variants created in tmp: CSV (semicolon), RDS, RData, and SAV (if `haven` is installed).

## Run

Deliberate suite:

```bash
bash scripts/tests.sh deliberate
```

Run only mixed models deliberate tests:

```bash
bash outputs/tests/run_mixed_models_tests.sh
```

## Expected Outputs

- `outputs/test-runs/<timestamp>/mixed_models_test.log`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/apa_report.md`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/analysis_log.jsonl`

## Notes

- The suite requires `lme4`. It will fail fast if `lme4` is missing.
- `emmeans` and `lmerTest` are optional; checks adapt based on package availability.
- Kenward-Roger coverage requires `pbkrtest` when `lmerTest` is installed; otherwise the suite expects a controlled failure for that case.
- SAV input tests require `haven` to write the .sav file; otherwise they are skipped.
