# Regression Comprehensive Test Plan

## Scope

- Validate linear regression (OLS) and generalized linear models (binomial/logistic, Poisson) in `core-stats/scripts/R/regression.R`.
- Exercise hierarchical blocks, interaction terms, centering, standardization, grouping, bootstrap CIs, and template rendering.
- Verify logging behavior (`analysis_log.jsonl`) for both successful runs and invalid inputs.
- Cover edge cases (missingness-driven zero complete cases) and negative cases (missing/unknown variables, invalid DV types).

## Data Sources

- `outputs/tests/golden_dataset.csv` (primary coverage; used to create `golden_dataset/golden_dataset.parquet`).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/regression_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/regression_workspace/golden_dataset/apa_report.md`
  - `outputs/test-runs/<timestamp>/regression_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/regression_test.log`

## Coverage Matrix

Positive coverage:
- OLS regression with explicit predictors (CSV input path).
- Hierarchical regression with blocks and model comparisons.
- Interaction terms with centering and appended interaction block.
- Grouped regressions across `site` levels.
- Standardized betas for OLS (`--standardize predictors`).
- Bootstrap confidence intervals with seed control.
- Logistic regression with numeric DV (`binary_outcome`).
- Logistic regression with factor DV (two-level `group2`).
- Poisson regression with count DV (`count_outcome`).
- Template mutation check (edited regression template affects APA output).

Negative coverage:
- Missing `--dv`.
- Unknown predictor variable.
- Unknown interaction variable.
- Non-numeric DV for gaussian regression.
- Predictors reduced to empty set (IVs identical to DV).
- No complete cases (all-missing predictor).
- Unknown grouping variable.

## How To Run

```bash
bash outputs/tests/run_regression_tests.sh
```

Or run the full deliberate suite:

```bash
bash scripts/tests.sh deliberate
```

The script asserts successful runs by validating new `analysis_log.jsonl` entries and checks negative cases by expecting failures with `results.status = invalid_input`.
