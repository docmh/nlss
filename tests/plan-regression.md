# Regression Comprehensive Test Plan

## Scope

- Validate linear regression (OLS) and generalized linear models (binomial/logistic, Poisson) in `scripts/R/regression.R`.
- Exercise hierarchical blocks, interaction terms, centering, standardization, grouping, bootstrap CIs, and template rendering.
- Cover input variants (workspace manifest, RDS, RData, CSV with custom separator/header) and option overrides (link, conf-level, digits, logging).
- Verify logging behavior (`analysis_log.jsonl`) for both successful runs and invalid inputs.
- Cover edge cases (bootstrap samples = 0, small-group failures) and negative cases (missing/unknown variables, invalid DV types, invalid link).

## Data Sources

- `tests/data/golden_dataset.csv` (primary coverage; used to create `golden_dataset/golden_dataset.parquet`).
- `tests/data/regression_data.csv` (auto-IVs and RDS/RData fixtures).
- `tests/data/regression_semicolon.csv` (custom separator coverage).
- `tests/data/regression_no_header.csv` (header FALSE coverage).
- `tests/data/regression_group_edge.csv` (small-group invalid-input case).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/regression_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/regression_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/regression_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/regression_test.log`

## Coverage Matrix

Positive coverage:

- OLS regression with explicit predictors (CSV input path) plus workspace manifest run (no input flags).
- Auto-selected numeric predictors (no `--ivs`/`--blocks`).
- CSV parsing options (`--sep`, `--header FALSE`).
- RDS and RData input paths.
- Hierarchical regression with blocks and model comparisons (OLS and binomial).
- Effect size reporting (*f*²) and block change effect size (Delta *f*²) for OLS.
- Interaction terms with centering and appended interaction block (OLS and binomial).
- Grouped regressions across `site` levels (OLS and binomial).
- Standardized betas for OLS (`--standardize predictors`).
- Bootstrap confidence intervals with seed control and bootstrap edge case (0 samples).
- Logistic regression with numeric DV (`binary_outcome`), factor DV (two-level `group2`), custom link, and bootstrap.
- Poisson regression with block comparisons (count DV).
- Config overrides logged (`--conf-level`, `--digits`, `--user-prompt`).
- Template override check (config-driven regression template affects NLSS format output).
- Logging toggle (`--log FALSE`) prevents JSONL append.

Negative coverage:

- Missing `--dv`.
- DV not found.
- Unknown predictor variable.
- Unknown interaction variable.
- Invalid binomial link function.
- Non-numeric DV for gaussian regression.
- Non-numeric DV for poisson regression.
- Predictors reduced to empty set (IVs identical to DV).
- No complete cases (all-missing predictor).
- Unknown grouping variable.
- Group with <3 complete cases (small-group dataset).

## How to Run

```bash
bash tests/run_regression_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

The script asserts successful runs by validating new `analysis_log.jsonl` entries and checks negative cases by expecting failures with `results.status = invalid_input`.