# Power Subskill Test Plan

## Scope

- Validate power analysis in `scripts/R/power.R` for t-tests, ANOVA, correlations, regression, and SEM (RMSEA).
- Cover a priori, post hoc, and sensitivity modes.
- Exercise effect-size inputs, effect estimation from data, and conversions (eta2 -> f, r2 -> *f*²).
- Verify `analysis_log.jsonl` logging for both successful runs and invalid inputs.

## Data Sources

- `tests/data/golden_dataset.csv` (primary coverage; used to create `golden_dataset/golden_dataset.parquet`).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/power_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/power_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/power_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/power_test.log`

## Coverage Matrix

Positive coverage:

- CSV input with explicit `--sep`/`--header`, plus RDS/RData inputs.
- SAV input when `haven` is installed.
- Interactive run via `NLSS_PROMPT_FILE`.
- t-test a priori (effect size input).
- t-test a priori with `--estimate-effect TRUE` (two-sample, paired, one-sample).
- t-test post hoc with `--n1/--n2`, `--n-total`, and `--n-per-group` + `--ratio`.
- t-test sensitivity (one-sample and two-sample).
- ANOVA post hoc with `--n-per-group` and with `--n` (inferred n/group).
- ANOVA a priori with `--effect-metric eta2` (conversion to f).
- ANOVA sensitivity and effect estimation.
- Correlation sensitivity with `--n` + `--power`.
- Correlation post hoc with `--n` and alternative `less`.
- Correlation effect estimation.
- Regression post hoc with `--effect-metric r2`.
- Regression sensitivity with `--n` (detectable *f*²).
- Regression a priori with `--effect-metric f2`.
- Regression effect estimation.
- SEM a priori, post hoc, and sensitivity when `semPower` is installed.
- Template override via `--template`, digits formatting via `--digits`, `--user-prompt` logging, and `--log FALSE`.

Prereqs:

- R package `pwr` installed.
- R package `semPower` installed for SEM power coverage.

Negative coverage:

- Missing effect size for a priori t-test (expect `invalid_input`).
- Unsupported effect metric for analysis.
- Missing `--n` for correlation post hoc.
- Missing paired variables for t-test effect estimation.
- Missing group for ANOVA effect estimation.
- Missing x/y for correlation effect estimation.
- Missing ivs for regression effect estimation.
- Invalid `--u` for regression (<= 0).
- ANOVA with `--groups < 2`.
- Missing `--n` for post hoc t-test.
- Regression with sample size too small.
- SEM `--estimate-effect`, missing `--df`, and invalid `--rmsea1` (when `semPower` is installed).

## How to Run

```bash
bash tests/run_power_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```