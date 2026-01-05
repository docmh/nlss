# Crosstabs Test Plan

## Scope

- Validate cross-tabulation output in `scripts/R/crosstabs.R`.
- Verify chi²/Fisher tests and effect-size outputs (phi, Cramer's V, contingency coefficient).
- Exercise NLSS format template output (default/grouped) and logging (`analysis_log.jsonl`).
- Run value-based golden checks for cells, tests, and diagnostics to validate numeric correctness.

## Data Sources

- `tests/data/golden_dataset.csv` (primary coverage).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/crosstabs_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/crosstabs_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/crosstabs_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/crosstabs_test.log`

## Coverage Matrix

Positive coverage:

- 3x3 table (`gender` x `group3`): expect Cramer's V and contingency coefficient in logs and NLSS format output.
- 2x2 table (`binary_outcome` x `group2`): expect phi (and Cramer's V), Fisher's exact test outputs, and Yates correction.
- Grouped table (`cat_var` x `cat_var2` by `group2`): validate grouped results and diagnostics.
- NLSS format output includes effect-size columns when values are present.
- Golden checks cover cells (counts/percents/residuals), tests (chi²/Fisher/effect sizes), and diagnostics (expected counts).

Negative coverage:

- Covered implicitly by smoke tests (invalid inputs are exercised in other suites).

## How to Run

```bash
bash tests/smoke/run_crosstabs_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```
