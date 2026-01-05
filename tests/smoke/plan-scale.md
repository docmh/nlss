# Scale Comprehensive Test Plan

## Scope

- Validate scale item statistics, item-total correlations, alpha-if-deleted, and reliability summary outputs in `scripts/R/scale.R`.
- Exercise reverse scoring, missing handling (pairwise vs. complete), score method (sum vs. mean), omega on/off, and grouped analyses.
- Validate output templates for scale and logging behavior.
- Numeric value tests: compare `analysis_log.jsonl` outputs against `tests/values/scale_item_golden.csv` and `tests/values/scale_reliability_golden.csv` (regenerate with `tests/values/scale_compute_golden.R`).

## Data Sources

- `tests/data/golden_dataset.csv` (primary coverage; f1 and f2 item sets plus group2).
- Inline CSV fixtures created by `tests/smoke/run_scale_tests.sh` (reverse scoring, missingness, coercion, omega error paths, template override).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/scale_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/scale_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/scale_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/scale_test.log`

## Coverage Matrix

Positive coverage:

- Baseline scale (f1 items) with reverse scoring and omega enabled.
- Missing handling: complete-case run on f2 items with reverse scoring.
- Grouped scale by `group2` with omega disabled and default sum scoring.
- Direct workspace run (no input flags).
- Input variants: CSV, RDS, RData, and SAV (if `haven` is installed).
- Template override and report marker checks.

Negative/edge coverage:

- Unknown variables/reverse items/group errors.
- Non-numeric items without `--coerce`.
- Coercion paths (factor, logical, date types).
- Omega status branches: insufficient items, insufficient n, correlation missing, factanal failure.
- Logging toggle (`--log FALSE`) and append behavior.

## How to Run

```bash
bash tests/smoke/run_scale_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

The script validates successful runs by checking new `analysis_log.jsonl` entries, report markers, and golden-value comparisons for item and reliability tables.
