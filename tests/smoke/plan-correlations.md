# Correlations Comprehensive Test Plan

## Scope

- Validate correlations (Pearson/Spearman), grouped correlations, and cross-correlations in `scripts/R/correlations.R`.
- Exercise bootstrap confidence intervals, Fisher r-to-z tests vs. r0, and Fisher r-to-z group comparisons.
- Check matrix-template output (r below diagonal, p above diagonal) and comparison-template output.
- Cover partial correlations with controls, p-adjust options, missing handling, input variants, and logging toggles.
- Numeric value tests: compare `analysis_log.jsonl` outputs against `tests/values/correlations_golden.csv`, `tests/values/correlations_diagnostics_golden.csv`, and `tests/values/correlations_comparison_golden.csv` (regenerate with `tests/values/correlations_compute_golden.R`).

## Data Sources

- `tests/data/golden_dataset.csv` (primary coverage; numeric vars plus group2/group3).
- Inline CSV fixtures created by `tests/smoke/run_correlations_tests.sh` (semicolon CSV, no header, non-numeric).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/correlations_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/correlations_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/correlations_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/correlations_test.log`

## Coverage Matrix

Positive coverage:

- Baseline CSV run with explicit vars (Pearson).
- Kendall correlations (nonparametric) for numeric pairs.
- Workspace run (no input flags) using active dataset.
- Cross-correlations via `--x`/`--y`, plus `--method spearman`, `--missing complete`, and `--p-adjust`.
- Grouped correlations for `group2` (two-level groups).
- Partial correlations with controls (Spearman).
- Bootstrap confidence intervals with seed control.
- Fisher r-to-z tests against r0.
- Compare-groups output with `group2` (comparison table and narrative).
- Matrix template via `--template matrix` with report check.
- Input variants: RDS, RData, and SAV (if `haven` or `foreign` is installed).
- Logging toggle (`--log FALSE`) to ensure no JSONL append.

Negative coverage:

- `--compare-groups TRUE` without `--group`.
- `--compare-groups TRUE` with `group3` (more than two levels).
- `--r0` missing a value and `--r0` out of range.
- Kendall method with controls, r0, or compare-groups (unsupported).
- Non-numeric variables and no numeric variables (CSV fixture).

## How to Run

```bash
bash tests/smoke/run_correlations_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

The script validates successful runs by checking new `analysis_log.jsonl` entries and report markers, and it asserts negative cases by expecting failures with no log changes.
