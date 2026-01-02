# Descriptive Stats Test Plan

## Scope

- Validate descriptive stats outputs with expanded metrics (robust, percentiles, outliers).
- Exercise new CLI flags (`--trim`, `--iqr-multiplier`, `--outlier-z`) and template selection.
- Confirm JSONL logging contains the extended columns and options.
- Cover negative inputs for trim/outlier thresholds.

## Data Sources

- `tests/data/golden_dataset.csv`

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/descriptive_stats_workspace`.
- Outputs (per dataset):
  - `outputs/test-runs/<timestamp>/descriptive_stats_workspace/<dataset-name>/report_canonical.md`
  - `outputs/test-runs/<timestamp>/descriptive_stats_workspace/<dataset-name>/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/descriptive_stats_test.log`

## Coverage Matrix

Positive coverage:

- Parquet workspace run with grouping, extended metrics, and `--user-prompt`.
- Template selection via `--template robust` and `--template distribution`.
- Template override via direct `--template <path>`.
- Workspace implicit run (no input flags) from a dataset folder.
- CSV input with `--sep` and `--header FALSE`.
- RDS, RData (`--df`), and optional SAV inputs.
- Interactive run via prompt file.
- JSONL log verification for new columns and options.
- Default variable selection coverage via `require_vars`.

Negative coverage:

- Trim proportion out of range.
- IQR multiplier <= 0.
- Outlier z threshold <= 0.
- Unknown variables.
- Invalid group variable.
- Non-numeric variables passed to `--vars`.
- No numeric variables available in the dataset.

Edge coverage:

- All-missing variable (`n = 0`, missing 100%).
- Single-observation variable (`n = 1`).
- Zero-mean variable (CV omitted).
- Tied mode (mode omitted).
- Tukey/z outlier counts with low z threshold.
- Grouping with NA group level.

## How to Run

```bash
bash tests/smoke/run_descriptive_stats_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```