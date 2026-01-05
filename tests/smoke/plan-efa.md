# EFA Comprehensive Test Plan

## Scope

- Validate exploratory factor analysis / PCA in `scripts/R/efa.R`.
- Exercise extraction methods, rotations, factor retention rules, and correlation types.
- Cover input formats, templates, logging, labels, and interactive prompts.
- Verify sorted loadings and NLSS format outputs.
- Validate numeric outputs against golden value files (summary/loadings/eigen).

## Data Sources

- `tests/data/golden_dataset.csv` (factor items f1_* and f2_*).
- Generated CSV fixtures under the run root:
  - semicolon delimiter, no header, polychoric (ordinal), tetrachoric (binary), tetrachoric invalid, nonnumeric coerce, template, interactive.
- Labeled RDS/RData/SAV created by `tests/smoke/prepare_labeled_dataset.R`.
- Value-test goldens under `tests/values/` (summary/loadings/eigen).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/efa_workspace`.
- Outputs:
  - `outputs/test-runs/<timestamp>/efa_workspace/golden_dataset/report_canonical.md`
  - `outputs/test-runs/<timestamp>/efa_workspace/golden_dataset/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/efa_test.log`

## Coverage Matrix

Positive coverage:

- Inputs: parquet (workspace copy), CSV (sep/header variants), RDS, RData, SAV (if haven), interactive.
- Methods: pca, pa, minres, uls, ml, gls, wls, alpha.
- Rotations: varimax, none, oblimin.
- Factor rules: eigen > 1 default, fixed `--n-factors`, eigen threshold override.
- Correlations: pearson, spearman, polychoric, tetrachoric.
- Missing handling: complete default, pairwise.
- Flags: `--loading-cutoff`, `--sort-loadings` (sorted check), `--coerce`, `--digits`, `--user-prompt`, `--log FALSE`, `--template` path override, `--vars` default selection.
- Labels: variable and group value labels rendered in NLSS format output.

Negative coverage:

- Missing vars, missing group, nonnumeric with coerce FALSE, tetrachoric with non-binary inputs, RData missing `--df`.

## How to Run

```bash
bash tests/smoke/run_efa_tests.sh
```

Or run the full deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```
