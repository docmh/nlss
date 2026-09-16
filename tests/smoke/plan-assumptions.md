# Assumptions Deliberate Test Plan

Goal: exercise assumptions coverage across t-tests, ANOVA, regression, mixed models, and SEM with clean, edge, and negative cases.

## Scope

- Cover analysis families: `ttest`, `anova`, `regression`, `mixed_models`, `sem` (including cfa/path/mediation/invariance routes).
- Exercise input modes (csv/parquet/rds/rdata, sav when available) plus help and interactive prompts.
- Validate normality/homogeneity behavior, regression diagnostics (linearity, homoscedasticity, VIF, Durbin-Watson, outliers, influence), mixed-model diagnostics (random-effects normality, singularity, convergence, DHARMa/performance when available), and SEM diagnostics (Mardia, Mahalanobis, collinearity, Heywood, convergence).
- Confirm numeric thresholds and validated option domains (alpha, VIF thresholds, outlier/Cook, max_shapiro_n, SEM thresholds, maxfun/reml/optimizer); negative maxfun is rejected rather than silently reset.
- Validate auto-analysis routing and logging behaviors (`--log`, `--user-prompt`).
- Template override checks for all assumptions templates (ttest/anova/regression/mixed_models/sem).
- Negative cases for invalid inputs (missing variables, invalid group levels, invalid model specs, missing random effects, invalid SEM inputs).
- Numeric value tests: compare `analysis_log.jsonl` outputs against all 33 registered rows in `tests/values/assumptions_golden.csv` when the reference packages are installed (regenerate with `tests/values/assumptions_compute_golden.R` when datasets or methods change). Existing 31 case IDs remain; repaired performance/influence paths add two formerly absent golden rows.

## Prereqs

- Run from repo root.
- Rscript available.
- R packages installed: `arrow`, `yaml`, `jsonlite`, `lme4`, `lavaan`.
- Optional: `haven` for `.sav` input coverage; `performance`, `DHARMa`, `influence.ME`, `MVN` for optional diagnostics.
- Interactive coverage uses `NLSS_PROMPT_FILE` to feed prompts during automation.

## Run

Deliberate suite:

```bash
bash cmdscripts/tests.sh deliberate
```

Run only assumptions deliberate tests:

```bash
bash tests/smoke/run_assumptions_tests.sh
```

## Expected Outputs

- `outputs/test-runs/<timestamp>/assumptions_test.log`
- `outputs/test-runs/<timestamp>/assumptions_workspace/golden_dataset/analysis_log.jsonl`
- `outputs/test-runs/<timestamp>/assumptions_workspace/golden_dataset/report_canonical.md`
- Separate format datasets `golden_dataset_semicolon`, `golden_dataset_rds`,
  `golden_dataset_rdata`, and (when available) `golden_dataset_sav`, each with
  its own `analysis_log.jsonl` and `report_canonical.md`.
- `outputs/test-runs/<timestamp>/assumptions_workspace/mixed_models_long/analysis_log.jsonl`
- `outputs/test-runs/<timestamp>/assumptions_workspace/mixed_models_long/report_canonical.md`
- Dataset-local `runs/<id>/request.json`, `result.json`, frozen templates and
  `output.md` for completed diagnostic runs. Failed resolved requests have a
  terminal failed bundle and no normal Markdown output.

## Notes

- The suite requires `lme4` and `lavaan`; it fails fast if either is missing.
- SAV input tests require `haven` to write the `.sav` file; otherwise they are skipped.
- Performance/influence numerical goldens are required when their packages are
  installed; a missing golden row is a test failure, not a skip. When these
  packages or MVN are absent, requested diagnostics must have explicit
  `unavailable` status, an explanation and no fabricated p-value or decision.
- Seeded DHARMa output/replay, full missing-package conditions and independent
  numeric coverage live in the registered Phase 2 diagnostic/assumptions suites;
  this legacy runner does not supply DHARMa numerical goldens.
- Format fixtures use distinct source basenames and matching log paths. The
  import contract correctly refuses to bind different source files to the same
  dataset name; these tests must not bypass that protection. CSV and its
  registered working Parquet still share the original dataset as intended.
  The selected RData object is also named `golden_dataset_rdata`, because RData
  dataset identity follows that object rather than the file stem alone.
- Template overrides use a private copy at `tmp/assumptions/config.yml` via
  `NLSS_CONFIG_PATH`; the suite does not edit or restore the repository config.
- All existing positive/negative and format cases remain. Wave 9 updates the
  scientifically corrected expectations: regression curvature uses the nested
  `Quadratic added-term F` test (with both degrees of freedom), and repeated
  ANOVA uses `mauchly.test(..., X = ~1)` for within-subject contrasts. Two repeated
  levels produce a `skipped` Mauchly row explaining automatic sphericity, not an
  absent check. Exceeding the requested Shapiro limit is likewise explicitly
  `skipped` rather than a normality verdict.
- The valid mixed-model golden case requests the configured positive maxfun;
  the former negative value is retained as a separate expected failure. The
  invariance-model positive case now names its group; the former groupless call
  remains as a separate expected failure. This is a constrained single-fit
  diagnostic check, not a sequence of invariance model comparisons.
- Expected failures require exactly one new terminal failed bundle, matching
  request hash and run identity, no normal `output.md`, a released analysis lock,
  and byte-identical previous report/log projections. A completed run must never
  be accepted as an expected failure. `--log FALSE` leaves the legacy JSONL
  byte-identical but still publishes its mandatory completed JSON/Markdown bundle.
- The golden checker validates all six numeric fields per row, including exact
  JSON nulls where the golden is NA; it rejects missing fields, malformed logs,
  ambiguous row matches and nonfinite/nonnumeric values. Ordinary numeric
  tolerance remains relative/absolute `1e-6`; p-values use relative `2e-7` with
  absolute floor `1e-300`, so a small positive probability cannot silently
  become zero. Every golden diagnostic must have `available` status.
- The separate Phase 2 mixed, SEM and standalone assumptions suites remain
  independent acceptance evidence. Real-terminal prompting and native Windows
  require their own platform checks; `NLSS_PROMPT_FILE` does not establish them.
