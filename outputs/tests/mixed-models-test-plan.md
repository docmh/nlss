# Mixed Models Deliberate Test Plan

Goal: stress mixed models feature coverage with clean, edge, and negative cases for robust behavior.

## Scope

- Formula-based and dv/fixed/random model construction.
- Random term normalization and required random effects.
- Type I/II/III tests and df-method fallbacks.
- Standardization of predictors and standardized betas.
- Diagnostics toggle behavior.
- Optimizer/maxfun configuration.
- emmeans + contrasts + p-adjust and conf-level handling.
- Negative cases for missing inputs, missing variables, nonnumeric dv, and no complete cases.

## Data

- Base: `outputs/tests/golden_dataset.csv`
- Long mixed models data: generated via `outputs/tests/mixed_models_prep.R` into the test run tmp folder.

## Run

Deliberate suite:

```bash
bash scripts/tests.sh deliberate
```

Run only mixed models deliberate tests:

```bash
bash outputs/tests/run_mixed_models_tests.sh
```

Run only mixed models template tests:

```bash
bash outputs/tests/mixed_models_template_tests.sh
```

## Expected Outputs

- `outputs/test-runs/<timestamp>/mixed_models_test.log`
- `outputs/test-runs/<timestamp>/mixed_models_template_tests.log`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/apa_report.md`
- `outputs/test-runs/<timestamp>/mixed_models_workspace/mixed_models_long/analysis_log.jsonl`

## Notes

- The suite requires `lme4`. It will fail fast if `lme4` is missing.
- `emmeans` and `lmerTest` are optional; checks adapt based on package availability.
