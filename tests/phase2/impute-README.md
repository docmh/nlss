# Imputation contract and independent values

Wave 13 retains simple mean/median/auto/mode/random/constant, mice and VIM kNN.
The runner and plan are registered in `tests/tests.yml`; run:

```bash
Rscript tests/phase2/run_impute_tests.R --root /tmp/nlss-impute-tests --keep 10
```

Use `--match` for focused development. `NLSS_TEST_ROOT` and `NLSS_KEEP_RUNS`
are supported. Each CLI command records its arguments, source identity,
timing and actual exit. Stateful shell harnesses require separate source copies.

`tests/values/impute_compute_golden.R` computes numerical expectations independently
from base R using the registered golden dataset. The Python checker compares
JSONL results with the checked-in CSV. Public mice/VIM calls supply separate
seeded reference draws/completions, not NLSS functions. Coverage includes donor
selection, all-missing constants, actual counts, factors, temporal storage,
labels/source provenance, collisions, flags/maps, invalid inputs, input formats,
configuration, templates and non-activating replay. Existing standalone module
feature tests and the Phase-1 imputation contract remain part of acceptance.

Shared publication checks live in `data_change_contract.R`; mids reference,
metadata, payload and tampering checks live in `imputation_artifact_contract.R`.
The Phase-2 core retains the separate MI-regression consumer and independent
Rubin-pooling comparisons. These checks do not establish the appropriateness
of a particular researcher's imputation model or missing-mechanism assumptions.

Independent package documentation: [R donor sampling](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html),
[mice mids fields](https://amices.org/mice/reference/mids.html),
[VIM kNN](https://statistikat.github.io/VIM/reference/kNN.html).
Acceptance uses the installed package versions recorded with each run, not an
assumption that those versions match the latest online documentation.
