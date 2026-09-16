# Phase 2 Power scientific acceptance

`tests/tests.yml` registers `run_power_tests.R` as `tests.scripts.phase2_power_r`.
The public-CLI runner uses private configuration and seeded synthetic fixtures;
it never sources NLSS statistical functions or reads historical goldens as its
oracle. Dependencies are `pwr`, `semPower`, `yaml`, `jsonlite`, `digest`, `haven`
and `arrow`. No network access is needed to run it.

```bash
Rscript tests/phase2/run_power_tests.R --root /tmp/nlss-power-check --keep 0
Rscript tests/phase2/run_power_tests.R --root /tmp/nlss-power-smoke --keep 0 --match smoke
bash cmdscripts/tests.sh phase2 --root /tmp/nlss-phase2-check --keep 0
```

`NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and `NLSS_TESTS_CONFIG` are honored. Results,
individual process logs and source/package hashes are saved below
`<root>/phase2-power/run-<timestamp>-<pid>/`. A forced root is never pruned.
Do not interpret a development run over changing source as final acceptance.

## Scientific oracles and coverage

- Direct `pwr` calls cover all three t-test designs, all three calculation modes,
  all alternatives, signed opposite-direction post-hoc effects, unequal allocation,
  nondefault alpha/power, and attained power at actual integer sample sizes.
- Direct `pwr.anova.test`, `pwr.r.test` and `pwr.f2.test` cover every supported mode
  and effect conversion, with observed group count and regression model rank checked
  independently. See the [author-maintained pwr manual](https://cran.r-project.org/web/packages/pwr/pwr.pdf).
- RMSEA references are computed independently from noncentral chi-square
  distributions: for the supported single-sample model,
  `lambda = (N - 1) * df * RMSEA^2`. The critical value uses the requested null
  noncentrality, and rejection probability uses the alternative noncentrality.
  Upper-tail close-fit and lower-tail not-close-fit tests are distinct; sensitivity
  solves the upper-tail alternative. This matches the
  [semTools author documentation](https://rdrr.io/cran/semTools/man/findRMSEApower.html)
  and its [documented formula and source](https://rdrr.io/cran/semTools/src/R/powerAnalysisRMSEA.R).
  Exact-fit (`RMSEA0 = 0`) cases also compare directly with
  `semPower::semPower.postHoc`; that single-effect package call is not used as the
  oracle for a nonzero RMSEA null.
- Data-estimated references come from independent `t.test`, `cor` and `lm`
  fits. Tests verify actual included source rows, signed effects, paired cases,
  unequal groups, unused factor levels, factor expansion and aliased predictors.
  SAV/RDS variable labels and SPSS user-missing codes are tested, as are plain
  CSV and Parquet routes. Estimation never fabricates a parameter-only dataset.
- Integer allocation differences and ANOVA's balanced-design approximation must
  be disclosed. Invalid domains, conflicting designs, non-finite observations,
  constant variables and nonnumeric scores must fail instead of silently changing
  the scientific request.

Numeric tolerances are relative/absolute `3e-6` by default; probabilities use
relative tolerance, including extremely small opposite-tail power. Source-row
identities use exact comparisons. The separate
[planning boundary suite](planning-README.md) owns deeper publication/replay,
project preservation and frozen-request integrity checks.

## Corrected legacy goldens

The independent generator `tests/values/power_compute_golden.R` preserves 19
case IDs while removing copied adapter logic. The following historical expected
values encoded scientific defects and are deliberately corrected:

| Case | Historical expectation | Corrected expectation |
| --- | --- | --- |
| Estimated ANOVA | 2 groups, 9 per group, N = 18 | Observed 3 groups, 8 per group, N = 24 |
| SEM a priori: df 120, RMSEA0 .05, RMSEA1 .08, alpha .05, power .8 | N = 58 | N = 117 |
| Same SEM, post hoc N = 200 | Power .9999998261 | Power .9776459375 |
| Same SEM, sensitivity N = 120, target .8 | RMSEA1 .05545739 | RMSEA1 .07939309 |

The SEM correction uses the null the user actually supplied instead of silently
testing exact fit. The other 15 standard cases retain their numeric values.
Expanded signed-effect tests cover errors that the old positive-effect fixtures
could not expose. Recompute affected historical analyses; reproducing the old
wrong number is not a correctness requirement.

```bash
Rscript tests/values/power_compute_golden.R \
  --data tests/data/golden_dataset.csv --out tests/values/power_golden.csv
NLSS_TEST_ROOT=/tmp/nlss-power-legacy NLSS_KEEP_RUNS=0 bash tests/smoke/run_power_tests.sh
```

Goldens support numerical acceptance, not a claim that observed post-hoc power
justifies a study or that assumed effects are valid population parameters.
Deterministic output remains evidence for a context-sensitive final report.
