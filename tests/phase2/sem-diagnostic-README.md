# Standalone SEM diagnostic acceptance

`run_sem_diagnostic_tests.R` exercises the public SEM branch of `assumptions.R`
in private projects. It compares published values to independent `MVN`, `lavaan`
and base-R calls, without sourcing NLSS statistical functions or reading goldens.
This remains a Legacy diagnostic refit, not an immutable-run/replay adapter.

```bash
Rscript tests/phase2/run_sem_diagnostic_tests.R --root /tmp/nlss-sem-diagnostics --keep 0
Rscript tests/phase2/run_sem_diagnostic_tests.R --root /tmp/nlss-sem-diagnostic-smoke --keep 0 --match '_smoke$'
```

The suite covers raw Mardia probabilities, Shapiro, Mahalanobis source-row
distances, correlation/condition screens, actual FIML multigroup cases, genuine
SAV labels and user missings, ordinal code ordering, skipped/unavailable/disabled
checks, converged negative-variance solutions, seeded bootstrap metadata,
single-fit invariance scope, exact option-name handling, invalid requests and
protection of previous report/log bytes. Selected infinities fail; unrelated
columns cannot change the model. The three `_smoke` groups are already included
in full acceptance, not additional independent cases.

Finite numeric comparisons use scaled `2e-7` tolerance; probabilities use relative
`2e-7` with a `1e-300` floor. Missing patterns and source-case identities are
checked separately. A nonconverged fixture is not evidence for an available
Heywood count: the negative-variance case explicitly requires convergence in
the independent reference. Source/runner hashes, package versions, per-case
counts, CLI logs and `results.json` remain under the selected root.

The runner reads `tests/tests.yml` or `NLSS_TESTS_CONFIG`, honors `NLSS_TEST_ROOT`
and `NLSS_KEEP_RUNS`, uses private configuration, and rejects empty selections.
It requires installed yaml/jsonlite/digest/arrow/haven/lavaan/MVN; it installs
and downloads nothing. Full acceptance is registered in `phase2`/`all`.
Other assumptions families and free semantic final reports are outside its scope.
