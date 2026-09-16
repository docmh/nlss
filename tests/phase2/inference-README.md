# Phase 2 inference acceptance: t-tests and correlations

Run the public-CLI numerical and audit acceptance suite:

```bash
Rscript tests/phase2/run_inference_tests.R --root /tmp/nlss-inference-check --keep 0
Rscript tests/phase2/run_inference_tests.R --root /tmp/nlss-ttest-check --keep 0 --modules t_test
Rscript tests/phase2/run_inference_tests.R --root /tmp/nlss-correlation-check --keep 0 --modules correlations
```

The canonical runner, suite registration and plan paths live in
[`tests/tests.yml`](../tests.yml). The runner honors `NLSS_TEST_ROOT`,
`NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`. Dependencies are `yaml`, `jsonlite`,
`arrow`, `digest` and `haven`; no additional inference package is required.
It creates private test projects, configuration, RDS/SAV fixtures and templates.
It does not change the installed configuration/templates and needs no network.
Run it against an unchanged repository or a private frozen copy: changing NLSS R
code during a run correctly invalidates exact replay.
For focused diagnosis, `--match REGEX` selects test-group names; the selection is
recorded in `results.json`, and an empty selection is an error. Filtered runs are
not evidence that the full suite passed. Empty or malformed regular expressions
are rejected; omitting `--match` retains the complete suite.

## Bounded smoke selection

Smoke uses the exact, anchored `tests.suites.smoke.inference_match` selector in
[`tests/tests.yml`](../tests.yml): **13 of the existing 83 groups**. No test IDs,
statistical assertions, tolerances or default selections were changed.

| Selected groups | Coverage |
| --- | --- |
| 3 t-test groups | Two-sided one-sample, independent and paired tests; exact source cases and replay |
| 2 additional t-test groups | Seeded paired bootstrap and SAV labels/user missings/group ordering; replay |
| 3 correlation groups | Two-sided Pearson, Spearman and Kendall values/diagnostics; replay |
| 1 partial-correlation group | Rank controls, complete-case selection and seeded bootstrap; replay |
| 1 Fisher-comparison group | Pearson correlations between independent groups |
| 3 contract/import/presentation groups | Configured controls frozen in replay, rounded/adjusted matrix output, and SAV labels/user missings |

The other 70 groups remain in the complete `phase2`/`all` execution, including
all alternative tails, pooled variance, the other bootstrap combinations,
all multiplicity methods, redundant controls, rank-test policy boundaries,
group-identity collisions, privacy/template/input replay and failure states.
The complete suite remains the numerical acceptance requirement; smoke does
not rerun it in full or count its subset as independent additional coverage.

## Independent numerical evidence

Every analysis is invoked through its real `Rscript` CLI. Assertions read the
published, unrounded `result.json`, saved request and generated Markdown. No NLSS
calculation function or golden generator is sourced. Numerical expectations are
computed from deterministic synthetic data and independent R primitives:

- T-tests: all three modes and alternatives, a nonzero one-sample null, Welch and
  pooled-variance tests, means/SDs/sample sizes, differences, degrees of freedom,
  t, p, native and null-relative intervals, and standardized effects. Native
  inference is compared with `stats::t.test`; independent d uses an auxiliary
  regression residual SD, and paired d uses the SD of complete paired differences.
  [R t-test documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/t.test.html)
  defines the native alternative-aware interval and variance policies.
- Correlations: Pearson, Spearman and Kendall, all alternatives, complete and
  pairwise cases, cross-set pair deduplication, ties and small-sample exact-policy
  behavior. Ordinary coefficients, tests and Pearson intervals are compared with
  `stats::cor.test`; its automatic rank-test policy is preserved, not replaced by
  an assumed normal approximation. [R correlation-test documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html)
  distinguishes exact tests, AS89/Edgeworth and asymptotic inference.
- Partial correlations: a QR projection independently gives residual
  correlations; a separate `stats::lm` slope test supplies p and residual df.
  Redundant and constant controls test effective rank, not the number of named
  columns. Spearman ranks all participating variables before residualization.
  Fisher intervals use the documented effective-rank standard error.
- Fisher comparisons: against a specified correlation and between independent
  groups, including alternatives and Spearman approximation paths.
- Multiplicity: all eight exposed `stats::p.adjust.methods`, independently
  adjusted within each group, including actual missing and literal `NA` groups.
  [R p-adjust documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html)
  is the reference for these distinct correction families.
- Diagnostics: Shapiro-Wilk and variance tests directly from `stats`; skewness
  and excess kurtosis from standardized central moments with finite-sample
  corrections. Their arithmetic is independent of the NLSS implementations.
- Bootstrap: exact seed-controlled resampling comparisons for every t-test mode,
  all correlation methods, partial ranks and alternative-tail percentile
  intervals. Resamples use the documented row/sample protocol and base R
  statistics rather than NLSS helpers. This checks implementation and replay,
  not empirical coverage of a confidence-interval method.

General numerical comparisons use relative-scaled tolerance `3e-8` with scale at
least one. Probability comparisons instead use relative tolerance `2e-7` and an
absolute floor of `1e-300`; replacing a small positive p-value by zero cannot pass
merely because both lie below an ordinary absolute tolerance. Rounded Markdown
is checked at the requested precision, including adjusted matrix p-values.

## Import, execution and replay evidence

The suite covers SPSS variable/value labels and user-missing normalization,
first-observed t-test group ordering, missing group identity, deliberately
requested factor/text coercion, exact source-row case selection, resolved
configuration controls and deterministic default seeds.
Numeric groups separated by `1e-15` and timestamps separated by fractional
seconds specifically test that display formatting cannot merge scientific
groups. Their raw identities and run-local group IDs independently determine
sample selection, multiplicity families, Fisher comparisons and matrix cells;
human-readable labels must remain distinguishable.

Successful public runs must publish one completed bundle with matching request,
input, dictionary, template and artifact hashes. Replay must preserve the raw
results and Markdown. Privacy tests disable legacy logging and legacy output
inclusion, then change the current data, original source, original template and
configuration before replay. Mandatory results remain available, private prompts
remain excluded, and replay must not overwrite the edited working dataset.

Negative tests cover unsupported methods and alternatives, numeric option
domains, duplicate roles, contradictory selections, constant/nonestimable data
and the legacy expected-two-groups convention. A failed resolved analysis must
remain a failed bundle, publish no normal output, preserve earlier canonical
output, release locks, and be rejected by replay. Correlations with one valid
pair may retain explicitly unavailable other pairs; an all-unavailable request
must fail. Perfect correlations and samples too short for an interval are
separate boundary cases.

The runner writes `phase2-inference/run-*/results.json`, per-command logs and
private project artifacts below its test root. It reports both consolidated test
groups and individual numerical comparisons. A group can contain multiple CLI
invocations; these counts are not interchangeable.

## Limits

These tests establish deterministic implementation agreement and the migrated
[run contract](../../references/run-contract.md) on the tested R environment.
They do not validate a study design, guarantee test assumptions or bootstrap
coverage, add categorical-control dummy coding, or supply a semantic research
report. Interactive human prompting and native Windows execution require
separate platform tests. Existing legacy numerical/module and full smoke suites
remain complementary regression evidence.
