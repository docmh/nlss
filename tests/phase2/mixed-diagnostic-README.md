# Standalone mixed-model diagnostic acceptance

`run_mixed_diagnostic_tests.R` exercises only the **mixed-model branch** of the
public `assumptions.R` CLI. This suite checks the retained `analysis_log.jsonl`
and canonical Markdown compatibility projections. It does not itself establish
immutable-run or replay behavior; the Wave 9 [full-module acceptance suite](assumptions-README.md)
adds that separate coverage for all five diagnostic families.

```bash
Rscript tests/phase2/run_mixed_diagnostic_tests.R --root /tmp/nlss-mixed-diagnostic-check --keep 0
Rscript tests/phase2/run_mixed_diagnostic_tests.R --root /tmp/nlss-mixed-diagnostic-smoke --keep 0 --match smoke
```

The full runner has nine groups. Three existing groups form the smoke subset:
optimizer-only failure, independent package diagnostics, and singularity with
an unavailable random-effects normality test. `--match REGEX` is a diagnostic
selection, not a replacement for the full Phase 2 check; an empty selection
fails. The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and
`NLSS_TESTS_CONFIG`. With no explicit root, retention applies only to this
suite's own generated run directories; `--keep 0` preserves all artifacts.

Dependencies are the installed `yaml`, `jsonlite`, `arrow`, `digest`, `lme4`,
`performance`, `influence.ME`, and (for the selected simulation case) `DHARMa` packages. No downloads, installation,
NLSS statistical-function sourcing, or historical numeric goldens are used.

## Independent scientific and output checks

- `sleepstudy`, `bobyqa`, and an 80-evaluation limit isolate an optimizer return
  code of 1 with no lme4 gradient/Hessian message. The published result must not
  say convergence passed. The reference explicitly fails as a **fixture
  failure** if a future package no longer exhibits this boundary.
- `nloptwrap` uses its actual `maxeval` control, compared with an independent
  `lmer()` call using that control. Requested/effective controls, return codes,
  evaluation counts and optimizer messages are checked, including Markdown.
- `nlminbwrap` retains `maxfun`, which its wrapper maps to `eval.max`; a
  one-evaluation fixture prevents mistakenly substituting an iteration limit.
- An exact zero random-intercept variance fixture converges successfully but
  is singular. These are distinct findings. Its all-zero estimated random
  intercepts also make `shapiro.test()` fail: the requested check must remain
  visibly unavailable, with a reason and no fabricated p-value.
- `performance::check_heteroscedasticity()` returns a classed numeric p-value;
  the test verifies that it survives publication. Independent
  `influence.ME::influence()` plus the `stats::cooks.distance()` generic provide
  all cluster distances, maximum, denominator, threshold and flagged count.
  These result rows must appear in Markdown, not merely in an enabled-options
  list. The CLI subprocess starts clean, so reference-process package
  attachment cannot conceal the module's function-local refit boundary.
- Configured Shapiro limits retain a skipped row with the real n and reason.
  Disabled checks remain absent, preserving legacy behavior, while metadata
  records their disabled requests. Neither may be represented as success.
- DHARMa uniformity and dispersion are compared to an independent explicit
  `seed=123`, `n=250`, `refit=FALSE` simulation. The reference first calls
  `set.seed(123)`, matching the shared run boundary's initialized RNG protocol.
  Without an existing `.Random.seed`, DHARMa consumes an additional `runif(1)`
  despite the explicit seed argument; relying on that nullable global state
  changes results. No tolerance or numerical assertion was relaxed. These
  effective defaults must be recorded; this case is not itself a replay test.
- Shuffled cases with missing responses, covariates and group IDs verify the
  actual included/excluded source rows, residual n and complete cluster
  partitions. Labels cannot replace original case identity.

Finite numbers use a scaled tolerance of `2e-7`. Probabilities use a relative
`2e-7` tolerance with a `1e-300` floor; small nonzero probabilities cannot pass
by being rounded to zero. Integer counts and optimizer controls are exact.
Fixtures, command lines, CLI logs, legacy output and a machine-readable
`results.json` remain under the selected private run directory.

These checks complement the migrated LMM suite; they do not broaden testing to
the t-test, ANOVA, regression or SEM branches of assumptions, impose a final
research-report template, or assert that a flagged assumption invalidates a
scientific model automatically.

Primary documentation: [lme4 convergence and optimizer controls](https://lme4.github.io/lme4/reference/convergence.html),
[singular fits](https://lme4.github.io/lme4/reference/isSingular.html), and
[performance heteroscedasticity return value](https://easystats.github.io/performance/reference/check_heteroscedasticity.html).
