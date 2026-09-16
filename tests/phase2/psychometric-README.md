# Phase 2 scale and reliability acceptance

This offline public-CLI suite checks `scale.R` and `reliability.R` without
sourcing their calculation functions or any NLSS statistical helper. It uses
base R, `stats`, and the installed **test-only** package `psych` as independent
numerical references; `psych` is not a new analysis runtime dependency.

```bash
Rscript tests/phase2/run_psychometric_tests.R --root /tmp/nlss-psychometric --keep 0
Rscript tests/phase2/run_psychometric_tests.R --modules scale
Rscript tests/phase2/run_psychometric_tests.R --modules reliability
```

The normal Phase 2 harness resolves this suite through `tests/tests.yml`.
`--root`, `--keep`, `--modules`, `--match REGEX`, `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and
`NLSS_TESTS_CONFIG` follow the other Phase 2 runners. Each invocation creates
isolated projects and private `NLSS_CONFIG_PATH` YAML; installed canonical
configuration is never edited. Explicit roots are never pruned. `results.json`
records each consolidated test group, full assertion failures, numerical check
counts, the selected pattern, elapsed time, and R/package versions. An assertion
failure or a pattern matching no tests exits nonzero; an empty or malformed
regular expression is rejected. Omitting `--match` retains the complete suite.

## Bounded smoke selection

Smoke uses the exact, anchored `tests.suites.smoke.psychometric_match` selector
in [`tests/tests.yml`](../tests.yml): **8 of the existing 34 groups**. Test IDs,
test bodies, numerical tolerances and the default complete selection are unchanged.

| Selected groups | Coverage |
| --- | --- |
| 2 scale groups | Pairwise sum/reversal/alpha/omega and SAV variable/value labels with user missings; replay |
| 2 ICC groups | Two-way random agreement/consistency, single/average units and unbalanced long-format identity; replay |
| 1 kappa group | Quadratic weights with numeric category ordering; replay |
| 2 test-retest groups | Pearson wide-format replay, SAV labels/user missings and explicit factor/text coercion |
| 1 failure group | Expected-invalid exit zero still creates a failed, non-replayable bundle |

The remaining 26 groups still run in `phase2`/`all`: complete-case and mean
scoring, observed reversal bounds, other ICC/kappa/retest variants, grouping,
matrix/nonestimability boundaries, privacy/template/input replay and invalid
options. Smoke is representative integration evidence, not full numerical
acceptance; the complete 34 groups remain mandatory for that claim.

## Independent numerical references

- Scale: `psych::alpha()` on the selected covariance matrix for raw/deleted
  alpha, and separately on the selected correlation matrix for standardized
  alpha. This distinction matters with pairwise missingness. Item-to-total/rest
  correlations use `stats::cor()` on a synthetic complete matrix having exactly
  the selected covariance; this avoids copying the implementation formula.
  `stats::factanal()` supplies one-factor loadings/uniquenesses for omega.
  Base-R summaries independently verify every item statistic, inter-item
  correlation range/mean, complete-case sum/mean scale scores, and missing counts.
- ICC: all 12 requested model/type/unit combinations are compared with
  `psych::ICC(..., lmer = FALSE, alpha = 1 - conf_level)`: estimate, both interval
  bounds, F, both degrees of freedom, p, raters/subjects, and missing counts.
  One-way consistency has the same effective agreement estimand. Two-way
  agreement and consistency are checked separately, including mixed models.
- Kappa: `psych::cohen.kappa()` checks unweighted, linear and quadratic point
  estimates on asymmetric tables. Cases distinguish numeric order `1, 2, 10`
  from lexical order and retain declared nonalphabetical/unused factor levels.
  Kappa CIs/p values remain unimplemented; this suite does not invent them.
- Test-retest: `stats::cor.test()` checks Pearson/Spearman estimates and p values;
  Pearson intervals are taken directly from that function. Spearman's retained
  Fisher-z interval is explicitly an approximation, independently evaluated as
  such, not claimed to be an exact confidence interval.

References use tight relative/absolute tolerance (`2e-7` for analytical fields;
most presentation/count checks use `1e-8`). Replays compare the entire raw result
object and Markdown exactly rather than rounding away differences. The synthetic
fixtures are generated deterministically by a test-local fixed seed.

## Boundary and execution coverage

Tests cover pairwise/complete missingness, sum/mean scoring, fixed and globally
observed reverse bounds, coercion of textual/factor numeric values, SPSS labels
and user-missing codes, and separate real `NA` versus missing groups. They also
exercise unbalanced wide/long ratings, factor preservation through pivoting,
duplicate id/rater failures, contradictory weighted category-order failures,
and exact source-row provenance for a coerced bad rating.

Every successful case checks run/request/input/dictionary/template/output hashes
and saved scientific/environment context. Selected cases check source row
identities and rounded Markdown. Replay cases change original source bytes,
working Parquet, original custom templates and config while preserving frozen
run results. Privacy/logging optout includes `logging.include_outputs = FALSE`;
mandatory raw results must remain available. `--interactive FALSE`, invalid
numeric/scientific domains, and repeated selected variables are covered.

Unavailable inference is tested explicitly: incomplete/indefinite scale matrices
must not produce misleading alpha/omega, while valid item descriptions remain
available. A wholly nonestimable reliability request is a failed run. The
compatibility `--expect-invalid TRUE` may return exit zero but must still leave
a failed, non-replayable bundle with no normal Markdown. Valid perfect/boundary
ICC or Pearson estimates are retained; a three-subject correlation must not
invent a confidence interval.

## Existing stored reliability goldens

```bash
Rscript tests/values/reliability_compute_golden.R
bash tests/smoke/run_reliability_tests.sh
```

The generator now uses `psych::ICC`, `psych::cohen.kappa`, and `stats::cor.test`
rather than duplicating the old module formulas. Substantive golden corrections
are two-way agreement interval bounds, honoring the requested mixed-model
agreement estimand (instead of silently switching to consistency), and labeling
the one-way result with its effective agreement type. Remaining tiny differences
are floating-point arithmetic. Existing Python JSONL golden checks remain in use.
Constant kappa is now explicitly expected to fail, not accepted as successful
null-valued analysis.

Run final acceptance only on a frozen repository snapshot, and do not replace
source files while its tests are loading them. These checks verify deterministic
statistical output and audit evidence, not the semantic adequacy of a final
scientific interpretation.
