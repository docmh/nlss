# Phase 2 categorical and exploration acceptance

This offline suite invokes the public R CLIs for `frequencies`, `crosstabs`, and
`data_explorer`. Statistical references use independent base R or `stats`, never
NLSS computation helpers. No new test framework is introduced.

```bash
Rscript tests/phase2/run_categorical_tests.R --root /tmp/nlss-categorical --keep 0
bash tests/smoke/run_frequencies_tests.sh --root /tmp/nlss-frequencies --keep 0
```

`--modules frequencies,crosstabs,data_explorer` selects modules explicitly. The
runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, `NLSS_TESTS_CONFIG`, and the paths
in `tests/tests.yml`. The frequency wrapper resolves the registered runner there.
Each invocation creates isolated projects and private `NLSS_CONFIG_PATH` YAML;
it never edits the installed canonical configuration. Explicit output roots are
never pruned. `results.json` records every case, elapsed time, numeric-comparison
count, errors and R/package versions; a failed assertion returns nonzero.

## Numerical coverage

- Frequencies: independent counts, total/valid percentages and missingness;
  nonalphabetical and unused factor levels, all-missing columns, implicit numeric
  selection, grouping with actual missing values and literal `NA`/missing-label
  collision cases, SPSS user-missing codes and labels, and rounded Markdown.
- Crosstabs: every cell of asymmetric 2x2 and 3x4 tables; total/row/column
  percentages; expected counts, Pearson and adjusted residuals;
  chi-square with/without Yates, phi, Cramer's V, contingency coefficient,
  expected-count diagnostics; exact Fisher conditional odds ratios and confidence
  limits (including true infinite limits), actual versus requested simulation,
  seeded Monte Carlo Fisher inference, multiple row/column pairs and missing
  groups, independent raw/Markdown percentage filters, and counts-only tables.
- Data exploration: missingness and unique counts, mean/SD/range/median/quartiles,
  numeric/date/ordered/logical type heuristics, asymmetric level percentages,
  top-N plus Other aggregation, SPSS labels and rounded overview Markdown.

Frequencies additionally have 71 stored independently generated golden rows on
the configured standard dataset and a Python legacy-JSONL checker. Regenerate:

```bash
Rscript tests/values/frequencies_compute_golden.R
```

The categorical runner calls the checker on both ungrouped and grouped outputs.
The existing crosstabs generator now obtains adjusted residuals directly from
`stats::chisq.test(..., correct = FALSE)$stdres`. It also excludes missing group
values before selecting the control group; the previous Boolean subset inserted
synthetic missing rows. Existing selected cell goldens happen to be unchanged;
the new full-matrix checks catch the previously untested off-diagonal residual
error. Only the affected grouped N/missing golden fields change.

## Execution contract coverage

Every successful CLI case validates the run state, saved request options/design,
request/input/dictionary/template/output hashes and required Markdown. Replay
checks full-precision result equality and exact Markdown equality. Cases cover
private prompt and legacy logging optout, frozen custom templates, changed working
data and configuration, `--interactive FALSE`, invalid option failures,
nonestimable/requested Fisher failures without completed output, and rejection of
success-shaped bundles in pending or misnamed directories. Data exploration also
checks deterministic per-run table numbering 1/2 after prior tables while legacy
numbering continues.

Always run integration acceptance on a stopped/frozen repository snapshot. Do
not update runtime files while a test process is loading them. These tests verify
statistical output, not the semantic adequacy of a final scientific report.
