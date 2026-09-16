# Calc utility acceptance

Run from a disposable checkout when running alongside stateful shell suites:

```bash
Rscript tests/phase2/run_calc_tests.R --root /tmp/nlss-calc-check --keep 0
Rscript tests/phase2/run_calc_tests.R --root /tmp/nlss-calc-check --keep 0 \
  --match '^calc_(operators_parentheses_and_constants_independent|math_functions_independent|distribution_functions_independent)$'
```

The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`.
Paths and suite registration belong in `tests/tests.yml`. Private configuration
is written under the run root, never into the installed canonical config.
Zero retention means keep the run artifacts, matching the existing harness.

Calc is dataset-free, so numeric references use direct, independent base-R and
`stats` calculations rather than a dataset golden. Tests do not evaluate
expected results through Calc's evaluator. They check every exposed operator
and numerical function, including actual raw numeric vectors and nonfinite
statuses, not merely the shape of JSON output.

Coverage includes:

- arithmetic, grouping, constants, ordered assignments/rebinding and unnamed
  expression collisions;
- all curated mathematical/distribution functions, plus explicitly unsafe
  vector statistics, matrices and controlled random calculations;
- plain/JSON/CSV stdout, unrounded raw values, NA/NaN/signed infinity and
  JSON decimal-point validity after an unrestricted `OutDec` change;
- canonical/configured/CLI templates, frozen bytes, table numbering, mandatory
  scope notices, private defaults and CLI precedence; explicit missing,
  directory and malformed template refusals before unrestricted evaluation;
- disabled JSONL with retained report/audit, unchanged active-dataset manifests,
  configured fallback without invented workspaces, output-root binding and
  symlink-ancestor refusal before unrestricted evaluation;
- exact private expressions with path/comment masking in human projections;
- parse/type/domain failures, restricted-function refusal, publication locks,
  unsafe side effects outside publication rollback, and directory targets;
- statistical-replay refusal, legacy checksums and reconstructible report blocks.

Each command gets separate stdout/stderr and a JSON execution record written
before launch, then completed with the actual exit code. `started.json` records
source identity; `results.json` records case outcomes, numeric comparison counts
and process results. A changed production file invalidates the run; freeze the
checkout before final acceptance.

The original `tests/smoke/run_calc_tests.sh` remains compatible and also invokes
a focused subset of these independent numerical cases. Overlapping cases are
overlapping evidence, not additional independent scientific coverage.

The tests do not claim an operating-system sandbox, automatic utility replay,
power-loss recovery or validation of the research assumptions behind a formula.
Unsafe side-effect tests touch only their own disposable fixture files.
