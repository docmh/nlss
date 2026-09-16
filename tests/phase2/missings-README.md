# Missing-data acceptance

Wave 12 tests the public `missings.R` and `replay_run.R` entrypoints. Numerical
references use base R without sourcing NLSS functions or reusing previous NLSS
output as an oracle. Existing statistical suites remain registered unchanged.

```sh
Rscript tests/phase2/run_missings_tests.R --root /tmp/nlss-missings --keep 0
Rscript tests/phase2/run_missings_tests.R --root /tmp/nlss-missings-focused --keep 0 --match 'temporal|tagged|listwise_then_transform'
Rscript tests/phase2/run_missings_tests.R --root /tmp/nlss-missings-smoke --keep 0 --match '^missings_.*_smoke$'
```

The seven `_smoke` cases cover the five golden methods, labelled SAV lineage and
non-activating replay after a later change. These overlap the complete module
runner; they are not seven additional independent datasets. The standalone shell
runner additionally emits explicit `run_ok` markers for each golden JSONL check.

Each case creates its own disposable project beneath
`<root>/phase2-missings/run-<timestamp>-<pid>/cases/`. Configuration uses a private
`NLSS_CONFIG_PATH`; the shipped configuration and templates are never rewritten.
Root/retention defaults and the standard dataset come from `tests/tests.yml`.
`--root`, `--keep`, `--match`, `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`,
`NLSS_TESTS_CONFIG` and `PYTHON_BIN` are supported. Explicit roots are retained.

`started.json` captures all production-R hashes and selected cases before work.
Every public CLI call has a `.process.json` beside its log with actual argv,
entrypoint hash, start/end and exit status. `results.json` records per-case status,
duration, numeric comparisons, package versions and source drift. Development
runs with changing source are not final acceptance evidence. Use isolated source
copies for concurrent stateful suites and replay checks.

## Independent values and retained semantics

- Five existing method families: `auto`, `listwise`, `impute`, `indicator`, `drop`.
  Automatic selection uses the maximum selected-variable missing proportion with
  inclusive low/moderate/high upper thresholds. Drop and indicator lower bounds
  are inclusive, and unselected variables are not dropped or filled.
- Original-N counts/proportions/percentages, ordered missingness patterns,
  lexical frequency-tie ordering, top-N truncation and Other aggregation.
- Complete-case row filtering over selected variables only and exact retained
  input-row identities; unselected missing observations survive appropriately.
- Mean versus median uses the retained sample-SD third-moment skew definition,
  strict absolute-skew comparison, configurable threshold, constant/one-donor/
  two-donor cases and full-precision unrounded stored values.
- Character, factor-level-order, ordered-factor and logical tied modes; unused
  factor levels and logical classes are preserved. Indicators use pre-fill masks
  and collision-safe suffixes; dropped variables do not leave indicators.
- Empty input and zero remaining listwise rows are valid data results with
  unavailable denominators and a no-inference limitation, not invented estimates.
  All-missing variables remain unresolved if retained; NaN counts as missing;
  infinite selected donors cannot silently produce an invalid fill.
- Date, fractional timestamp/timezone and duration units; SAV/RDS labelled data,
  normalized SPSS user-missings and tagged missing rows. Dictionary source
  observations remain historical, with explicit row lineage for current versions.
- A listwise -> transformation -> selector-free analysis chain must retain the
  original source row count/labels/tags while each request's row map addresses its
  immediate input version. Project activation is explicitly initialized first.
- Extreme finite rescaling cannot change the skew-based mean/median decision;
  tiny expected zero fills are checked exactly, not hidden by absolute tolerance.
- Filling an original SPSS user-missing code does not revive its missing-reason
  display label. Explicit-code/range collisions preserve original provenance,
  legitimate labels and the numeric fill through reload, replay and follow-up.
- Repeated selected names are processed once, preserving first occurrence.

`tests/values/missings_golden.csv` contains **8,892 independent reference cells**
over the five methods. The selected standard-dataset variables are `age`, `income`,
`pre_score`, `group2`, `satisfaction`, `high_missing_var`. Each case uses
`--indicator-threshold .1 --drop-threshold .3 --max-patterns 3` and otherwise
canonical method thresholds/skew settings. Auto selects indicator for this input.
The reference contains summary fields, pattern fields, every selected transformed
cell and ID/indicator, selected method and removed-row counts. Immutable Parquet,
private result JSON and legacy JSONL are checked as different projections of this
same oracle, not counted as independent datasets.

```sh
Rscript tests/values/missings_compute_golden.R --data tests/data/golden_dataset.csv --out tests/values/missings_golden.csv
python3 tests/values/check_missings_values.py /path/to/analysis_log.jsonl 0 tests/values/missings_golden.csv impute
```

Default numerical tolerance is `1e-10` relative/absolute, with exact row counts and
source indices. Missing/nonfinite masks are checked separately; categorical
values are exact. Numerical correctness does not validate a missingness mechanism
or make single imputation an inferentially sufficient analysis strategy.

## Publication and negative coverage

Every successful run validates before/after immutable data and dictionary hashes,
dataset/source/import identity, request/artifact hashes, current working data,
verified backups when applied, and owned-lock cleanup. No-op preserves versions
without redundant backups. `--log FALSE` suppresses only the optional JSONL;
mandatory audit artifacts remain. Custom templates cover both tables and tokens;
private config defaults and explicit CLI overrides remain separately verified.

Replay recomputes from frozen before-data without activating that output, adding
backups, modifying the original run, or changing current data/metadata. Tampered
requests, input/output snapshots, output dictionaries and lineage artifacts are
refused. Failures after an initialized no-op must preserve working data,
dictionary, codebook, report, log, manifest and previously published versions.
Foreign analysis/import/publication locks must survive refused execution.

Malformed CLI coverage includes unknown/bare flags, invalid method, nonfinite/
out-of-range/inverted thresholds, fractional/zero pattern limits, invalid skew,
digits/Boolean options, missing variables, competing sources and zero-column drop.
The separately registered shared `data_change_contract.R` tests controlled
publication/rollback failures; module acceptance does not duplicate its complete
fault matrix or claim crash-atomic behavior.
