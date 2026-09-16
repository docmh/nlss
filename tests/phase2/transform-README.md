# Data-transformation acceptance

The Wave-11 runner exercises the public `data_transform.R` and `replay_run.R`
entrypoints. It does not source NLSS numerical functions or use previous module
output as a numerical oracle. Existing suites remain unchanged.

```sh
Rscript tests/phase2/run_transform_tests.R --root /tmp/nlss-transform --keep 0
Rscript tests/phase2/run_transform_tests.R --root /tmp/nlss-transform-focused --keep 0 --match 'recode|coercion'
Rscript tests/phase2/run_transform_tests.R --root /tmp/nlss-transform-smoke --keep 0 --match '^transform_.*_smoke$'
```

`tests/tests.yml` registers this runner and its shared publication contract.
`--match` selects case names, not arbitrary CLI options of another runner. The
three `_smoke` cases cover the full-operation numerical golden, labelled SAV
lineage and frozen-input replay after a later data change; they overlap the full
suite and are not additional independent coverage. The full registered Phase-2
run already includes the complete transformation suite: do not launch it again
beside that full run.
The existing module smoke runner additionally runs the same all-operation
JSONL golden with explicit `run_ok` markers. This preserves its standalone
numerical acceptance; it overlaps, rather than adds to, the 3,120-value oracle.

Every case uses a separate project beneath
`<root>/phase2-transform/run-<timestamp>-<pid>/cases/`. The runner uses a private
`NLSS_CONFIG_PATH` and never rewrites shipped configuration or templates. Explicit
roots are retained; the default root/retention settings come from `tests/tests.yml`.
`NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG` are supported. Use a
disposable source copy when other work may change production R code during replay.

`started.json` records selection and all production-R hashes before execution.
Each CLI log has a neighboring `.process.json` with argv, start/end timestamps,
entrypoint hash and actual exit status. `results.json` retains per-case status,
timing and numeric-comparison count, dependencies and a source-drift indicator.
Intermediate runs with source drift are development evidence, not final acceptance.

## Independent numerical coverage

- `log`, `log10`, `sqrt`, `exp`, `abs`, centering, z-standardization and all three
  z-transform aliases use independent base-R computations.
- Scalar and sequential calculations, missing-value arithmetic, actual operation
  order, dependent operations and custom names/suffixes are checked against
  directly calculated vectors. Legacy single-column matrices and ordinary list
  columns remain usable, with general expressions explicitly outside automatic
  replay. Bounded calculation tests cover function/data-name shadowing and
  propagation of an unverified dependency's replay restriction.
- Recodes match original values simultaneously, including missing values, numeric
  to text promotion, factor recodes to new categories and arrow pair syntax.
  They do not cascade earlier replacements or create factor-level missings;
  close numeric codes must retain their double-precision identity.
- Numeric factor coercion uses displayed values, not integer level indices;
  character conversion losses are preserved and disclosed.
- Quantile bins use base-R type-7 quantiles, duplicate-breakpoint reduction and
  `cut`; custom bins check sorting, right-closed endpoints, included lowest
  endpoints and values outside the provided range.
- In-place variants retain explicit overwrite permission and confirmation.
  Missing/nonfinite values, constant/all-missing standardization and empty-row
  scalar calculation are checked without fabricating numerical estimates.
- The static golden contains **3,120 independent numeric values** for 13 derived
  columns and 240 rows of the registered standard dataset. The runner compares
  immutable output Parquet values; the separate Python checker also reads the
  legacy JSONL `transformed_df`. These two checks are complementary projections
  of the same expected values, not independent datasets.

The generator uses only base R and never sources NLSS code:

```sh
Rscript tests/values/data_transform_compute_golden.R \
  --data tests/data/golden_dataset.csv \
  --out tests/values/data_transform_golden.csv
```

The golden case uses the following options on the standard golden dataset:

```sh
--calc 'gain=post_score-pre_score|constant=7' \
--transform 'age=log|income=log10|education=sqrt|x1=exp|x2=abs|outcome_reg=center|outcome_anova=scale' \
--standardize pre_score --recode 'ordinal_var=1:2,2:3,NA:0' \
--percentile-bins 'satisfaction=4' --bins 'age=18,30,45,80' \
--rename gain:change --drop x3 --confirm-drop TRUE
```

```sh
python3 tests/values/check_data_transform_golden.py \
  /path/to/dataset/analysis_log.jsonl 0 \
  tests/values/data_transform_golden.csv all_operations
```

Default numeric tolerance is `1e-10` relative/absolute. Row identities and explicit
  integer mappings are exact; finite values, NA/NaN masks and infinite signs are
checked separately. JSONL uses the same tolerance and requires expected missing
cells to be JSON null, rather than skipping missing expected values.

## Data and execution contract

CSV, RDS, RData, Parquet and SAV paths are exercised with independent fixtures.
Special cases cover semicolon/decimal-comma CSV, leading-zero identifiers,
non-syntactic names, duplicate SPSS value labels, user-missings, pure renames and
overwrites. A formerly missing code explicitly reintroduced by recoding must
remain valid on the next load. Unchanged metadata and immutable input definitions
remain preserved; derived values cannot inherit false labels or missing rules.
Date, fractional timestamp/timezone and difftime units are checked using the
stored numeric values and the explicit logical-type dictionary.

All successful runs verify source identity, before/after data/dictionary hashes,
input-row identity, dataset/import lineage, immutable artifacts, current output,
verified backup when applied, and release of owned locks. A following descriptive
analysis must select the transformed version. No-op, custom-template, private
configuration and `--log FALSE` paths retain truthful mandatory audit output.
One public masking regression preserves spaced/nested division expressions in
Markdown and legacy log details/options/commands, redacts quoted Unix, escaped
Windows and raw-R path literals there, and verifies exact unredacted values in
the immutable output dataset and private machine-readable run result.

Expected failures cover overwrite/drop permission, invalid expressions and
shapes, missing variables, coercion refusal, fractional/invalid percentile counts,
constant/missing/nonfinite quantile inputs, duplicate recode keys, duplicate
custom breaks and dropping all columns. Initialized working data, dictionary,
codebook, report, log, manifest and earlier versions must remain unchanged.
Foreign analysis/import/publication locks must not be removed.

Replay must use the original before-version after later working changes, preserve
the original run and current data/metadata/backups, reproduce output data and
dictionary hashes and deterministic Markdown, and explicitly report that the
replayed output was not applied. Tampered requests and tampered immutable output
data/dictionaries are refused. General legacy
R expressions remain callable, but random or side-effecting calculations must
be marked non-replayable; refusal must happen before executing the expression.

The separately registered `data_change_contract.R` performs rigorous isolated
publication fault injection. Its internal assertions count as one runner case;
its own diagnostic output is preserved. This is controlled-error protection, not
a claim of crash-atomic publication or arbitrary-expression sandboxing. No tests
promise rollback of external side effects from user-requested general R code.
