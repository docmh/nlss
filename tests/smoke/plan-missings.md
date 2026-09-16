# Missing-data module smoke plan

The registered standalone runner is `tests.scripts.modules.missings`. It delegates
the five independent numerical method cases to the public Phase-2 acceptance
runner with private configuration, then emits a separate explicit `run_ok` marker
for each Python JSONL golden check. This intentionally overlaps, rather than adds
independent datasets to, the full module acceptance evidence.

- Exercise `auto`, `listwise`, `impute`, `indicator` and `drop` on the registered
  `tests.golden_dataset` using six selected variables.
- Compare original-N missing counts/proportions/percentages, full transformed
  selected values and identifiers, selected methods, drop/indicator decisions,
  imputation methods, pattern ranking and top-three-plus-Other aggregation.
- Verify immutable before/after references, current output, verified backup,
  source identity, mandatory artifact hashes and released owned locks.
- Read legacy JSONL independently in Python against 8,892 base-R golden cells;
  require exact categorical values/missing masks and numeric tolerance `1e-10`.

Run `bash tests/smoke/run_missings_tests.sh --root /tmp/nlss-missings-smoke --keep 0`.
`NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, `NLSS_TESTS_CONFIG` and `PYTHON_BIN` are honored.
The source configuration/templates are never rewritten. The full coverage matrix,
scientific edge cases and replay failures are documented in
`tests/phase2/missings-README.md`; use the full runner for those checks.
