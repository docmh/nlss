# Phase 1 import regression tests

Run from the repository (or pass an absolute script path):

```sh
Rscript tests/phase1/run_phase1_tests.R
Rscript tests/phase1/run_phase1_tests.R --root /tmp/nlss-phase1 --keep 10
```

The runner reads `tests/tests.yml` (override with `NLSS_TESTS_CONFIG`) and honors
`--root` / `NLSS_TEST_ROOT` and `--keep` / `NLSS_KEEP_RUNS`. Without an explicit
root, outputs go to `tests.output_dir/<timestamp>/phase1/run-<timestamp>-<pid>/`.
Each case has a separate project, subprocess logs, and retained analysis
artifacts; `results.json` records the checks and package versions. An explicit
root is never pruned. Keep `0` disables pruning. Source configuration is never
modified: a private configuration from NLSS built-in defaults is supplied via
`NLSS_CONFIG_PATH`, isolating these tests from legacy smoke-template overrides.
All fixtures are generated or decoded locally; no network is needed.

## Coverage and independence

- The focused import-contract unit script checks normalization idempotence,
  source-row provenance through filtering/imputation, temporal metadata,
  explicit CSV overrides, and invalid import options.
- CLI/configuration checks exercise complete option parsing, early rejection of
  unknown/invalid inputs, module option-registry coverage, and effective config
  overrides without editing source configuration.
- Seeded mice checks preserve every imputation in a hashed `mids` artifact,
  compare imputation draws to an independent `mice::mice` call, retain original
  columns and the existing single-completion output, and require the explicit
  warning that this completion is not Rubin-pooled statistical inference.
- Clean numeric CSV uses the configured `tests.golden_dataset` and checks
  descriptive statistics and regression against base R and `stats::lm`.
- One synthetic dataset is stored as SAV, RDS, RData, and external Arrow
  Parquet. It contains actual `haven_labelled_spss` columns, a discrete missing
  value `99`, missing range `97:98`, and a regular `NA`. All four import paths
  must produce identical normalized values and the same independently
  calculated sample sizes, means, standard deviations, variances, medians,
  regression coefficients, standard errors, test statistics, p-values,
  confidence intervals, and R-squared values. Absolute tolerance is `1e-6`.
  A misspelled or empty RData object name must fail instead of selecting another frame.
- Public haven fixtures check string value labels, original SPSS missing
  definitions, dates, large datetimes, subsecond time durations, and Stata
  tagged missing values through the supported RDS import path. Tagged missing
  reasons must survive as source-row provenance, not as unsafe analysis values.
- CSV tests specify decimal mark, UTF-8 encoding, column types, and NA tokens;
  character identifiers keep leading zeroes, malformed typed data fail, and
  changed import options require explicit version creation.
- Same-basename collisions cannot silently use another source. Updated
  sources fail with feedback until `--import-action new-version` is explicit.
  An explicit `--dataset-name` disambiguates another source.
- Every basic analysis must log stable dataset/version identifiers and
  verifiable data/dictionary hashes; old snapshots and old log entries remain
  unchanged. Original source bytes remain archived after reimport; working-copy
  changes receive distinct analysis snapshots. Explicit source reimport also
  restores original values after working-copy edits, even when the source bytes
  themselves are unchanged, while preserving the edited snapshot. A deliberately corrupted
  dictionary in an isolated test project must stop further analysis with an
  integrity error. The imported dataset exposes `codebook.md`, `dictionary.json`,
  and `import.json` in addition to canonical analysis Markdown.
- Changed source bytes receive distinct provenance versions even when the
  normalized numeric data are identical. A corrupted archived source is
  rejected even when the analysis explicitly targets the working Parquet copy.

These tests use NLSS readers only to inspect the storage contract. Expected
statistics are computed without importing NLSS module functions. They add
import-focused regression coverage, not a replacement for the full existing
module value suites. They do not assert direct DTA/SAS/Excel support or certify
every statistical option.

## Public fixture provenance

`tests/data/import/haven-fixtures.json` contains the exact four small public
test fixtures as Base64, so changes remain text-reviewable and test runs do not
download anything. Each decoded file is checked against its recorded SHA-256
before use. The manifest includes a pinned upstream URL, original filename,
download date, and checksum for every fixture.

Source: [tidyverse/haven test fixtures at commit
f067fb27e436bc1207e8424f50df90ed9d5acc3a](https://github.com/tidyverse/haven/tree/f067fb27e436bc1207e8424f50df90ed9d5acc3a/tests/testthat),
downloaded for the NLSS import review on 2026-09-12. Files are
`spss/labelled-num-na.sav`, `spss/labelled-str.sav`, `spss/datetime.sav`, and
`stata/tagged-na-double.dta`. They contain upstream test data, not participant
records. The haven package is MIT-licensed; the attribution and permission
notice are preserved in `tests/data/import/LICENSE-haven.txt`.
