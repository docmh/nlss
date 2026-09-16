# Import contract and input versions

Use this reference when importing data, resolving a source conflict, choosing CSV
types, interpreting missing codes, or maintaining the shared data-loading code.
It does not replace the semantic analysis and reporting workflow.

## Existing workflows remain available

CSV, SAV, RDS, RData and Parquet retain their source flags and statistical options.
Working data stay visible outside `.nlss/`; originals are untouched. Current
projects publish `.nlss/runs/<id>/` evidence and the automatic root protocol,
without a parallel JSONL log. See [the run contract](run-contract.md) and
[agent-side folder setup](utilities/project-create.md).

SAV now requires haven: the importer reads user-missing definitions before masking
them. All import routes normalize these codes to ordinary R `NA` for analysis.
Numeric labels do not automatically make a variable categorical. Declare model
roles, reference groups and contrasts through the respective analysis options.

Original variable/value labels, missing codes/ranges and tagged missing reasons
are retained in `dictionary.json`. Row-specific missing metadata refers explicitly
to the original source rows; it is not reapplied to reordered or imputed data.
Date, timestamp, hms and difftime values use explicit double storage plus logical
type metadata, preserving fractional seconds, units and timezone information.

## CSV interpretation

```bash
Rscript scripts/R/descriptive_stats.R --csv survey.csv --sep ';' \
  --csv-decimal ',' --csv-encoding UTF-8 \
  --csv-col-types 'id=character,score=numeric' --csv-na-values 'NA,99' \
  --vars score
```

Options override `defaults.csv.decimal`, `encoding`, `col_types`, `na_values`,
`sep` and `header`. Defaults are decimal `.`, UTF-8, automatic types, `NA`, comma
separator and a header row. Types can be `auto`, `character`/`string`,
`numeric`/`double`, `integer`, `logical`, `date` (ISO YYYY-MM-DD), `factor` or `skip`.
Explicit numeric/type conversions fail rather than silently introduce new missings.
Digit-only columns with leading zeros and integers that would lose numeric
precision remain text during inference. An explicit numeric override is deliberate.
Encoding errors stop the import rather than silently truncate rows.

Inspect the generated `codebook.md` before analysis: names, labels, R types and
missing counts. Its role is an import preview, not a template for final scientific
interpretation. Decimal-comma and categorical-role ambiguities should be clarified
with the researcher; labels alone cannot settle those decisions.

## Changed sources and filename collisions

- Unchanged source bytes and import options: reuse the current working copy,
  recording the actual current input snapshot, which may contain documented edits.
- Changed source bytes/options: stop. To deliberately import the new source,
  repeat the command with `--import-action new-version`. Earlier snapshots remain.
  This explicit action also reloads an unchanged source when the intent is to
  start again from the original rather than use an edited working copy.
- A different source with the same dataset filename/name: stop. Use a distinct
  `--dataset-name wave2`.
- An existing working file without a verified import binding must not be claimed
  as an import of another source. An explicit `--parquet` can select that working
  file; unknown original provenance remains unknown. Do not reimport over later
  edits merely to suppress an unexplained conflict.

The explicit-file importer preserves its source under the import directory's
`sources/<source-sha256>/<filename>` and records source hash, reader options,
reader/contract version and dataset ID. Project setup/import through
`project-create` instead uses the shared object/version store for all five input
formats, binding received raw bytes and effective reader options in `dataset.json`
(`source.import`). Matching setup reuses working edits; deliberate separate
imports use a distinct registration, not the explicit-file importer's
`--import-action` flag. Neither route converts older project layouts.

## Input artifacts and audit

Explicit-file imports retain these input-side assets in their visible dataset
directory. This is not the current project's output layout:

```text
dataset/
  dataset.parquet                 # editable imported working copy
  import.json                     # source binding and imported version
  dictionary.json                 # current preview dictionary
  codebook.md                     # current human-readable preview
  sources/<source-sha256>/...      # preserved original bytes
  versions/v-<version-sha256>/
    data.parquet                  # immutable analysis input
    dictionary.json               # exact dictionary for this version
    provenance.json
```

Registered managed inputs instead use `.nlss/datasets/` descriptors and shared
`.nlss/objects/`; their working path remains visible. Both input routes publish
statistical results through the same current-project `.nlss/runs/` boundary.
Deliberately unmarked standalone runs retain their own output and optional log;
a bare statistical command does not create a project marker.

The top-level `dataset` object in saved dataset-backed requests/results contains
`dataset_id`, `version_id`, `data_sha256`, `dictionary_sha256`, `snapshot_path`,
`dictionary_path`, `source_sha256` (when known) and, for explicit imports,
`import_version_id`. Paths are
workspace-relative. The hashes refer to actual saved file bytes (SHA-256).
The version key additionally distinguishes source bytes/options and reader metadata,
so identical table values from changed source files cannot share contradictory provenance.
Working copies with unknown provenance have no invented source hash.
`data-transform`, `missings` and `impute` bind before/after versions and publish
working changes through the shared boundary. Current-project recovery reuses
preserved inputs, without a permanent backup-copy family. Missingness handling also records retained input-version
row indices; source missing observations are historical provenance and must not
be reapplied to filtered/imputed rows. Reusing a matching import preserves edited working data.
Project setup is not repeated import and adds no separate utility journal.
Source imports commit per dataset rather than as one multi-file transaction. See
[data-transform](subskills/data-transform.md), [missings](subskills/missings.md) and [impute](subskills/impute.md)
for metadata and replay boundaries.

Source imports use a per-dataset lock; an interrupted import is not silently
accepted as a successful source binding. Check that no import is running before
manually removing a stale `.import-lock`, then recover with an explicit new-version
import. Snapshots/source bytes already preserved remain available. Do not edit
files inside `versions/` or `sources/` to repair an import conflict.

Reusing an earlier statistical result requires matching input/dictionary hashes
and effective analysis settings, not just matching command text or filenames.
The [run contract](run-contract.md) adds verified replay for the
statistical modules. Utility/lifecycle events explicitly use a different audit
and are not statistical replay requests. Saved numbers
and tables support a freely structured, semantically written final research report.

## Developer checks

Run `bash cmdscripts/tests.sh phase1 --root /tmp/nlss-phase1-check --keep 0`.
The portable R runner is listed in `tests/tests.yml`; it uses private configuration,
offline public fixtures with pinned SHA-256, and independent base-R statistical
expectations. Older smoke wrappers still contain retired setup/path assumptions; see
[the current testing guidance](../README.md#current-project-acceptance-and-historical-smoke-runners).
Keep numerical expectations when aligning scaffolding; obsolete setup failures
are not statistical-method failures.
