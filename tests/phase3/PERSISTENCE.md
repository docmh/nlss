# Phase 3: focused project persistence acceptance

## Folder-first initiation/import follow-up

Use `--match '^startup_|^create_preserves|^creation_'` for direct
CSV/SAV/RDS/RData/Parquet initiation with defaults and structured JSON results.
Cases cover raw-source byte identity, labels/user-missing metadata, leading-zero
IDs, effective CSV defaults, unchanged originals, visible working data, ordinary
descriptives/root protocol, replay without activation, zero-write repeated setup,
additional dataset registration, name/source/options conflicts, relocation,
invalid-import preflight and contention on the existing writer locks.
The unsupported-format negative fixture replaces the formerly rejected valid CSV;
all existing collision/path and scientific checks remain.

## Task A/B selection

The shared execution route now places current-project statistical and utility
records under `.nlss/`, with an automatically maintained `report_canonical.md`
at the project root. Statistical entrypoints are unchanged. Run the bounded
A/B selection from the repository root:

```bash
Rscript tests/phase3/run_persistence_tests.R --root /tmp/nlss-project-routing-check --keep 0 --match '^task_ab_|^publication_|^noop_and_real_transform'
```

These groups cover activated read-only location APIs, deep/explicit project
selection, regression against `lm`, parameter-only Power, Calc, general dataset
selection, CSV/SAV/RDS/RData/Parquet imports, visible working data and unchanged
originals. They check automatic root protocol entries, evidence links and
reconstruction of saved numerical tables without rerunning analyses. Protocol
collisions fail explicitly for statistics and utilities; injected transformation
publication faults preserve earlier protocol entries and do not erase a later
external data edit. No parallel JSONL or per-dataset protocol is expected.

This is A/B coverage; C's artifact/replay/MI checks are below. Task F aligned
the complete suite with the current layout. Creation uses `project_create.R`;
unsupported marker writes are rejected without conversion. Independent Power
values, a subsequent transformation, exactly-once protocol entries and warning/
failed-status reconstruction are included in the final acceptance.

## Task C selection

```bash
Rscript tests/phase3/run_persistence_tests.R --root /tmp/nlss-project-artifact-check --keep 0 --match '^task_c_|^publication_|^noop_and_real_transform|^root_rename_|^replay_rejects_'
```

Seventeen groups cover run-local plot links/layers, saved fitted objects,
registered and explicit-source imputation-to-pooling chains, whole-project
relocation and non-activating replay (including parameter-only input and invocation
from another project). Numerical expectations use ggplot2, base R and direct
`mice::pool`; no fitting or pooling implementation is duplicated.

The selection includes existing code/environment/damaged-input replay gates,
special-file rejection, no-op/version reuse and managed publication faults.
Additional failures cover ordinary imported working files, including preservation
of a later external edit. Labelled row filtering retains its input-row map.
New unmarked mice generation/pooling works without inventing a marker. Only
the current `.nlss/imputations/` layout is read; there is no legacy artifact adapter.
Source/fixture identities and real subprocess outcomes use this existing harness.

## Complete current-layout coverage

Task F's separately authorized repairs are covered by `--match '^task_f_'`:
real named imports both standalone and in a project, correct grouped missing
counts/denominators across CSV, factor/RDS and SPSS-labelled user-missing input,
and all eight existing descriptive numerical golden rows through current CLI
executions. The grouped golden uses observed row indices, not reproduction of
the former NA-indexing defect. These checks retain the missing-group category
and do not reduce statistical options or add module-specific storage logic.

This suite checks shared current-project storage with representative unchanged
statistical procedures. It complements the [inspection suite](README.md) and
[report suite](REPORTS.md). It does not change or comprehensively revalidate every
statistical method, generate semantic reports, implement foreign-format project
writers or claim crash-atomic recovery.

Run from the repository root after the implementation is stable:

```bash
Rscript tests/phase3/run_persistence_tests.R --root /tmp/nlss-persistence-check --keep 0
```

The runner accepts `--match REGEX` for affected cases and honors `NLSS_TEST_ROOT`,
`NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`. Its fixtures come from the registered
golden dataset and ordinary synthetic study document. Selected columns add labelled values,
user-missing provenance, leading-zero IDs and fractional temporal values. These
remain synthetic test inputs, not claims about a real study or its findings.

Acceptance checks cover:

- Exact received-source preservation, a separately writable visible working file,
  schema-2 declaration and small dataset/version descriptors.
- One content-addressed object per unchanged byte stream; repeated analyses,
  no-op transformations and ordinary document edits do not create data copies.
  Real executions still publish distinct authoritative run bundles.
- No automatic study-document capture or scratchpad/log/import projections.
  The root canonical protocol is automatically available; no backup family.
- Real transformation values against base R; immutable before/after references;
  external changes and reordered rows consumed as the next fixed input without
  applying stale missing masks by row position.
- Deterministic conflict injection after candidate preparation. Final bundle
  rename failure restores only owned bytes; a concurrent external edit must not
  be erased by rollback. Candidates and calculated output remain verifiable;
  calculation/publication states stay distinct, and failures cannot appear as
  completed runs.
- Creation collisions, paths escaping the project, nested-project boundaries,
  existing markers, malformed descriptors and conflicting
  selectors. Contained absolute selections become project-relative descriptors.
  Symlink/FIFO cases, including non-request and wrong-layout replay inputs, have
  bounded subprocess timeouts and explicit skips where the operating system cannot
  create the test object. POSIX inventories use the real `test -f` regular-file
  check before hashing; R's `file_test("-f", ...)` also accepts FIFOs on Linux.
- Read-only inspection of a changed managed project. Its initial registered
  reference is explicitly not its latest state, and counted run directories are
  unverified candidates rather than replay verdicts.
- Whole-project relocation and replay from frozen inputs without activation;
  damaged historical bytes and code/environment mismatches still refuse replay.
- Explicit standalone data-file analysis remains unmarked. It does not discover
  child projects, including child markers that are symlinks/FIFOs; source bytes
  and the unrelated child remain untouched. No old-project adapter or pilot
  module restriction is exercised.

All configuration, fixture changes and faults are isolated in the runner's fresh
directory or local R environments. Repository configuration/templates and existing
projects are never rewritten. `results.json` records source/fixture SHA-256s,
selected invocation, start/end times, actual subprocess exit codes, assertions,
skips and environment identity from the outset. Captured stdout/stderr are bounded
to 16,000 characters per stream with explicit truncation flags. A source-drift gate rejects a run
whose implementation changed while it was executing. Forced roots are never
pruned; default retention only removes positively identified completed directories
owned by this suite.

Use targeted checks while developing and one consolidated acceptance run on the
stable persistence slice. This is not a substitute for the relevant existing
shared import, execution, data-change, replay and read-only inspection checks.
Native Windows results are only claimed when actually executed there.
