---
name: project-create
description: Initialize or reuse a user-selected NLSS project and import CSV/SAV/RDS/RData/Parquet directly, with raw-source evidence, visible working data and structured results.
license: Apache-2.0
---

# Create a current NLSS project

## Scope

Use when the user selects an existing folder and invokes NLSS for
work there. Initialization is part of that workflow, not an additional command
the researcher must request. The same call reuses matching registrations or
imports a selected additional dataset into a current project. All statistical procedures use the same current project
route; no module-specific activation is needed. A deliberately unmarked standalone analysis
remains possible, but does not silently create or adopt a marked project.
Older project layouts are unsupported; no reader, conversion or cleanup is
performed.

## Create and use

The researcher chooses the folder and source data; the agent supplies a sensible
dataset name and unused visible working path unless the user has preferences.
The commands below are agent/developer mechanics, not a researcher prerequisite.
Ask about ambiguous sources, existing conflicting files or import interpretation,
not whether ordinary setup should accompany already requested NLSS work.

Pass the selected raw source directly. The utility uses the existing shared
readers and metadata-preserving Parquet writer; no agent-generated conversion
script or visible intermediate file is needed. Raw source bytes, effective import
options, reader version and import-contract version are recorded in the existing
dataset descriptor/object store. Only the working file is intended for editing.

Ordinary statistical source flags still accept all five formats, but do not
create a marked project on their own. Read-only inspection or a question about
NLSS does not authorize initialization. Incomplete/unsupported infrastructure
requires clarification, not deletion.

```bash
Rscript scripts/R/project_create.R --project /path/to/study --source survey.sav
Rscript scripts/R/descriptive_stats.R --project /path/to/study \
  --dataset survey --vars pre_score,post_score
Rscript scripts/R/data_transform.R --project /path/to/study \
  --dataset survey --calc 'change=post_score-pre_score'
Rscript scripts/R/project_inspect.R --project /path/to/study
```

Only `--project` and, for a new project, `--source` are required. `--name` defaults
to the source basename (RData: selected object name); `--working` defaults to
`data/<sanitized-name>_working.parquet`. Source format is inferred from its extension.
CSV accepts `--sep`, `--header`, `--csv-decimal`, `--csv-encoding`,
`--csv-col-types` and `--csv-na-values`, with canonical configuration defaults.
RData requires `--df OBJECT`; options for another format are rejected.
See [import interpretation](../import-contract.md).

- Existing project without `--source`: return the selected `--name` or active
  registration without writes; import options are not meaningful here.
- Matching source path, raw bytes and options: reuse the registration and preserve
  any working-file edits. Repeated setup creates no data versions or logs.
- New source/name: add a registration. Existing datasets and the active selection
  stay unchanged. Use the returned name in subsequent `--dataset` flags.
- Changed source/options or conflicting working path: stop without resetting data.
  A deliberate separate import can use a distinct `--name` and unused `--working`.
  If one source has multiple registrations, select `--name` explicitly.
- An incomplete `.nlss/`, unsupported marker or unregistered existing working
  destination is not adopted, overwritten, migrated or cleaned up.

Paths are project-relative (absolute paths inside the same project are accepted),
never relative to an arbitrary invocation directory. Symlinks, special files,
parent traversal and paths through another marked project are refused.
Source/working files cannot live in `.nlss/`.

For dataset analyses, `--project` explicitly selects the project;
`--dataset NAME` selects a registration, otherwise the active name is used.
Without `--project`, selection uses the nearest ancestor marker. An explicit
source flag still selects that source through the common loader, with output
routed to the selected project; it is not silently replaced by the active data.
Conflicting input selectors are rejected. A missing explicit project never
falls back elsewhere. Inspection remains read-only.

## Agent and future tool interface

`nlss_create_project(project, source = NULL, working = NULL, name = NULL,
import_options = list())` in `lib/project_store.R` is the shared callable boundary.
`project_create.R` only parses arguments and serializes its result; a future
MCP/plugin adapter can call this function or invoke the CLI without duplicating
setup logic. No MCP server, transport schema or plugin package is added.

Normal CLI success emits one JSON object with `status` (`created`, `dataset_added`
or `reused`), the absolute selected `project`, `workspace_id`, `dataset`
(`name`, `id`, project-relative `working`, `initial_version`), `source` and
`protocol` (`path`, `available`). The initial version is not the latest working
state. Setup does not invent statistical output: protocol availability becomes
true after the first successful analysis. Input/operation failures emit
`status: error` with a message and exit 2; installation/bootstrap failures may
terminate before that serializer. The direct R function raises ordinary R errors.
Reuse is not a full history-integrity check; use explicit verification when needed.

## Files and authority

```text
study/
  nlss-workspace.yml              # schema 2, identity, active dataset, registration
  survey.sav                     # received file, never overwritten by NLSS
  data/survey_working.parquet     # independent, visible editable working file
  report_canonical.md            # automatic SPSS-like protocol after execution
  Manuscript.md                  # optional authored report, name/path chosen freely
  .nlss/
    objects/<sha256>             # exact bytes, reused for identical content
    datasets/<id>/dataset.json   # identity, original source, working location
    datasets/<id>/versions/*.json # small version-to-object references
    runs/<run-id>/               # one authoritative request/result/output bundle
    utility-runs/<run-id>/       # existing utility request/result/output records
    imputations/                # preserved MI artifacts, referenced by consumers
    reports/<report-id>/rev-*.json # explicit semantic report revisions, object refs
    staging/                    # temporary capture/recovery area
```

The root YAML declares managed storage as `managed_parquet_v1`; request/result
schema 1 still denotes dataset-backed execution. Request `storage` and `project` identify registered managed inputs. Parameter-only
Power's schema-2 **request** is a different contract.

The descriptor's `initial_version` is not a latest-version pointer. A run records
exactly the state observed for that run. Current working bytes are frozen before
reading/calculating, including after external edits; neither the edit's author,
time, recipe nor a causal parent is invented. The Parquet import contract retains
labels, factors, missing-code provenance and temporal types. Historical source
row positions are never reapplied as current missing masks.

Parquet sources and initially identical working bytes share one preserved object.
Other formats need one raw-source object plus the imported Parquet object;
`source.import` holds resolved reader options and byte identity. Preparation
files are temporary, not another visible dataset or import directory.
The visible working file is a physical copy, never a mutable hardlink/symlink alias
to history. Each changed version preserves its necessary complete Parquet bytes;
this is not a delta engine. Identical data/dictionaries reuse verified
objects. No unchanged-data backups, timestamped working-file families, parallel
JSONL journal or duplicated row-by-row JSON export is produced here.
The automatic root protocol remains a readable projection of saved outputs,
with artifact links rebased by the common renderer. Each actual execution still has its own request/results and
SPSS-like `output.md`; transformations include a small lineage and codebook.

On Unix, `.nlss` starts private (0700), objects owner-readable (0400), and the
working copy retains source permissions with owner-write enabled. These are local
permissions, not encryption or tamper-proof storage. Back up `.nlss` **with** the
visible files: it contains history, not disposable cache. Users retain filesystem
authority; missing/moved registered files require explicit reconciliation, not
automatic scanning or relinking.

## Authored reports

User-supplied scientific documents are ordinary agent inputs, with no prescribed
filename, inspection, capture or note field. The agent interprets them alongside
actual saved results and writes a visible report. As part of requested delivery,
[project-report](project-report.md) preserves its exact bytes and selected evidence,
without a separate user-facing finalization or copied run bundles.

Use [project-inspect](project-inspect.md) to find the root protocol, data, saved
analysis/utility outputs and authored-report revisions. Browsing shows recorded
status and availability, not a certification of evidence integrity.

## Publication and recovery

Normal runs take one managed analysis lock; publication also takes a short
managed publication lock. Before replacing the visible Parquet, NLSS rechecks
the starting working hash and registration hashes. A detected conflict leaves
the user's file alone and retains the candidate object and calculated results.
The CLI exits nonzero; the run is not a successfully published analysis:
`status: failed`, `calculation_status: completed`,
`publication_status: conflict_not_applied`, and `data_change.applied: false`.
`partial-output.md` preserves calculation output without presenting it as a
successfully applied change. Check the JSON state before using such evidence.

A same-directory temporary working copy is hash-checked and renamed, then the
run bundle is published. On an ordinary bundle-publication error, rollback uses
the already preserved input object, not another permanent backup. It restores
only a working file still matching this attempt's candidate; a further external
edit is left untouched and `manual_recovery_required` is recorded. Failed runs
use `applied: null` when recovery is uncertain, not a false claim of rollback.
Registration identity changes may prevent finalization entirely; then the
pending bundle remains diagnostic evidence for explicit recovery. Failed runs
retain diagnostics and candidates; no automatic garbage collection occurs.

This is **not crash-atomic multi-file recovery**, a distributed lock, or a lock
against external editors. A check/rename race remains with uncooperative writers.
Abrupt termination can leave a lock, `.pending-*` run or staging file; initialization
publishes the marker last and can leave an incomplete `.nlss` and working file.
Do not retry by deleting metadata, reimporting or blindly restoring the source.
Inspect running processes and frozen input/candidate hashes, obtain direction,
and recover only exact known targets. A pending directory is never a completed run.
Native Windows atomic replacement has not been validated; if rename fails, the
operation fails safely instead of truncating the current file.

## Replay

```bash
Rscript scripts/R/replay_run.R \
  --request /path/to/study/.nlss/runs/<run-id>/request.json
```

Replay preserves the original input, creates another run, and never
activates an old transformation output. Whole-project relocation retains relative
references. Data/dictionary/artifact hashes and the existing exact
code/environment gates remain mandatory. Changed code requires the matching
historical installation; no replay gate is relaxed for this layout. Hashes prove
byte association/integrity, not an independent signature or scientific validity.

Dependencies are the existing bootstrap, `arrow`, `yaml`, `jsonlite`, `digest`
and established scientific/import dependencies. No daemon or new service is used.
