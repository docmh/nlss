# Phase 3: preserved semantic report revisions

This focused suite checks ordinary delivery preservation of freely authored Markdown and
read-only browsing or explicit verification of its evidence references. It neither generates a report
nor certifies its interpretation, statistical correctness or exact replayability.
The visible manuscript remains author-owned and editable.

Run once the implementation is stable:

```bash
Rscript tests/phase3/run_report_tests.R --root /tmp/nlss-report-check --keep 0
```

`--match REGEX` selects affected groups. `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and
`NLSS_TESTS_CONFIG` follow the existing Phase 3 runner conventions. Explicit roots
are never pruned; default retention only removes completed directories positively
identified as this suite's own work. Run the existing persistence/inspection
checks as warranted by changes to their shared contracts, not as a claim that
every statistical method was rerun.

The runner builds one real managed seed from the registered golden dataset and
ordinary synthetic study document, executes descriptive statistics and a transformation,
then gives each case a private copy. Authored UTF-8/CRLF Markdown includes arbitrary
structure and quoted inert code. No source repository configuration, template or
existing research project is modified.

Task D also checks report-path identity, normal agent-side save without an action
or report/revision ID, general statistical/explicit-file/parameter/utility evidence,
read-only cross-session browsing with missing artifacts, explicit verification,
and ordinary unknown-option rejection of retired note switches.

Task E adds current-project navigation checks: root protocol and working-data
links; multiple statistical procedures, parameter-only Power, Calc and a real
dataset-bound lifecycle CLI invocation through the existing publisher; authored
report revisions; whole-project relocation with encoded Markdown links; missing,
malformed, oversized, pending, nested and POSIX symlink/FIFO records. Traps ensure
routine browsing does not load data, hash evidence, scan objects or publish
anything. Only the current marker/layout is accepted. Dataset registration names
are displayed alongside recorded input versions.

Focused E/common-consumer selection:

```bash
Rscript tests/phase3/run_report_tests.R --root /tmp/nlss-report-check --keep 0 --match '^task_e_|^task_d_'
```

The supplemental `metaskill_flags_` cases exercise explicit project selection
from another project, nearest-ancestor selection, an explicit dataset overriding
the active default, rejection without fallback/writes, root protocol placement
with no parallel current-project JSONL, repeat-call lock cleanup, preserved foreign
locks, cleanup after data-loading failure, explicit finalization preserving visible
report bytes, and unchanged standalone/current-project source-file semantics.
These utility CLI checks form part of Task F's consolidated selection; they do
not constitute a new statistical migration.

Coverage includes:

- Exact report bytes, deterministic revision identity and idempotency independent
  of selected-run ordering and finalizer implementation fingerprint. Repeated
  finalization preserves object/descriptor counts and file mtimes.
- Authored-report revisions and immutable earlier prose; ordinary document edits
  do not produce note captures or new revisions.
- Read-only inspection with unchanged, changed, missing or unselected visible
  text. Preserved evidence verification does not certify today's edited claims.
- Old-engine and explicitly replay-ineligible evidence remains usable without
  calling a data loader, statistical executor, replay gate or Markdown renderer.
  These cases modify private fixture records and recompute their request binding;
  they are simulated historical evidence, not authentic runs from an old engine.
- Whole-project moves and mutable dataset names/working locators do not rewrite
  historical requests or report revisions.
- Missing, failed, pending, duplicate and foreign-identity run selections;
  tampered requests, outputs, input objects, report objects and report records.
- Unsafe identifiers/paths, nested-project boundaries, FIFO/symlink report or
  descriptor inputs, and bounded CLI preflight failures. POSIX inventories use
  `test -f` before hashing; unsupported OS fixtures are explicitly skipped.
- Foreign publication locks and injected final descriptor rename failure preserve
  authored files and referenced analyses; a retry publishes one valid revision.
- Document growth/emptying at actual capture and edits after capture, with bounds
  checked at the point of use. Separate document/run/marker edits injected after
  revision serialization must also prevent publication without being overwritten.
  A locally reduced post-serialization limit tests
  oversized final records without constructing enormous fixtures. Pending evidence
  remains distinguishable from published revisions.
- Rehashed malformed revision metadata still cannot claim semantic certification,
  or use unsafe historical locators.

`results.json` records the selected command/cases, source and fixture SHA-256s,
environment, start/end times, actual subprocess exits, assertions and skips from
the outset. CLI stdout/stderr are bounded to 16,000 characters each with explicit
truncation flags. Direct in-memory fault and read-only checks are identified by
their case assertions. A source-drift gate prevents certifying a run executed
against changing files. Native Windows execution is only claimed when performed.
