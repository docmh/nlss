---
name: project-report
description: Preserve an authored Markdown report with its selected saved evidence as part of ordinary delivery, or browse and explicitly verify revisions.
license: Apache-2.0
---

# Authored reports and saved evidence

The agent writes the report at the researcher's chosen visible Markdown path.
This utility preserves those exact bytes and references the completed outputs
actually used. It does not generate prose, parse claims, impose a template or
certify scientific correctness. Reports remain editable outside `.nlss/`.

## Ordinary delivery

When a researcher requests a report, author it and save its evidence as part of
that same task. Do not ask them to choose internal run/revision IDs or request a
separate finalization. The agent obtains the run IDs from the outputs it used;
never automatically select all runs or guess the latest analysis.

Read user-supplied study documents with ordinary agent capabilities. Any filename
works; there is no note discovery, flag, fingerprinting or automatic capture.
Distinguish supplied design/theory/hypotheses from computed findings and
interpretation. Check reported numbers, populations, units and citations against
the selected evidence. Preserve meaningful scientific context in the authored
report itself, not in a new note-management feature.

1. Read the requested context and the relevant saved results. The automatically
   maintained root `report_canonical.md` is the default SPSS-like evidence view.
2. Write the requested synthesis at a visible path, with useful evidence links.
   Choose structure for the research question; do not merely concatenate outputs.
3. Invoke the existing report saver with that path and the actual evidence IDs.
4. Deliver a link to the visible report and a concise scientific summary. Report
   a failed save honestly; the visible draft remains usable but is not a saved
   revision. No extra lifecycle log or protocol synopsis is required for this save.

This workflow uses the current project layout for every procedure through common
records. No statistical module has to opt into report storage.

## Agent-side command

```bash
Rscript scripts/R/project_report.R --project /path/to/study \
  --report Manuscript.md --runs RUN_ID1,RUN_ID2
```

- Default `--action save`: requires an existing nonempty Markdown `--report`
  and a comma-separated `--runs` selection of completed statistical or utility
  records. Empty, duplicate, missing, ambiguous and incomplete selections fail.
- Omit `--project` for nearest-ancestor discovery. No child/sibling search.
- Report paths are project-relative. Saving derives the report ID from the
  normalized relative path unless an optional advanced `--report-id SLUG` is
  supplied. No mutable registry or latest pointer.
- The generated `report_canonical.md` is not an authored report capture target.
- Success is one JSON object with `status: saved|already_saved`, `report_id`,
  `revision_id`, `record_path`, `report` and the limited capture verification
  scope. Errors use stderr and a nonzero exit.
- `--help` documents the interface. Retired `--context-note` and inspector
  `--note` are ordinary unknown options; `--action finalize` is not an alias.

Paths cannot traverse symlinks, special files, managed storage or nested projects.
Default limits in `scripts/config.yml`: 8 MiB/report, 32 MiB/evidence JSON.
Ordinary validated `NLSS_CONFIG_PATH` overrides can change those limits.

## Browse or explicitly verify

Opening the visible report requires no command or verification step. Optional
history access uses the same report path, including in a later session:

```bash
Rscript scripts/R/project_report.R --action inspect --project /path/to/study --report Manuscript.md
Rscript scripts/R/project_report.R --action inspect --project /path/to/study \
  --report Manuscript.md --revision rev-HASH
# Only when a full evidence check is requested:
Rscript scripts/R/project_report.R --action inspect --project /path/to/study \
  --report Manuscript.md --revision rev-HASH --verify TRUE
```

Listing returns existing revision descriptors and their saved object/evidence
paths, with `status: listed`. Selecting a revision returns `status: read`.
Both check descriptor structure/identity but label evidence `not_checked`;
they do not read every run, load data or require today's working file.
The record's capture-time verification claim is not a current verification.

Explicit `--verify TRUE` requires one revision. It checks its report object,
originally pinned request/result hashes, common completed-run association,
registered run-local artifacts/templates and input data/dictionary hashes.
It returns `status: verified` only on success. Missing or damaged dependencies
produce an error, without repairing or replacing references.

Verification is byte integrity at these boundaries, not a dependency-graph audit,
re-execution, replay eligibility, estimator/lineage validation or semantic
certification. No statistical objects or expressions are executed. Exact replay
retains its own stricter code/environment/input gates. A report can be browsed
even if a linked artifact has subsequently gone missing; browsing must not call
that evidence verified.

An optionally supplied visible report is compared with the selected revision;
edits or missing live text do not invalidate preserved Markdown. Whole-project
moves retain relative references. Renaming a visible report does not automatically
relink its history: inspect its previous relative path or saved report ID.

## Storage and limits

The existing `.nlss/reports/<report-id>/rev-<hash>.json` pins one report object
in `.nlss/objects/<sha256>` and each selected run's request/result references.
Datasets, run bundles and figures are referenced, not copied. Identical report
bytes/evidence reuse the existing descriptor without rewriting its mtime.
An edited report or changed evidence selection creates a new small descriptor;
unchanged content objects are reused. Saving is part of delivery, not every
keystroke or incidental draft edit.

Capture checks the selected evidence once. Before publication, it rechecks the
pinned request/result files, visible report bytes, report object and marker.
Existing locks and staged rename protect the write; a failure cannot replace
the authored report or source data. Interrupted publication can retain pending
metadata/objects for explicit recovery. This is not a crash-atomic transaction
or protection against arbitrary concurrent external edits.

No additional log, index, watch service, report backup family or utility bundle.
Attachments/relative images/literature URLs in arbitrary Markdown are not
automatically packaged. Open a preserved object's exact bytes through the
returned path; relative images are intended for the visible report's location.
Keep the project and `.nlss/` together. Older schemas are not imported or
converted, and existing archived bytes are not rewritten.

Dependencies: existing `yaml`, `jsonlite`, `digest` and shared helpers;
report capture/browsing does not load Arrow or historical analysis packages.
Developer coverage: `tests/phase3/REPORTS.md` in the source checkout (test files
are not part of the installed runtime).
