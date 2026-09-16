# Utility and lifecycle evidence

Use this reference for Calc, literature retrieval and explicitly requested
metaskill lifecycle events. Their audit is deliberately different from
the [statistical run contract](run-contract.md).

## What is recorded

The current project receives
`.nlss/utility-runs/<id>/request.json`, `result.json`, `output.md` and module-specific
artifacts. Requests are explicitly `kind: utility`, record the module, timestamp,
resolved request, production R-code hash and execution environment, and declare
automatic statistical replay ineligible. Results bind request/output/artifact
bytes with SHA-256. This is association/integrity evidence, not a digital signature.

- Calc preserves expressions, bindings, results and value types. Restricted
  calculations are deterministic given their recorded inputs/environment;
  unrestricted R may access external state and have side effects.
- Research preserves selected sources, query/filter settings, source/page
  statuses, sanitized response snapshots and ranked results. A repeated network
  request is a new retrieval, not reproduction of an earlier search. API keys
  and full configuration are not audit content. Fixture transport is explicitly
  identified and never silently used for a live search.
- Metaskill events record activation or finalization, relevant dataset references
  and exact authored report bytes when finalized. Their audit does not generate
  or certify the semantic interpretation.

The shared publisher automatically extends the root `report_canonical.md`.
No additional current-project JSONL journal is written. `--log FALSE` does not
suppress evidence or the protocol. Neither Calc nor research fabricates a dataset.
Lifecycle events remain explicit utilities, not required steps for ordinary
authored-report delivery; that uses `project-report` below.
History readers stay standalone and create no utility/statistical runs or
older-project conversion path.

## Failure and recovery boundaries

Only published directories are terminal evidence. `result.status` distinguishes
completed, partial and failed retrieval; a failed retrieval retains
`diagnostic-output.md`, not normal successful output. `.pending-*` remains
incomplete even if it contains success-shaped intermediate files. Invalid
requests that fail before staging may have only stderr/nonzero exit evidence.

Publication uses the existing project-level `.publication-lock`, protected
temporary recovery copies of declared publication targets, staged artifacts and a final
directory rename. Ordinary callback, integrity or rename failures restore old
projection bytes and remove newly created projections. Foreign locks are not
removed. Symlink paths and targets outside the declared project are rejected.

If ordinary recovery itself fails, the pending result reports
`projections_restored: false`; its `recovery_backup` directory retains original
bytes and `targets.json` maps them to project-relative targets. Inspect the
active processes, exact targets and retained evidence before a user-authorized
recovery. Do not delete a pending bundle or lock merely because it looks old.

This does not promise crash-atomic multi-file recovery, protection from
uncooperative external writers, or rollback of unrestricted R side effects.
Human-authored final reports remain semantic syntheses, not reproducible templates.

## Project utilities without duplicate logs

[project-inspect](utilities/project-inspect.md) stays entirely read-only and
links existing current-layout records. It is not a background index or audit.
[project-create](utilities/project-create.md) records the dataset/source
descriptor and initial shared version, publishing the current marker last;
it does not duplicate registration in an additional utility bundle.
The older `init-workspace` marker writer is unsupported; do not use it to create
or convert a current project.

[project-report](utilities/project-report.md) similarly uses one authoritative
managed report-revision record plus a content-shared Markdown object, not an
additional utility bundle/log. It binds the actual selected statistical/utility/MI/report evidence
without replaying it or certifying the authored semantic interpretation. Its
inspection action is entirely read-only; the visible report stays user-editable.
