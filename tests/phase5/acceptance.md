# Phase 5 native release acceptance

This is a bounded manual/live acceptance sheet, not a new test runner or a
report template. Start from a selected built release; register results against
its `release.json` payload hash. Automated dependency and maintenance coverage
is registered separately in `tests/tests.yml`.

## Scope and setup

- Five primary local harness journeys: Codex CLI, Claude Code, VS Code Copilot,
  Antigravity (record the exact surface), Vibe. One additional short Codex IDE
  skill-discovery probe. Other surfaces and Windows/macOS are not inferred.
- Prefer temporary client configuration/profiles and separate disposable study
  folders. Do not change the active personal NLSS install or existing projects.
  Use documented isolation options; if a client lacks one, obtain a scope
  decision before altering personal registration. Never copy credentials.
- Record OS, harness/version, surface, actual model (or explicitly unavailable),
  installed path, archive/payload identity, command or UI actions, start/end,
  exit/status, approval prompts and any available usage. Authentication happens
  locally through the client's normal flow, not in the evidence log.
- Preserve first failures. Allow one correction round and no more than three
  affected live rechecks. No repeated sampling until a model passes.
- Use a fresh model conversation and open only the study folder, not a parent
  containing development notes, evaluator evidence or other harness results.
  Installed release guidance and normal R libraries are legitimate inputs;
  internal Wave plans, reference answers and temporary test-library workarounds
  are not. Folder separation is not an OS sandbox: inspect observed tool access,
  record scope violations or possible reference leakage separately from numeric
  correctness, and do not claim independent acceptance from hashes alone.

## Native installation/discovery

Use [installation guidance](../../references/installation.md). Verify the native
manager's own installed/enabled result and discovery in a fresh client session.
For standalone routes use the packaged helper and native skill discovery.
Manifest validation, calling an internal parser, copying files, and explicitly
providing a SKILL path are useful separate checks, **not** UI activation proof.
Record those distinctions; do not label missing clients/accounts as NLSS defects.

For the short Codex IDE probe: activate the standalone skill in an isolated
profile or an explicitly approved user location, reload the extension, confirm
`nlss` is selectable, and ask what it can do without creating a project.
Record actual extension/version and visible activation. CLI app-server discovery
alone does not establish this result. The Codex desktop app is another surface,
not a replacement for the IDE check or an automatically required sixth journey.

For the 16 September 2026 acceptance, the researcher additionally authorized
short native Codex CLI and App activation/explanation checks. These supplement
the original explicit-path CLI research journey, not repeat it. Their scope is
instruction loading without a supplied path and file-effect inspection; record
unnecessary reads and harness-created directories separately from NLSS writes.
The researcher accepted closure with Antigravity functionally tested but with
its documented scope violation/unresolved development-document exposure retained.
This is a release-specific exception, not a general waiver of scoped execution.

## One research scenario per primary harness

Prepare a fresh folder containing the release's
`assets/sample-data/golden_dataset.csv` and, optionally, the ordinary study-note
fixture `tests/data/project/research_note.md`. Hash inputs before starting.
The note is user-supplied context, never a discovery/registration feature.

Suggested research request (not a required report layout):

> Use NLSS on the raw data in this folder. The research note describes the
> synthetic study. Focus on H1: does mean post_score differ from mean pre_score?
> Use available paired observations, no imputation or composite scoring. Make
> one suitable figure, explain the result and save a concise freely authored
> study_report.md. Handle project setup and report delivery. This is a synthetic
> demonstration, not a literature manuscript; no new literature search is needed.

The harness must use the installed payload, not development code, tests or
reference answers. Review the root `report_canonical.md`, saved request/results,
figure and authored report. Check:

1. Raw source unchanged; working data visible outside `.nlss/`; automatic setup.
2. Selected paired test and figure completed through the installed launcher.
3. Saved values agree with an independent base-R paired `t.test` reference on
   the same raw rows. Verify sample/missingness, direction, CI, p and effect-size
   definition, not just whether the result is significant.
4. Root protocol contains the selected runs; protocol/report artifact links
   resolve from their own locations; actually view the figure.
5. Report interpretation matches the design, uncertainty and synthetic context,
   with no invented causality, scoring or literature. No prescribed headings,
   exact wording or module-specific prose template.
6. Authored Markdown is preserved byte-for-byte by `project-report`, selecting
   the actual runs. Integrity preservation does not certify interpretation.

Ask a short follow-up about the existing CI and causality. Do not request a new
analysis or report. Compare project file names/hashes before and after: no data
reload, install, new run, revision or protocol append. Prefer the same session;
if a fresh-context read-only probe is used, record that limitation rather than
claiming same-session conversational continuity.

## Maintenance and handoff

Reuse unchanged Wave 1/2 evidence. In disposable installs, update to the selected
release, inspect the existing scenario project from the updated payload, and
compare historical/project file hashes. Do not rerun statistics or migrate the
project merely to update NLSS. Existing exact-code replay gates remain intact.

Complete the [maintainer checklist](../../packaging/RELEASE_CHECKLIST.md). Report
each boundary as verified, prepared/not live-tested or blocked, with reasons.
If authenticated/UI journeys remain unavailable, explicitly obtain acceptance
of a limited-support candidate or keep all-platform acceptance open. No public
release, tag, commit, push or marketplace submission is authorized by this sheet.
