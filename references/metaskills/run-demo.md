---
name: run-demo
description: Guided NLSS onboarding that explains capabilities, initializes a demo workspace from the golden dataset, and offers starter prompts.
---

# Run Demo (Agent-Run, NLSS onboarding)

## Overview

This metaskill provides a friendly, guided onboarding to NLSS. It first explains NLSS capabilities (workspace-first workflow, subskills, outputs), then **asks for explicit permission** to create a demo workspace from the sample `golden_dataset.csv`. After setup, it offers natural language starter prompts that encourage the user to build experiences about how to work with the agent in NLSS.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

Exception for this metaskill: **only** ask for permission to create the workspace; do not ask any other clarifying questions unless the user explicitly invites them.

## Intent/Triggers

Use this metaskill when the user asks for onboarding, a demo, or guidance on how to start with NLSS, for example:

- "Give me a quick demo of NLSS."
- "How do I get started with this repo?"
- "Show me what NLSS can do, then set up a sample workspace."
- "Walk me through a starter workflow." 

## Routing Guardrails

- If the user asks for **actual analysis results**, route to a specific metaskill (`describe-sample`, `explore-data`, `screen-data`, etc.).
- If the user wants only **concept explanations**, use `explain-statistics`.
- If the user wants a full report with hypotheses, use `write-full-report`.

## Inputs/Clarifications

### Inputs

- Demo dataset path (default: `assets/sample-data/golden_dataset.csv`).
- User permission to create a workspace (required).
- Optional intent label to describe the demo focus.

### Clarifying Question (Only One)

- Do you want me to create a demo workspace now? (required)

If the user does **not** grant permission, stop after the capability overview and starter prompts (no workspace creation, no logging).

## Procedure (Pseudocode)

```
explain NLSS capabilities (workspace-first, subskills, outputs, logs)
ask permission to create demo workspace

if permission denied:
  provide starter prompts only
  stop (no scripts, no logging)

if permission granted:
  briefly explain the next steps and note that setup can take a little time
  ensure workspace root is valid (no nested/sibling manifests)
  run init-workspace --csv <assets/sample-data/golden_dataset.csv> --user-prompt <last user message>
  run metaskill-runner --meta run-demo --intent <intent> --phase activation

  write a short demo plan to scratchpad.md
  optionally inspect dataset (data-explorer) only if explicitly requested by the user
  summarize progress updates in scratchpad.md

  provide friendly guidance:
    - where files live (scratchpad.md, report_canonical.md, analysis_log.jsonl, parquet)
    - how to run common tasks
    - 4 natural language starter prompts that encourage experiential use of NLSS

  write report_<YYYYMMDD>_run-demo_<intent>.md (NLSS format-ready, onboarding-focused)
  align report using references/metaskills/formatting/align-report.md
  run metaskill-runner --phase finalization --synopsis "<short demo synopsis>"
```

## Default Rules and Decision Logic

- Always ask permission before creating a workspace. Do not ask any other follow-up questions.
- Default dataset path is `assets/sample-data/golden_dataset.csv` unless the user explicitly requests the test copy.
- Keep the demo lightweight: avoid running analysis subskills unless the user explicitly asks for them.
- Do not run `data-explorer` unless the user explicitly requests a scan.
- Update `scratchpad.md` with the demo plan before running scripts and mark progress after each step.
- When running any subskill, pass `--user-prompt` with the last user message for traceability.

## Outputs

When permission is granted and the demo runs:

- `report_canonical.md`: NLSS format entries from `init-workspace`, optional `data-explorer`, and metaskill activation/finalization logs.
- `analysis_log.jsonl`: JSONL entries for `init-workspace`, optional `data-explorer`, and metaskill activation/finalization.
- `scratchpad.md`: Demo plan, dataset notes, and completion summary.
- `report_<YYYYMMDD>_run-demo_<intent>.md`: A friendly, onboarding-focused report that explains capabilities, records workspace setup, and lists starter prompts.

If permission is **not** granted, the response is conversational only (no files are created or modified).

### Final Report Requirements

- Do not copy `report_canonical.md`; write a new narrative report.
- Use `assets/metaskills/report-template.md` as the default structure, but replace academic sections with onboarding-appropriate headings when needed (for example, Overview, Workspace Setup, Starter Prompts, Next Steps).
- Keep the report NLSS format-ready and suitable for journal submission when applicable (for a demo, focus on clarity and traceability).

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

- Write `report_<YYYYMMDD>_run-demo_<intent>.md` using an ASCII slug for `<intent>` (finalization fails if this report is missing).
- Align the report using `references/metaskills/formatting/align-report.md` (must be the last step before finalization).
- Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` section to `report_canonical.md`.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `init-workspace` uses `assets/init-workspace/default-template.md`.
- `data-explorer` uses `assets/data-explorer/default-template.md` (if run).
- `metaskill-runner` uses `assets/metaskill-runner/default-template.md` for activation and `assets/metaskill-runner/finalization-template.md` for finalization logging.

## NLSS format Reporting Guidance

- Explain what NLSS is and what it can do (subskills, workspace-first outputs, reporting/logging).
- Summarize the demo workspace creation (dataset used, files created, and where to find them).
- Provide 4 natural language starter prompts that invite the user to experiment and learn how to work with the agent (for example, planning, clarifying variables, choosing analyses, and interpreting results).

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
