---
name: run-demo
description: Guided NLSS onboarding that explains capabilities, initializes a demo workspace from the golden dataset, and offers starter prompts.
---

# Run Demo (Agent-Run, NLSS onboarding)

## Overview

This metaskill provides a friendly, guided onboarding to NLSS. It explains NLSS capabilities (workspace-first workflow, subskills, outputs), **asks for explicit permission** to create a demo workspace from the sample `golden_dataset.csv`, and then guides the user conversationally with starter prompts so they can learn how to work with NLSS.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

Exception for this metaskill: **only** ask for permission to create the workspace; do not ask any other clarifying questions unless the user explicitly invites them. Also: do not run `metaskill-runner` and do not create a metaskill report file for this onboarding flow.

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
explain NLSS capabilities (workspace-first, subskills, metaskills, outputs, logs)
ask permission to create demo workspace

if permission denied:
  provide starter prompts only
  stop (no scripts, no logging)

if permission granted:
  briefly explain the next steps and note that setup can take a little time
  run init-workspace --csv assets/sample-data/golden_dataset.csv --user-prompt <last user message>
  write a short demo plan to scratchpad.md
  inspect dataset (data-explorer)
  summarize progress updates in scratchpad.md

  provide friendly guidance:
    - where files live (scratchpad.md, report_canonical.md, analysis_log.jsonl, parquet)
    - how to run common tasks
    - provide natural language starter prompts that invite experimentation with NLSS:
      - 3 should cover subskills
      - 3 should cover metaskills
      - only include variables in the starter prompts which are already defined in golden_dataset.csv (see results of data-explorer)
```

## Default Rules and Decision Logic

- Always ask permission before creating a workspace. Do not ask any other follow-up questions.
- Default dataset path is `assets/sample-data/golden_dataset.csv` unless the user explicitly requests the test copy.
- Keep the demo lightweight: start with simple, easy to-understand analyses and avoid complex modeling.
- Update `scratchpad.md` with the demo plan before running scripts and mark progress after each step.
- When running any subskill, pass `--user-prompt` with the last user message for traceability.
- Do not run `metaskill-runner` and do not create any `report_<YYYYMMDD>_run-demo_<intent>.md` file for this metaskill.

## Outputs

When permission is granted and the demo runs:

- `report_canonical.md`: NLSS format entries from `init-workspace` and optional `data-explorer`.
- `analysis_log.jsonl`: JSONL entries for `init-workspace` and optional `data-explorer`.
- `scratchpad.md`: Demo plan, dataset notes, and completion summary.

If permission is **not** granted, the response is conversational only (no files are created or modified).

### Final Report Requirements

This metaskill does **not** create a standalone metaskill report file. All onboarding guidance is delivered conversationally, and the only persistent artifacts (if permission is granted) are the standard workspace files created by `init-workspace` and `data-explorer`.

Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`).
All artifacts (reports, tables, figures) must be created inside the dataset workspace folder; do not write outside the workspace root.

## Finalization

No metaskill finalization step. Do **not** run `metaskill-runner` and do **not** create a metaskill report for this onboarding flow.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `init-workspace` uses `assets/init-workspace/default-template.md`.
- `data-explorer` uses `assets/data-explorer/default-template.md` (if run).

## NLSS format Reporting Guidance

- Explain what NLSS is and what it can do (subskills, metaskills, workspace-first outputs, reporting/logging).
- Summarize the demo workspace creation (dataset used, files created, and where to find them).
- Provide 6 natural language starter prompts (3 for subskills and 3 for metaskills) that invite the user to experiment and learn how to work with the agent.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
