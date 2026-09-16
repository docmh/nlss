---
name: run-demo
description: Guided NLSS onboarding that explains capabilities, initializes a demo workspace from the golden dataset, and offers starter prompts.
license: Apache-2.0
---

# Run Demo (Agent-Run, NLSS onboarding)

## Overview

This metaskill provides a friendly, guided onboarding to NLSS. It explains NLSS capabilities (workspace-first workflow, subskills, outputs), **asks for explicit permission** to create a demo workspace from the sample `golden_dataset.csv`, and then guides the user conversationally with starter prompts so they can learn how to work with NLSS.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

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
invitingly and clearly explain NLSS capabilities (workspace-first, subskills, metaskills, outputs, logs)
ask permission to create demo workspace

if permission denied:
  provide starter prompts only
  stop (no scripts, no logging)

if permission granted:
  briefly explain the next steps and note that setup can take a little time
  create a fresh demo project folder under the user's chosen working location
  resolve assets/sample-data/golden_dataset.csv relative to the installed skill
  leave the bundled CSV untouched; do not create an intermediate conversion/workspace
  copy the bundled CSV into that folder as demo.csv; preserve the installed sample
  run project-create --project <demo> --source <demo>/demo.csv --working data/current.parquet --name demo
  use the returned dataset name for the following analysis (demo for a fresh project)
  write a short demo plan to scratchpad.md
  inspect dataset (data-explorer --project <demo> --dataset demo --user-prompt <last user message>)
  summarize progress updates in scratchpad.md

  provide friendly guidance:
    - where files live (visible working data, root report_canonical.md, .nlss/ evidence, optional scratchpad)
    - how to run common tasks
    - provide natural language starter prompts that invite experimentation with NLSS:
      - 3 should cover subskills
      - 3 should cover metaskills
      - only include variables in the starter prompts which are actually present in golden_dataset (see results of data-explorer)
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

- Root `report_canonical.md`: automatically appended NLSS-format output from `data-explorer`.
- `.nlss/runs/`: the saved exploration request/results; no extra JSONL log.
- `data/current.parquet`: the independent, visible working dataset; original source unchanged.
- `scratchpad.md`: Demo plan, dataset notes, and completion summary.

If permission is **not** granted, the response is conversational only (no files are created or modified).

### Final Report Requirements

This metaskill does **not** create a standalone metaskill report file. All onboarding guidance is delivered conversationally, and the only persistent artifacts (if permission is granted) are the current project files created by `project-create` and `data-explorer`.

Use the common project locations in `SKILL.md`. `project-create` imports the
bundled CSV directly; its working Parquet needs `arrow`. The common preflight
reports missing packages and the agent requests approval for their installation.
No manual conversion is needed. Do not use the retired
`init-workspace` writer, adopt an existing marker or create nested demo projects.
No compatibility reader or automatic cleanup is part of onboarding.

## Finalization

No metaskill finalization step. Do **not** run `metaskill-runner` and do **not** create a metaskill report for this onboarding flow.

## NLSS format Templates

This metaskill does not define its own NLSS format template. It relies on the templates configured for the subskills it invokes:

- `data-explorer` uses `assets/data-explorer/default-template.md` (if run).

## NLSS format Reporting Guidance

- Explain what NLSS is and what it can do (subskills, metaskills, workspace-first outputs, reporting/logging).
- Summarize the demo workspace creation (dataset used, files created, and where to find them).
- Provide 6 natural language starter prompts (3 for subskills and 3 for metaskills) that invite the user to experiment and learn how to work with the agent.

## Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).
