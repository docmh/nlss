---
name: format-document
description: Agent-run NLSS format pass that standardizes a report using NLSS formatting guides and produces a journal-ready Markdown output.
---

# Format Document (Agent-Run, NLSS format)

## Overview

This metaskill formats an existing report so it conforms to NLSS format rules that are representable in Markdown. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. It applies the NLSS formatting guides in a fixed order and produces a clean, journal-ready Markdown report without altering substantive content.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks to:

- "Format this report in NLSS format."
- "Make the report NLSS format-compliant."
- "Standardize headings, citations, tables, and references."

## Inputs/Clarifications

### Inputs

- Report source (path to a Markdown report or workspace report).
- Target output filename (default: `<filename>-formatted.md`).
- Whether to preserve or normalize existing section order.
- Whether to update citations and the reference list when entries are missing or inconsistent.

### Clarifying Questions

- Which report should be formatted (draft, `report_canonical.md`, or a prior metaskill report)?
- Should any sections be added or removed (e.g., Abstract, References)?
- Are citations and references final, or should missing entries be flagged for revision?
- Are tables/figures embedded in the report or stored as external files?

## Core Workflow

1. Identify the report source and intended output name (`<filename>-formatted.md`).
2. If the report is tied to a dataset workspace, inspect the dataset, summarize candidate variables in `scratchpad.md`, and note any constraints (do not alter data or statistics).
3. Log activation with `metaskill-runner`.
4. Inspect the report and list formatting issues by section in `scratchpad.md`.
5. Apply formatting guides in the ordered sequence below.
6. Normalize paragraph separation: ensure a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes).
7. Write the formatted report to `<filename>-formatted.md`.
8. Run `metaskill-runner --phase finalization --synopsis "<text>"` to append a `# Synopsis` to `report_canonical.md` (runner fails if the report is missing).

## Decision Hygiene

- Base formatting decisions on prior observations in `scratchpad.md` or the report; preserve limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance) and make them clearer, not less visible.
- If limitations are implied but not stated, flag them in `scratchpad.md` rather than changing substantive content.

## Formatting Sequence (Required Order)

Apply each guide in order and record changes in `scratchpad.md`:

1. `nlss/references/metaskills/formatting/headings-sections.md`
2. `nlss/references/metaskills/formatting/text-style-capitalization.md`
3. `nlss/references/metaskills/formatting/lists-seriation.md`
4. `nlss/references/metaskills/formatting/numbers-statistics.md`
5. `nlss/references/metaskills/formatting/in-text-citations.md`
6. `nlss/references/metaskills/formatting/quotations.md`
7. `nlss/references/metaskills/formatting/tables-figures.md`
8. `nlss/references/metaskills/formatting/reference-list.md`

Use `nlss/references/metaskills/formatting/` as the global reference for Markdown-compatible NLSS format rules.

## Procedure (Pseudocode)

```
resolve report_source
resolve output_path = <filename>-formatted.md

if workspace exists:
  open <dataset>/scratchpad.md
  inspect dataset and summarize candidate variables
  note report target and scope

run metaskill-runner --meta format-document --intent <intent>

read report_source
write plan to scratchpad.md

apply headings/sections guide
apply text style/capitalization guide
apply lists/seriation guide
apply numbers/statistics guide
apply in-text citations guide
apply quotations guide
apply tables/figures guide
apply reference list guide
ensure blank lines between paragraphs and block elements (use `\n\n`)

verify NLSS format Markdown constraints using the formatting guides
save formatted report to output_path

run metaskill-runner --phase finalization --synopsis "<synopsis>"
```

## Outputs

- `<filename>-formatted.md`: Formatted NLSS format-ready report.
- `report_canonical.md`: Updated only by the `metaskill-runner` finalization synopsis.
- `analysis_log.jsonl`: Metaskill activation and finalization entries.
- `scratchpad.md`: Formatting plan, decisions, and completion notes.

All outputs must be written inside the dataset workspace folder when a workspace is used.

## Finalization

- Generate `<filename>-formatted.md` first.
- Run `metaskill-runner --phase finalization --synopsis "<text>"` after the report exists.
- Ensure the final report aligns with the formatting guides in `nlss/references/metaskills/formatting/`.
