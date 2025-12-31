---
name: formatting-align-report
description: Step-by-step guide for aligning a finished report with NLSS format rules.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2025 Mike Hammes"
  nlss.version: "1.0.0"
---

# Align Report With Formatting Guides

## Overview

Use this helper to align a finished report with NLSS format rules. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. It is intended for metaskills that produce `report_<YYYYMMDD>_<metaskill>_<intent>.md` files and need a final formatting pass before metaskill finalization.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when a metaskill instruction says:
- "Align the report with the formatting guides."
- "Align report_<YYYYMMDD>_<metaskill>_<intent>.md with references/metaskills/formatting/."

## Inputs/Clarifications

- Path to the report file that should be aligned.
- Whether any sections are intentionally nonstandard (e.g., no Abstract).
- Whether any content must be left unchanged verbatim (quoted text, exported tables, fixed templates).

## Procedure (pseudocode)

```
open report file
scan for formatting issues by section

apply all formatting guides in order:
  headings-sections.md
  text-style-capitalization.md
  lists-seriation.md
  numbers-statistics.md
  in-text-citations.md
  quotations.md
  tables-figures.md
  reference-list.md

ensure blank lines between paragraphs and block elements (\n\n rule)
verify headings are Title Case
verify statistical symbols are italicized where required
verify tables/figures numbers, titles, notes
verify references are alphabetized and cited
save report
```

## Outputs

- Updated report file, aligned to NLSS format rules in `references/metaskills/formatting/`.
- `scratchpad.md` note indicating the alignment pass is complete (when applicable).

## Checks

- Headings are Title Case and in a consistent hierarchy.
- Statistical symbols are italicized in narrative text.
- Tables/figures are numbered and titled correctly with notes.
- References correspond to in-text citations.
- Paragraphs and block elements are separated by a blank line.
