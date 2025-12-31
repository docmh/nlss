---
name: formatting-tables-figures
description: NLSS format table and figure numbering, titles, notes, and Markdown layout.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2026 Mike Hammes"
  nlss.version: "1.0.0"
---

# Tables and Figures (NLSS format)

## Overview

Format tables and figures according to NLSS format rules using Markdown-first layouts. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. Apply this when placing tables and figures in NLSS reports.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Create or place a table or figure.
- Number and title tables and figures.
- Add general, specific, or probability notes.

## Inputs/Clarifications

- Table or figure number in order of appearance.
- Title text (Title Case).
- Notes required (general, specific, probability).
- Renderer capabilities for captions and spacing.

## Core Workflow

1. Number tables and figures in the order they are mentioned.
2. Add a bold number line and an italicized Title Case title line.
3. Insert the table or figure immediately after the title.
4. Add notes below in the correct order.
5. Place the table or figure near its first mention in text.

## Tables

- Use Arabic numerals (Table 1, Table 2, ...).
- Table number is bold; title is italicized in Title Case on the next line.
- Notes appear below the table in this order: general note, specific notes, probability notes.

## Figures

- Use Arabic numerals (Figure 1, Figure 2, ...).
- Figure number is bold; title is italicized in Title Case on the next line.
- Place the image below the title; add legends within the figure if needed.
- Notes appear below the figure.

## Markdown Implementation

- Keep a blank line between the title block and the table/figure block.
- Use a "Note." paragraph below the table/figure for notes.

Example layout:

```md
**Table 1**

*Title in Title Case*

| Column | Column |
| --- | --- |
| ... | ... |

*Note.* General note text. Specific notes use superscripts. Probability notes use asterisks.
```

```md
**Figure 1**

*Title in Title Case*

![Alt text describing the figure](path/to/figure.png)

*Note.* General note text.
```

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Table/figure numbers are in order of first mention.
- Titles are Title Case and italicized.
- Notes follow the correct order and format.
- The table/figure is placed near its first mention.
