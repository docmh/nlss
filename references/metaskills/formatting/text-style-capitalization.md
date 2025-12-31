---
name: formatting-text-style
description: NLSS format capitalization, italics, and quotation mark usage for NLSS narratives.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2025 Mike Hammes"
  nlss.version: "1.0.0"
---

# Text Style and Capitalization (NLSS format)

## Overview

Apply NLSS format rules for capitalization, italics, and quotation marks in NLSS narratives. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. Use this to format titles, headings, and special terms consistently.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Apply Title Case or sentence case.
- Decide between italics and quotation marks.
- Format titles of works in text vs. reference list entries.

## Inputs/Clarifications

- Is the title used in text or in the reference list?
- Is the work standalone (book, report, journal) or part of a larger work (article, chapter)?
- Are you introducing a new technical term?

## Core Workflow

1. Determine whether Title Case or sentence case is required.
2. Apply italics or quotation marks based on work type.
3. Use italics for standalone works and quotation marks for parts of larger works.
4. Avoid italics or quotation marks for emphasis.

## Capitalization Rules

- Title Case for paper titles, headings, table titles, and figure titles.
- Sentence case for titles of works in the reference list.
- Capitalize the first word after a colon, em dash, or end punctuation in a title.

## Italics vs. Quotation Marks

- Italicize standalone works (books, reports, theses/dissertations, journals, websites, software).
- Use quotation marks for works that are part of a larger whole (journal articles, book chapters, web pages).
- Use italics for words used as linguistic examples or newly introduced technical terms (when clarity requires).

## Markdown Implementation

- Italics: `*Title*`
- Quotation marks: "Title"
- Avoid using italics or quotation marks for emphasis.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Title Case vs. sentence case is applied consistently.
- Italics and quotation marks match the work type.
- Titles in the reference list use sentence case.
