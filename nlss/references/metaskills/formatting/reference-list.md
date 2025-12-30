---
name: formatting-reference-list
description: APA 7 reference list ordering, author formatting, and Markdown implementation notes.
---

# Reference List (APA 7 Formatting)

## Overview

Format the APA 7 reference list in Markdown, including ordering, author formatting, and hanging indents. This guidance is used when generating or reviewing `References` sections in NLSS reports.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Build or review the References section.
- Format author lists and titles correctly.
- Apply hanging indents and alphabetical order.

## Inputs/Clarifications

- Full author list and publication year.
- Source type (journal article, book, report, web page).
- DOI or URL when applicable.
- Renderer support for hanging indents.

## Core Workflow

1. Add a bold, centered "References" label on its own line.
2. Alphabetize entries by the first author's surname.
3. Format author names as surname, initials.
4. Apply sentence case to work titles and Title Case to journal or source titles.
5. Apply hanging indents via renderer or export pipeline.

## Author List Rules

- List up to 20 authors in the reference list.
- If 21 or more authors, list the first 19, then an ellipsis, then the final author.
- Use commas between authors and an ampersand before the final author.

## Title and Source Rules

- Article, book, and report titles use sentence case.
- Journal titles use Title Case and italics.
- Italicize the source (journal, book, report), not the article title.

## Markdown Implementation

- Use a blank line between entries only if the renderer enforces double spacing globally.
- If the renderer does not support hanging indents, apply indentation via export templates or post-processing.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- References are alphabetized by first author.
- Every in-text citation appears in the reference list.
- Author name formatting and author count rules are applied correctly.
- Titles use the correct capitalization style.