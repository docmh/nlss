---
name: formatting-quotations
description: NLSS format short and block quotation formatting with Markdown guidance.
license: Apache-2.0
---

# Quotations (NLSS format)

## Overview

Format direct quotations using NLSS format rules, including short quotes and block quotes. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. Apply this when integrating quoted material into NLSS narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Include a direct quote.
- Decide between short quotes and block quotes.
- Place citations and locators correctly.

## Inputs/Clarifications

- Word count of the quote.
- Required locator (page, paragraph, or section).
- Whether the quote must be embedded in a sentence or set off as a block.

## Core Workflow

1. Count the words in the quote.
2. Use a short quote format for fewer than 40 words; use a block quote for 40+ words.
3. Add the citation and locator in the correct position.
4. Preserve the original wording and punctuation.

## Short Quotes (< 40 Words)

- Keep the quote within the sentence.
- Use double quotation marks.
- Place the citation immediately after the quote.

## Block Quotes (40+ Words)

- Start on a new line.
- Indent the entire block (0.5 in) and double-space.
- Do not use quotation marks.
- Place the citation after the final punctuation.
- Indent subsequent paragraphs within the block an additional 0.5 in if needed.

## Markdown Implementation

- Use blockquote syntax (`>`) to indicate indentation and rely on the renderer for spacing.
- Insert a blank line before and after the block quote.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Short vs. block quote threshold is applied correctly.
- Every quote includes a locator.
- Citation placement follows NLSS format rules for the quote type.
