---
name: formatting-lists
description: NLSS format seriation rules for ordered, unordered, and in-text lists in Markdown.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2026 Mike Hammes"
  nlss.version: "1.0.0"
---

# Lists and Seriation (NLSS format)

## Overview

Apply NLSS format seriation rules for ordered lists, unordered lists, and in-text lists. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. Use this when formatting steps, procedures, or grouped items in NLSS narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Present steps or procedures.
- List items without an inherent order.
- Format short in-text lists within a sentence.

## Inputs/Clarifications

- Is order meaningful (steps, chronology) or not?
- Are list items full sentences or short phrases?
- Should the list be embedded in text or in its own block?

## Core Workflow

1. Decide between ordered vs. unordered list.
2. Determine whether items are sentences or phrases.
3. Apply consistent capitalization and punctuation.
4. Use in-text seriation when a full list block is unnecessary.
5. Ensure list formatting does not break paragraph flow.

## Markdown Implementation

- Ordered lists: use `1.` for each item (Markdown auto-numbers).
- Unordered lists: use `-` for each item.
- In-text lists: use `(a)`, `(b)`, `(c)` for short items within a sentence.

## Punctuation and Capitalization

- Full sentences: capitalize the first word and end each item with a period.
- Short phrases: keep a consistent grammatical form; omit end punctuation unless the item completes the lead-in sentence.
- Items with internal commas: use semicolons between items in in-text lists.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Ordered lists are used only when sequence matters.
- Unordered lists are used when order does not matter.
- Punctuation style is consistent within each list.
- In-text seriation uses letters in parentheses.
