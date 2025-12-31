---
name: formatting-in-text-citations
description: NLSS format author-date in-text citation rules for narrative and parenthetical formats.
license: Apache-2.0
---

# In-Text Citations (NLSS format)

## Overview

Apply NLSS format author-date in-text citations in NLSS narratives. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. This covers narrative vs. parenthetical forms, multi-author rules, group authors, and special cases (no date, unknown author).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Cite a source in narrative or parenthetical form.
- Handle multiple authors or group authors.
- Cite multiple works in a single parenthesis.

## Inputs/Clarifications

- Author list (including group authors and abbreviations).
- Publication year (or no date).
- Whether the citation is narrative or parenthetical.
- Whether a direct quote is used (requires a locator).

## Core Workflow

1. Choose narrative vs. parenthetical citation based on sentence structure.
2. Apply author rules (1 author, 2 authors, 3+ authors).
3. Add year and required suffixes (e.g., 2020a, 2020b).
4. Add locators for direct quotes (page, paragraph, or section).
5. Verify every in-text citation has a matching reference list entry.

## Author Rules

- One author: (Smith, 2020) or Smith (2020).
- Two authors: use "and" in narrative; use "&" in parenthetical.
- Three or more authors: use first author + "et al." in all citations.
- Group authors: spell out the full name at first mention and include the abbreviation; use the abbreviation thereafter.

## Multiple Works in One Citation

- Order alphabetically by first author.
- Separate works with semicolons.

## Special Cases

- Same author, same year: add letter suffixes (2020a, 2020b).
- Unknown author: use a shortened title in place of the author.
- No date: use "n.d.".

## Direct Quotes

- Include a locator: page (p. 12), pages (pp. 12-14), paragraph (para. 4), or section heading.
- For short quotes, keep the quote in the sentence with quotation marks.
- For long quotes, follow the block quote guidance (see `quotations.md`).

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Every in-text citation has a reference list entry.
- Narrative vs. parenthetical formatting is consistent.
- Et al. is used correctly for 3+ authors.
- All direct quotes include a locator.
