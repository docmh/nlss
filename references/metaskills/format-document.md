---
name: format-document
description: NLSS format specification and formatting workflow for producing journal-alike Markdown reports.
license: Apache-2.0
---

# Format Document (NLSS Format Specification)

## Overview

This document is the single source of truth for NLSS format. It consolidates all formatting rules into one sequence that is easy to apply. NLSS format is inspired by APA 7 and is designed to be implemented in Markdown while remaining readable in plain text.

Use this guide to format a report so it conforms to NLSS format rules that are representable in Markdown. The goal is a clean, journal-alike Markdown report without altering substantive content.

## Scope and Principles

- NLSS format approximates APA 7 in Markdown; it is not a replacement for final publisher layout.
- Preserve substantive content (results, interpretations, limitations); only adjust formatting.
- Apply rules in the order listed below so headings, text style, lists, numbers, citations, quotes, tables/figures, and references remain consistent.

## Inputs and Clarifications

- Report source (path to a Markdown report or workspace report).
- Target output filename (default: `<filename>-formatted.md`).
- Whether section order should be preserved or normalized.
- Whether citations and references should be reconciled (missing entries flagged, not invented).

Clarify if needed:
- Which report should be formatted (draft, `report_canonical.md`, or a prior metaskill report)?
- Should any sections be added or removed (e.g., Abstract, References)?
- Are citations and references final, or should missing entries be flagged?
- Are tables/figures embedded or referenced externally?

## Formatting Workflow (Required Order)

Before applying rules, scan the report by section to identify formatting issues that need correction.

1. Headings and section labels
2. Text style and capitalization
3. Lists and seriation
4. Numbers and statistical notation
5. In-text citations
6. Quotations
7. Tables and figures
8. Reference list
9. Paragraph separation and final checks

## 1) Headings and Section Labels

### Heading Levels and Markdown Mapping

- Level 1: `# Heading` (centered by renderer if supported)
- Level 2: `## Heading`
- Level 3: `### Heading`
- Level 4 (run-in): `**Heading.**` followed by text on the same line
- Level 5 (run-in): `**_Heading._**` followed by text on the same line

Rules:
- Use Title Case for all headings.
- Do not skip heading levels (1 → 2 → 3 → 4 → 5).
- Run-in headings (Levels 4–5) end with a period and continue on the same line.
- Use dedicated, bold section label lines for labels such as **Abstract** and **References** (centered by renderer if supported).
- Do not add an “Introduction” heading; the paper title serves as the first heading.
- Keep a blank line above and below each heading block to avoid Markdown merging.

Renderer notes:
- Markdown does not enforce centering or run-in indentation; rely on renderer styles or templates.

## 2) Text Style and Capitalization

### Capitalization Rules

- Title Case: paper titles, headings, table titles, figure titles.
- Sentence case: titles of works in the reference list.
- Capitalize the first word after a colon, em dash, or end punctuation in a title.

### Italics vs. Quotation Marks

- Italicize standalone works: books, reports, theses/dissertations, journals, websites, software.
- Use quotation marks for parts of larger works: journal articles, book chapters, web pages.
- Use italics for words used as linguistic examples or newly introduced technical terms (when clarity requires).
- Avoid italics or quotation marks for emphasis.

Markdown:
- Italics: `*Title*`
- Quotation marks: "Title"

## 3) Lists and Seriation

### List Types

- Ordered lists: use `1.` for each item (Markdown auto-numbers).
- Unordered lists: use `-` for each item.
- In-text lists: use `(a)`, `(b)`, `(c)` for short items within a sentence.

### Punctuation and Capitalization

- Full sentences: capitalize the first word and end each item with a period.
- Short phrases: keep a consistent grammatical form; omit end punctuation unless the item completes the lead-in sentence.
- Items with internal commas: use semicolons between items in in-text lists.
- Ensure list formatting does not break paragraph flow; keep a blank line before and after list blocks.

## 4) Numbers and Statistical Notation

### Numerals vs. Words

- Use words for numbers zero through nine; use numerals for 10 and above.
- Always use numerals for measurements, time, dates, ages, percentages, ratios, and scale points.
- If a number starts a sentence, spell it out or rephrase the sentence.

### Decimals and Leading Zeros

- Use a leading zero for values that can exceed 1 (e.g., 0.75).
- Omit the leading zero for values that cannot exceed 1 (e.g., p values, correlations).

### Statistical Symbols and Tests

- NLSS rule: statistical symbols are plain text only (no italics, bold, or special styling).
- Examples: t, F, r, p, M, SD, N, n.
- Use superscript ² for squared statistics (e.g., R², f², eta²).
- Use parentheses for test statistics and degrees of freedom.
- Use brackets for confidence intervals.
- Use spaces around operators (e.g., t(45) = 2.14).

### p Values (Default)

- Report exact p values to two or three decimals when possible.
- Use p < .001 for very small values.

## 5) In-Text Citations

### Narrative vs. Parenthetical

- Narrative: Smith (2020) reported…
- Parenthetical: (Smith, 2020)

### Author Rules

- One author: (Smith, 2020) or Smith (2020).
- Two authors: use “and” in narrative; use “&” in parenthetical.
- Three or more authors: use first author + “et al.” in all citations.
- Group authors: spell out the full name at first mention and include the abbreviation; use the abbreviation thereafter.

### Multiple Works in One Citation

- Order alphabetically by first author.
- Separate works with semicolons.

### Special Cases

- Same author, same year: add letter suffixes (2020a, 2020b).
- Unknown author: use a shortened title in place of the author.
- No date: use “n.d.”.

### Direct Quotes

- Include a locator: page (p. 12), pages (pp. 12–14), paragraph (para. 4), or section heading.
- Short quotes stay in the sentence with quotation marks.
- Long quotes follow block quote rules (see next section).

## 6) Quotations

### Short Quotes (< 40 Words)

- Keep the quote within the sentence.
- Use double quotation marks.
- Place the citation immediately after the quote.
- Preserve the original wording and punctuation.

### Block Quotes (40+ Words)

- Start on a new line.
- Indent the entire block (0.5 in) and double-space (renderer-controlled).
- Do not use quotation marks.
- Place the citation after the final punctuation.
- Indent subsequent paragraphs within the block an additional 0.5 in if needed.
- Preserve the original wording and punctuation.

Markdown:
- Use blockquote syntax (`>`) to indicate indentation.
- Insert a blank line before and after the block quote.

## 7) Tables and Figures

### Numbering and Placement

- Number tables and figures in the order they are mentioned (Table 1, Figure 1, etc.).
- Place each table or figure near its first mention in text.

### Table Format

- Use Arabic numerals: **Table 1**.
- Table number is bold; title is italicized in Title Case on the next line.
- Insert the table immediately after the title.
- Notes appear below the table in this order: general note, specific notes, probability notes.

### Figure Format

- Use Arabic numerals: **Figure 1**.
- Figure number is bold; title is italicized in Title Case on the next line.
- Place the image below the title; add legends within the figure if needed.
- Notes appear below the figure.

Markdown layout example:

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

Additional table/figure rules:
- Keep a blank line between the title block and the table/figure block.
- Use a "Note." paragraph (formatted as *Note.*) below the table or figure for notes.

## 8) Reference List

### Reference Section Label

- Add a bold, centered **References** label on its own line.

### Ordering and Author Rules

- Alphabetize entries by the first author’s surname.
- Format author names as surname, initials.
- List up to 20 authors.
- If 21 or more authors, list the first 19, then an ellipsis, then the final author.
- Use commas between authors and an ampersand before the final author.

### Title and Source Rules

- Article, book, and report titles use sentence case.
- Journal titles use Title Case and italics.
- Italicize the source (journal, book, report), not the article title.

Markdown notes:
- Use a blank line between entries only if the renderer enforces double spacing globally.
- If hanging indents are unsupported, apply indentation via export templates or post-processing.

## 9) Paragraph Separation and Final Checks

### Paragraph Separation Rule

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

### Final Checks

- Headings are Title Case and in a consistent hierarchy.
- Run-in headings (Levels 4–5) end with a period and continue on the same line.
- Statistical symbols are plain text (no italics or special styling).
- Squared statistics use ² where appropriate (R², f², eta²).
- Tables/figures are numbered and titled correctly with notes in order.
- Every in-text citation has a matching reference list entry.
- References are alphabetized and formatted consistently.
- Paragraphs and block elements are separated by blank lines.

## Output Notes

- Produce a formatted Markdown report that follows this specification.
- Do not alter substantive content; only adjust formatting.
