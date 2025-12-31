---
name: formatting-numbers-statistics
description: NLSS format number rules, decimal formatting, and statistical notation for NLSS reports.
license: Apache-2.0
---

# Numbers and Statistical Notation (NLSS format)

## Overview

Apply NLSS format rules for numbers, decimals, and statistical notation in NLSS reports. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. Use this when writing results, tables, and narrative summaries.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Intent/Triggers

Use this when you need to:

- Decide between numerals and words.
- Format decimals, percentages, and statistical notation.
- Report test statistics and p values.

## Inputs/Clarifications

- Measurement units, scale ranges, and reporting precision.
- Whether the number starts a sentence.
- Which statistics are being reported (e.g., t, F, r).

## Core Workflow

1. Apply the numeral vs. word rules.
2. Format decimals and leading zeros consistently.
3. Keep statistical symbols in plain text (no italics or special styling) and format test statistics.
4. Report exact p values when possible; use thresholds for very small values.
5. Apply spacing around operators.

## Numerals vs. Words

- Use words for numbers zero through nine; use numerals for 10 and above.
- Always use numerals for measurements, time, dates, ages, percentages, ratios, and scale points.
- If a number starts a sentence, spell it out or rephrase the sentence.

## Decimals and Leading Zeros

- Use a leading zero for values that can exceed 1 (e.g., 0.75).
- Omit the leading zero for values that cannot exceed 1 (e.g., p values, correlations).

## Statistical Symbols and Tests

- **NLSS rule:** statistical symbols are plain text only (no italics, bold, or special styling) for readability in plain text.
- Examples: t, F, r, p, M, SD, N, n.
- Use superscript ² for squared statistics (e.g., R², f², eta²).
- Use parentheses for test statistics and degrees of freedom.
- Use brackets for confidence intervals.
- Use spaces around operators (e.g., t(45) = 2.14).

## p Values (NLSS Default)

- Report exact p values to two or three decimals when possible.
- Use p < .001 for very small values.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Numerals vs. words are consistent with NLSS format rules.
- Statistical symbols are plain text (no italics or special styling).
- p values and confidence intervals are formatted consistently.
