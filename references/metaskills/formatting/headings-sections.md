---
name: formatting-headings
description: NLSS format heading levels, section labels, and Markdown mapping for NLSS reports.
---

# Headings and Section Labels (NLSS format)

## Overview

Apply NLSS format heading hierarchy and section labels in Markdown for NLSS reports. NLSS format is inspired by APA 7 and aims to approximate it in Markdown. This guidance focuses on how to format headings (Levels 1-5), apply Title Case, and handle run-in headings in Markdown-first outputs.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this when you need to:

- Structure a report into sections and subsections.
- Apply NLSS format heading levels.
- Format section labels (e.g., Abstract, References) in Markdown.

## Inputs/Clarifications

- Which sections are needed (e.g., Abstract, Method, Results, Discussion)?
- How many heading levels are required?
- Does the renderer support centering and run-in indentation?

## Core Workflow

1. Identify the section hierarchy (main sections vs. subsections).
2. Assign heading levels (Level 1 for main sections, Level 2 for subsections, etc.).
3. Apply Title Case to all headings.
4. Render headings using the Markdown mapping below.
5. Verify the order of levels (do not skip levels) and spacing around headings.

## Markdown Mapping (Action Steps)

- Level 1: `# Heading` (centered by renderer if supported)
- Level 2: `## Heading`
- Level 3: `### Heading`
- Level 4: `**Heading.**` followed by text on the same line (run-in)
- Level 5: `**_Heading._**` followed by text on the same line (run-in)

## Section Labels

- Use a dedicated label line for section labels (e.g., Abstract, References).
- Format the label in bold and rely on the renderer for centering.
- Do not add an "Introduction" heading; the paper title serves as the first heading.

## Renderer Notes

- Markdown does not enforce centering or run-in indentation. If needed, apply renderer styles or templates.
- Keep a blank line above and below each heading block to avoid Markdown merging.

## Markdown Paragraph Separation

- Use a blank line (two consecutive `\n`) between paragraphs and between block elements (tables, figures, lists, block quotes) to prevent Markdown from merging elements.

## Checks

- Headings proceed in order (1 -> 2 -> 3 -> 4 -> 5).
- All headings use Title Case.
- Run-in headings (Levels 4-5) end with a period and continue on the same line.
- Section labels are bold and on their own line.
