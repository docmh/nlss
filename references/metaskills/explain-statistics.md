---
name: explain-statistics
description: Conversational, student-friendly explanations of statistical concepts, methods, outputs, and interpretations without running analyses.
license: Apache-2.0
---

# Explain Statistics (Agent-Run, Conversational)

## Overview

This metaskill provides teacher-style explanations of statistical concepts, test logic, and interpretation. It is used when the user asks for clarification (for example, "How do custom contrasts work?") rather than for running analyses. It is conversational by default and intentionally skips `metaskill-runner` activation/finalization logs unless the user explicitly requests documentation.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Intent/Triggers

Use this metaskill when the user asks for an explanation of statistical concepts, outputs, or interpretation, for example:

- "How do custom contrasts work?"
- "Explain what an interaction term means."
- "What does a p-value tell me?"
- "When should I use Spearman instead of Pearson?"
- "How do I interpret this ANOVA table?"

## Inputs/Clarifications

### Inputs

- The concept, method, or output that needs explanation.
- Context (analysis type, model, design, or software output).
- Audience level (intro, intermediate, advanced).
- Desired depth (intuition, step-by-step mechanics, formulas, or reporting guidance).
- Optional: a specific output table, contrast definition, or snippet to interpret.

### Clarifying Questions

- What concept or output do you want explained?
- Which analysis or model is this from (ANOVA, regression, mixed model, SEM, etc.)?
- What is your current familiarity level?
- Do you want intuition, formulas, or step-by-step mechanics?
- Do you have a specific output table, contrast definition, or example we should use?

If the user does not specify a level, default to an intro-friendly explanation with minimal math.

## Procedure (Pseudocode)

```
identify the concept and user goal
if intent or context is ambiguous:
  ask 1-3 clarifying questions
choose depth based on audience and request
stay conversational; do not run `metaskill-runner` or R scripts unless the user requests documentation
explain in a teacher style:
  define in plain language
  provide intuition or a short analogy
  give a simple worked example (small numbers or a toy table)
  connect to interpretation and decisions
  list common pitfalls or misconceptions
  offer reporting tips when relevant
check understanding and invite follow-up questions

if the user requests citations or literature support:
  run research-academia (multiple query variants; curate sources)

if the user asks for an actual analysis:
  pivot to the appropriate NLSS subskill or metaskill
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Keep explanations concise and structured; expand only when asked.
- Define jargon the first time it appears.
- Use ASCII math and minimal notation unless the user requests formulas.
- If a specific output is provided, tie the explanation to the exact rows/columns.
- Avoid prescriptive decisions unless the user asks for recommendations.
- If the request turns into data analysis, use the appropriate NLSS subskill or metaskill.

## Outputs

- Primary output: a conversational explanation in the chat.
- `scratchpad.md`: only if the user requests documentation in a workspace context.
- `report_canonical.md`: only if the user explicitly asks to log the explanation; append a short "Explanation" entry manually (do not use `metaskill-runner`).
- `analysis_log.jsonl`: not used for conversational-only requests unless explicitly requested.
- `report_<YYYYMMDD>_explain-statistics_<intent>.md`: optional, only if the user asks for a formal write-up.

## Scratchpad Updates

- If documentation is requested, write a brief plan (concept, audience level, depth), then note completion after the explanation is delivered.
- Otherwise, do not update `scratchpad.md`.

## Finalization

- No finalization step is required for conversational use.
- If a formal report is requested, write `report_<YYYYMMDD>_explain-statistics_<intent>.md` (ASCII slug for `<intent>`), align it using `references/metaskills/format-document.md`, then append a brief `# Synopsis` to `report_canonical.md` manually (no `metaskill-runner`).

## NLSS format Templates

This metaskill does not define NLSS format templates. It is conversational and does not run subskills by default.

## Parquet Support

Not applicable unless the user requests a data-based example. If a dataset must be loaded, follow workspace parquet conventions and note that Parquet support requires the R package `arrow`.
