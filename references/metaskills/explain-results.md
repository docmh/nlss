---
name: explain-results
description: Conversational, researcher-friendly explanations of the meaning of statistical results using the numbers provided in tables or outputs, without running analyses.
---

# Explain Results (Agent-Run, Conversational)

## Overview

This metaskill explains the meaning of statistical results that the user references (tables, model outputs, summaries). It focuses on interpreting the numbers in context (direction, magnitude, uncertainty, and practical meaning) without running analyses. It is conversational by default and intentionally skips `metaskill-runner` activation/finalization logs unless the user explicitly requests documentation.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this metaskill when the user asks for the meaning of results or output, for example:

- "What does this table mean?"
- "How should I interpret these coefficients?"
- "What does this *p*-value and CI imply?"
- "Is this effect meaningful?"
- "What is the practical meaning of this ANOVA result?"

## Inputs/Clarifications

### Inputs

- The specific output to interpret (paste, screenshot description, or file excerpt).
- The analysis or model type (t-test, ANOVA, regression, mixed model, SEM, etc.).
- Variable roles and coding (outcome, predictors, group labels, reference category).
- Scale direction and units (e.g., higher scores = better, or reversed items).
- Hypotheses or contrasts the result relates to.
- Optional: sample size, alpha level, and study design details.

### Clarifying Questions

- Which table or section should I interpret (model summary, post hoc, coefficients, fit indices)?
- What are the outcome and predictors or group labels, and how are they coded?
- What do higher scores mean for each variable?
- Which row(s) or comparison(s) matter most for your research question?
- Do you want focus on statistical significance, effect size, practical meaning, or all three?

If the user does not specify a level, default to a clear, research-grade explanation with minimal math and explicit ties to the numbers shown.

## Procedure (Pseudocode)

```
identify the result type and user goal
if output or context is ambiguous:
  ask 1-3 clarifying questions
choose depth based on audience and request
explain in a structured, conversational way:
  restate the comparison or parameter in plain language
  interpret direction and magnitude using the reported numbers
  interpret uncertainty (SE/CI) and evidence (p-value/test statistic)
  connect to practical meaning and the research question
  note assumptions, limitations, and avoid causal claims when not justified
summarize in 1-3 sentences
invite follow-up questions or deeper dives

if the user requests literature context or citations:
  run research-academia (multiple query variants; curate sources)

if the user requests a new analysis or re-computation:
  pivot to the appropriate NLSS subskill or metaskill
```

## Default Rules and Decision Logic

- Make step choices based on observed data limitations (e.g., small sample size, non-normality, outliers, missingness, group imbalance); adapt analyses or caveats and record the rationale in `scratchpad.md` (and in the final report if one is produced).
- Use only the numbers provided; do not invent values.
- Keep explanations accurate but accessible; avoid oversimplifying or trivializing.
- Distinguish statistical significance from practical importance.
- Avoid causal language unless the design supports it.
- Define jargon on first use and keep notation minimal unless requested.
- If results are ambiguous or missing key context, ask clarifying questions.

## Outputs

- Primary output: a conversational explanation in the chat.
- `scratchpad.md`: only if the user requests documentation in a workspace context.
- `report_canonical.md`: only if the user explicitly asks to log the explanation; append a short "Explanation" entry manually (do not use `metaskill-runner`).
- `analysis_log.jsonl`: not used for conversational-only requests unless explicitly requested.
- `report_<YYYYMMDD>_explain-results_<intent>.md`: optional, only if the user asks for a formal write-up.

## Scratchpad Updates

- If documentation is requested, write a brief plan (result type, key comparisons, interpretation focus), then note completion after the explanation is delivered.
- Otherwise, do not update `scratchpad.md`.

## Finalization

- No finalization step is required for conversational use.
- If a formal report is requested, write `report_<YYYYMMDD>_explain-results_<intent>.md` (ASCII slug for `<intent>`), align it using `references/metaskills/formatting/align-report.md`, then append a brief `# Synopsis` to `report_canonical.md` manually (no `metaskill-runner`).

## NLSS format Templates

This metaskill does not define NLSS format templates. It is conversational and does not run subskills by default.

## Parquet Support

Not applicable unless the user requests a data-based example. If a dataset must be loaded, follow workspace parquet conventions and note that Parquet support requires the R package `arrow`.
