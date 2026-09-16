---
name: explain-statistics
description: Context-sensitive explanations of statistical concepts and methods, from accessible intuition to technical detail, without requiring data or analysis.
license: Apache-2.0
---

# Explain Statistics (Agent-Run, Conversational)

## Overview

Help the researcher understand the concept or decision they are asking about.
Use the shared [semantic answer/report guidance](../../SKILL.md#semantic-answers-and-authored-reports).
The agent chooses the explanation's shape, not a teaching script. No dataset,
project setup, R execution or lifecycle event is required for a conceptual
conversation or simply to document it.

## Intent/Triggers

Examples include “How do custom contrasts work?”, “Explain an interaction term”,
“What does a p-value tell me?” or “When is Spearman appropriate?”
For interpretation of a particular empirical result, use
[explain-results](explain-results.md); conceptual explanation may accompany it.

## Context and Depth

Use the question and any supplied design, model or output to infer the useful
level of detail. Ask about familiarity or context only when it would materially
change the explanation. Do not impose an introductory level on a professional
researcher or require an audience questionnaire.

Plain language, an analogy, a worked illustration, formulas or a visual are
available choices, not obligatory stages. Explain unfamiliar terms when useful
and use notation when it clarifies the concept. A focused question may need one
paragraph; a request for a derivation may need considerable detail. There is no
fixed sequence, length, list of misconceptions or mandatory check-in question.

## Scientific Grounding

- Make clear what the concept means and when it applies, including distinctions
  or limitations material to the question. Simplification must not change its
  statistical meaning.
- Label invented example numbers as illustrative, not findings from the user's
  data. Keep any arithmetic consistent. An example does not establish evidence
  about the user's study.
- When a real output is supplied, distinguish its actual values and interpretation
  from the hypothetical illustration; read relevant rows and context rather
  than pretending to have checked the underlying data.
- Method-choice explanations can discuss tradeoffs without selecting or running
  a new analysis on the user's behalf. If actual computation is requested, use
  the appropriate existing NLSS procedure and its normal permissions.
- For requested citations or source-dependent claims, read and assess available
  sources. Use [research-academia](../utilities/research-academia.md) when relevant
  support is missing; do not force a fresh search when adequate sources were
  supplied or manufacture references when support is unavailable.

## Delivery

The normal output is an answer in the conversation, without new files, scratchpad
entries or protocol appends. If the researcher asks to retain the explanation,
write the selected note or document; documentation alone does not call for
statistics, metaskill activation or a plan/completion log.

Apply [format-document](format-document.md) only as appropriate to the requested
document. A report using saved NLSS results follows the existing
[project-report](../utilities/project-report.md) delivery workflow. A concept-only
document needs no fabricated evidence IDs, project initialization or statistical
run. No new response or report templates.
