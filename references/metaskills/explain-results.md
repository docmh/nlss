---
name: explain-results
description: Conversational, researcher-friendly interpretation of selected statistical outputs in context, without rerunning analyses.
license: Apache-2.0
---

# Explain Results (Agent-Run, Conversational)

## Overview

Explain the results the researcher actually selected, at the depth their question
needs. Follow the shared [semantic answer/report guidance](../../SKILL.md#semantic-answers-and-authored-reports).
A short answer, a detailed interpretation and a requested Results section need
not share a structure. No data load, project setup, lifecycle event or new
analysis is required merely to explain existing evidence.

## Intent/Triggers

Examples include “What does this table mean?”, “How should I interpret these
coefficients?”, “Is this effect meaningful?” or “Why do these models disagree?”
For concept-focused teaching, use [explain-statistics](explain-statistics.md).

## Inputs and Consequential Gaps

Use the selected table, screenshot, excerpt or saved output and available study
context. Relevant context may include the model/contrast, outcome and predictor
coding, reference category, units, scale direction, analysis sample and design.
Read the relevant saved request/result/dictionary when needed to interpret NLSS
output; do not substitute a newer run or today's working data.

Clarify only gaps that affect the answer. If the selected evidence is readable
but incomplete, explain what it supports and qualify the remaining uncertainty.
A pasted table is supplied material, not a verified NLSS run. Do not require
project IDs, all dataset details or an audience questionnaire before answering.

## Interpretation Decisions

- Address the actual question with the relevant estimates and uncertainty.
  Choose depth, notation and organization for the researcher; no prescribed
  recap length, paragraph sequence or closing question.
- Keep effect direction tied to coding and the actual comparison. Practical
  importance depends on the scale and scientific context, not just a p-value.
  Nonsignificance does not establish equivalence or absence of an effect.
- Use design and available diagnostics to judge the scope of inference. Do not
  infer causality from association or describe an unavailable check as passed.
  Explain material limits, not a universal list of caveats.
- When comparing models, retain each model's sample, covariates, estimand and
  uncertainty. Discuss disagreements and plausible explanations as such; do
  not claim to have demonstrated their cause. Significance in one model but not
  another does not by itself establish a difference between their estimates.
  Do not fabricate a pooled result or a formal comparison.
- Use literature support when needed for the requested interpretation or
  explicitly requested. Read and assess adequate supplied sources; use
  [research-academia](../utilities/research-academia.md) for missing relevant
  support. Disclose unavailable support instead of inventing citations.
- If the question actually requires a new estimate, test or diagnostic, distinguish
  that from explaining the selected output. Use the appropriate existing NLSS
  procedure when execution is within the request; otherwise describe the gap
  and ask before expanding to an analysis.

## Delivery

Answer in the conversation unless documentation is requested. Do not append to
the root protocol, update a scratchpad or save a report revision merely for an
explanation. A requested note can be written directly; no plan/completion log is
required just to retain the explanation.

For a requested formal write-up, use a chosen visible Markdown path and
[format-document](format-document.md) as appropriate. If based on saved NLSS
results, preserve the actual selected evidence through
[project-report](../utilities/project-report.md). Supplied-only material needs
honest source attribution, not fabricated run IDs or a statistical run to make
it saveable. No new report template or separate finalization step.
