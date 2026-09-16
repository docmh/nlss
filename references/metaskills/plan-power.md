---
name: plan-power
description: Agent-run study planning that clarifies design and effect-size justification, runs auditable Power calculations with or without pilot data, and writes a context-sensitive research report.
license: Apache-2.0
---

# Plan Power

## Intent and scope

Use for sample-size planning ("How many participants do we need?"), power at a
specified sample size, or detectable-effect sensitivity. Read the
[Power reference](../subskills/power.md) before execution. The supported families
are t-tests, one-way between-subjects ANOVA, Pearson correlation, omnibus linear
regression and RMSEA-based SEM power. A migrated SEM or mixed-model analysis does
not imply that its complete design has a matching power calculation here.
Route model diagnostics to `check-assumptions` and observed hypothesis tests to
`test-hypotheses`. Out-of-scope power designs require the user's permission before
using `generate-r-script`; do not silently substitute a simpler design.

## Clarify the research question

Resolve only the choices needed for the proposed calculation:

- Primary outcome/hypothesis and planned test, including independent versus
  paired observations, groups/allocation and the direction of a one-sided test.
- Effect metric and justified magnitude: relevant literature, approved pilot
  estimate, or a smallest scientifically meaningful effect. Conventional
  small/medium/large values are illustrative scenarios, not a substantive basis.
- Alpha, target power, sample-size constraints and any multiple-primary-test
  adjustment. Offer canonical defaults where useful and record their adoption.
- For SEM, model degrees of freedom and the null and alternative RMSEA values.
  RMSEA power is not power for an individual loading or structural path.

Distinguish researcher-specified assumptions from quantities estimated from
observed data. Pilot effects are uncertain; an apparent precise sample-size
answer must not hide uncertainty in its inputs.

## Choose the input branch

### Parameter-only planning

No data file or dummy dataset is needed. Use `--planning TRUE`.
In a current project, runs go to `.nlss/runs/` and automatically extend the
root protocol, just like other analyses. Keep useful rationale in an ordinary
visible scratchpad; pass agreed assumptions through `--effect-basis` and
`--user-prompt`. Unmarked standalone planning is also available without
silently creating/adopting a project.

Do not call `init-workspace` or a dataset-only lifecycle logger for planning.
Current projects use their existing marker and common output route. No separate
planning dataset, project-local protocol or JSONL journal is needed.

```bash
Rscript <scripts>/power.R --project <project> --planning TRUE --analysis ttest --mode apriori \
  --t-type two-sample --effect-metric d --effect-size 0.4 \
  --alpha 0.05 --power 0.8 --ratio 1 \
  --effect-basis "Researcher-approved minimum relevant effect" \
  --user-prompt "<last user request>"
```

### Dataset-backed planning

When the researcher approves pilot effect estimation, follow the
[import contract](../import-contract.md), inspect the dataset/codebook, confirm
variable roles and document missing-case handling in the dataset scratchpad.
Use an explicit source or `--planning FALSE` for an existing dataset context.
Pass `--estimate-effect TRUE` and the required variables; SEM does not support
pilot effect estimation. The resulting run preserves the actual data snapshot,
dictionary, cases and effective design.

Both branches follow the same ordinary report-delivery workflow below;
no metaskill activation or separate synopsis is required.

## Calculation and review

1. Document the agreed design, input source, effect-size basis and options.
2. Run the appropriate mode: a priori for sample size, sensitivity for a
   detectable effect at fixed N, or post hoc for power conditional on specified
   N and effect. Read warnings and the actual resolved request, not just the CLI.
3. When useful, run a few scientifically plausible effect/allocation scenarios
   and compare their run references. There is no mandatory scenario count or
   automatic scenario-grid engine. Consult primary literature when the effect
   justification needs it; do not invent a citation or a prior effect estimate.
4. Check total versus per-group N, rounding and attained power. Distinguish
   analyzable observations/pairs from recruitment targets and any separately
   justified allowance for attrition; no automatic attrition assumption is made.
5. Update the scratchpad with findings, decisions and limitations.

## Outputs and semantic finalization

Both branches produce `.nlss/runs/<id>/request.json`, `result.json`, `output.md`
and saved templates in a current project, with the automatic root protocol.
Use [verified replay](../run-contract.md) when repeating a completed recorded run.
`output.md` is deterministic SPSS-like output, not the final research report.

When a final report is requested, write it at a chosen visible Markdown path
and preserve delivery with its actual evidence through
[project-report](../utilities/project-report.md). Follow [NLSS formatting](format-document.md) but choose a structure
suited to the research question. Explain why the design and effect assumptions
fit the question, how uncertainty affects feasibility, and what the calculation
does and does not establish. Reference the actual runs, integrate comparisons,
and distinguish computed results from semantic interpretation.

Do not copy the canonical report or constrain the synthesis to template tokens.
Data-derived post-hoc power is not independent evidence that an observed result
or its sample is adequate. Do not turn a planning calculation into a causal
claim, a validated measurement model or a guarantee of study success.

For either input branch, summarize useful decisions in the scratchpad and the
requested report, without fabricating additional lifecycle log entries.
Always give the researcher a self-contained conversational summary.
