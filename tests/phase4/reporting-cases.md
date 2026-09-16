# Semantic reporting: manual acceptance cases

This is a manual behavioral test sheet, not an automated passing suite, report
template or required answer structure. Registration: `tests.plans.phase4_reporting`
in [tests.yml](../tests.yml). Use the current [NLSS skill](../../SKILL.md).

## Execution and fixtures

Use one fresh-context forward-testing agent at a time. Give it the skill,
the researcher prompt below (substituting actual paths) and necessary artifacts;
**do not give it this sheet, reviewer criteria or an intended answer**. The main
reviewer checks actual responses and filesystem effects. Case 5 retains context
for its short follow-up; all other cases start fresh. Repeat Cases 1 and 3 in
fresh contexts: eight scenarios total. Permit one instruction-correction round
and at most four targeted rechecks; retain failures, not just successful samples.

Use one private validation root and the existing registered fixtures:

- `tests.golden_dataset`: synthetic `tests/data/golden_dataset.csv`.
- `tests.phase3.study_document`: ordinary `tests/data/project/research_note.md`.
- For Case 4, copy only those two files into a fresh unmarked `study/` folder.
  Let the forward agent perform initialization and analyses.
- For Cases 1, 3 and 6, import the golden CSV into a private `evidence/` project
  with `project_create.R`. Run `regression.R --dv outcome_reg` with `--ivs x1,x2`
  (primary), `--ivs x1,x2,mediator` (adjusted) and `--ivs x1,x2,x3` (focused x3).
  Use genuine completed requests/results, not hand-written result records.
  No additional data fixture is required. Record actual generated run paths.
- Reuse the Case 4 report/project for Case 5. Prepare human edits in its visible
  report, leaving saved revisions intact: a valid researcher interpretation to
  retain, and one unsupported study-design assertion to discuss.
- Before Case 6 only, change `outcome_reg` to its negative in the private
  evidence project's working data through the existing `data_transform.R`
  overwrite/confirmation flags, then repeat the primary regression. Keep the
  older results and raw CSV intact. Name a specific absent residual-plot path
  as the requested supporting artifact; do not fabricate an artifact record.

Keep prompts, responses/reports and a short review in the private validation
root. Record file hashes/paths and run/revision counts around each task. Inspect
each empirical numerical statement and table cell against its source; read the
reasoning for material omissions and unsupported claims. Different defensible
wording is expected. No heading, phrase or caveat checklist is an answer oracle.

## Forward-agent prompts

### 1. Focused result question

> Use NLSS to explain the x3 coefficient in this saved regression: FOCUSED_RUN.
> I'm a researcher and need a brief interpretation of direction and uncertainty.
> Does this establish that x3 has no association with outcome_reg? Please answer
> here, using this selected result.

Fresh-context repeat:

> For x3 in FOCUSED_RUN, what can I responsibly tell a colleague about the
> association with outcome_reg? A short professional answer here is enough.

### 2. Conceptual conversation

> Use NLSS to help me understand an interaction. I'm a first-year psychology
> student, with no dataset yet. How can a score improve in both groups but still
> show a time-by-group interaction? A small made-up example would help.

### 3. Cross-model synthesis

> Use NLSS to compare these saved primary and adjusted regressions:
> PRIMARY_RUN and ADJUSTED_RUN. The outcome is outcome_reg and the focal
> predictor is x1; x2 is included in both, and the second also includes the
> variable named mediator. These are synthetic observational data, and the
> causal ordering of the variables is not established. Write a concise Results
> subsection here with a small comparison table and interpretation. Use the
> selected outputs; no new analysis is requested.

Fresh-context repeat:

> I need an explanation for a research-team discussion of what changes when
> mediator is included alongside x1 and x2 in these two models: PRIMARY_RUN,
> ADJUSTED_RUN. Same synthetic observational setting; causal ordering is unknown.
> Please give a concise comparison here, with each model's estimate for x1,
> interval and sample size, and the scientific takeaway.

### 4. Folder-first study report

> Please use NLSS on the data in this folder. My research_note.md describes the
> study and hypotheses. Analyze H1 (pre-to-post change) and H2 (whether this
> change differs between group2 groups). Use available paired pre/post
> observations; no imputation or questionnaire-composite construction. This is
> a synthetic demonstration; I need a concise methods/results/discussion report
> at study_report.md, using the study context and the results, not a full
> literature manuscript or new literature search. Please take care of the
> project setup and report delivery for me.

### 5. Human-edited report

> I've edited study_report.md. Please tighten the Discussion for readability
> while retaining my interpretation and the rest of the report. Do not change
> or rerun the analyses. Deliver the revised report at the same path.

Follow-up, same context:

> Now do a formatting-only pass using NLSS conventions. Keep the substantive
> wording and numbers unchanged, including any issue we have not resolved.
> Deliver it at the same path.

### 6. Evidence selection and limits

> Use NLSS to explain x1 in the older primary run PRIMARY_RUN. I know the
> working data and other runs are newer, but this is the result I mean.
> Also inspect RESIDUAL_PLOT_PATH if available and tell me what you can say
> about the diagnostics. Please answer here, without running analyses or
> recovering files.

## Reviewer-only criteria (never sent to the forward agent)

1. The x3 coefficient is near zero, with uncertainty covering either direction.
   Match actual estimate/interval/p and model identity. Nonsignificance does
   not establish no association/equivalence. No dataset read, rerun, new report,
   protocol append, scratchpad or project initialization merely to explain it.
2. Explain unequal changes despite improvement in both groups at a novice's
   level. Toy numbers must be clearly hypothetical and arithmetically correct;
   distinguish an illustrative difference from demonstrated statistical evidence.
   No files, project setup or R execution. No mandated pedagogical structure.
3. Primary x1 is about 0.531, adjusted about 0.188; both use 221 observations
   with this fixture. Match each CI, p and adjustment set to its own run.
   Discuss attenuation/changed conditioning without claiming mediation or
   causality from a variable name. Do not invent a pooled estimate or a test of
   the difference between coefficients. No rerun or unsolicited file creation.
4. Check original CSV/note bytes, visible editable working data, root protocol,
   completed run evidence, authored report and saved revision. Check all
   reported empirical numbers and contrast directions. Methods must reflect
   actual complete-case/missing/group choices; don't invent randomization,
   scoring/validation facts, preregistration, literature search or causal benefit.
   The note is an ordinary input, not a new feature. Read links as evidence,
   not a semantic certificate. No custom statistical implementation.
5. Preserve the valid human interpretation; flag the unsupported design claim
   without silently replacing the researcher's science. Formatting-only must
   preserve wording/numbers. Save actual delivered revisions with selected
   existing evidence; prior revisions, runs, raw data and working data stay
   unchanged. No numerical rerun or protocol append for authoring/formatting.
6. Use the older positive x1 result, not the later negative-outcome fit or
   current data. Clearly qualify the absent plot; an available Shapiro result
   alone cannot establish all model assumptions or observed residual shape.
   No invented verification, recovery, reanalysis or state change.

## Scope and complementary checks

Use quality-first agentic literature research when a task requests or needs it;
adequate supplied sources can support a bounded write-up. These six cases do not
require a new literature search or benchmark live source-service availability.
They do not certify full-manuscript literature appraisal or general LLM reliability.

Run the existing registered report-consumer subset once on stable source:

```bash
Rscript tests/phase3/run_report_tests.R \
  --root /tmp/nlss-phase4-report-check --keep 0 \
  --match '^task_d_ordinary_delivery|^task_d_lightweight_cross_session|^task_e_current_project_view'
```

This structural check is separate from semantic review. No full statistical
matrix or production-code changes are required. Report actual scenario outcomes,
corrections, elapsed resources and limits; passing these local cases is not
human-user testing, native-Windows testing or a model reliability rate.
