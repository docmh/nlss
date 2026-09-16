---
name: reliability
description: Inter-rater/test-retest reliability with ICC (oneway/twoway, agreement/consistency), kappa (weighted), or test-retest correlations; wide/long formats, CIs, grouping, and NLSS format outputs.
license: Apache-2.0
---

# Reliability Analysis (Base R, NLSS format)

## Overview

Compute reliability for ratings or repeated measurements:

- **ICC** for continuous ratings (inter-rater or repeated measures).
- **Kappa** for categorical ratings (two raters).
- **Test-retest** correlations for stability across two time points.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose `--analysis` (icc/kappa/test_retest).
3. Choose `--format` (wide/long) and supply the required variables.
4. Run `scripts/R/reliability.R` with the correct flags.
5. Review the saved request, results, warnings and deterministic output; synthesize the findings in context rather than treating the output template as a final research report.

## Script: `scripts/R/reliability.R`

Run with `Rscript` and base R only.

### ICC (Wide Format)

```bash
Rscript <path to scripts/R/reliability.R> --csv <path to CSV file> --analysis icc --vars r1,r2,r3
```

### ICC (Long Format)

```bash
Rscript <path to scripts/R/reliability.R> --csv <path to CSV file> --analysis icc --format long --id id --rater rater --score score
```

### Kappa (Categorical)

```bash
Rscript <path to scripts/R/reliability.R> --csv <path to CSV file> --analysis kappa --vars rater1,rater2
```

### Test-Retest

```bash
Rscript <path to scripts/R/reliability.R> --csv <path to CSV file> --analysis test_retest --vars t1,t2 --method spearman
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--analysis` selects `icc`, `kappa`, or `test_retest` (default: `modules.reliability.analysis`).
- `--format` selects `wide` or `long` (default: `modules.reliability.format`).
- `--vars` lists the wide-format rating variables (required for `wide`).
- `--id`, `--rater`, `--score` define long-format IDs/raters/scores (required for `long`).
- `--group` runs analyses per group.
- `--missing` selects `complete` or `pairwise` (default: `modules.reliability.missing`).
- For ICC, missing handling resolves to complete cases even if `pairwise` is requested, with a warning and both requested/effective options saved. For the two-variable analyses, pairwise and complete handling select the same complete rating pairs.
- `--icc-model` selects `oneway`, `twoway-random`, or `twoway-mixed` (default: `modules.reliability.icc_model`).
- `--icc-type` selects `agreement` or `consistency` (default: `modules.reliability.icc_type`).
- Two-way random and mixed models retain the explicitly requested type. Agreement is labelled `ICC(A,1)`/`ICC(A,k)`; consistency is `ICC(C,1)`/`ICC(C,k)`, with the random/mixed model recorded separately. One-way ICC has no separate consistency estimand: a consistency request is retained for audit but resolves to one-way agreement with a warning.
- `--icc-unit` selects `single` or `average` (default: `modules.reliability.icc_unit`).
- `--kappa-weight` selects `none`, `linear`, or `quadratic` (default: `modules.reliability.kappa_weight`).
- Weighted kappa uses equally spaced category ranks, not raw numeric distances. Declared factor levels (including unused levels) determine order; otherwise numeric codes sort numerically and character values sort lexically under the recorded locale. Conflicting declared orders are errors. Check that this order has substantive meaning before using weights; value labels do not define an ordinal scale.
- `--method` selects `pearson` or `spearman` for test-retest (default: `modules.reliability.method`).
- `--conf-level` sets the confidence level for CIs (default: `modules.reliability.conf_level`).
- `--coerce` coerces non-numeric inputs for numeric analyses (default: `modules.reliability.coerce`).
- Deliberate coercion converts numeric text, not factor integer positions. Newly introduced missings cause a warning and are recorded by subject and exact original rating-row indices. Infinite numeric ratings are rejected.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs.
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--expect-invalid` is a legacy test convenience (default: `FALSE`): handled input errors exit successfully with `EXPECTED_NEGATIVE`, but the audit bundle still has `status: failed` and no normal output. It never turns an invalid statistical analysis into a completed run.
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.
- `--interactive FALSE` disables prompts. Confidence levels must lie strictly between 0 and 1; digits must be an integer from 0 to 15. Unknown option values are errors, not silent default selections.

### Long-format and grouping safeguards

- IDs and raters retain first-observed order; original rating classes and factor levels survive pivoting. ID, rater and score roles must be distinct.
- Missing ID/rater rows are excluded and recorded explicitly. Missing score cells remain missing; they are never silently aggregated or filled.
- Duplicate ID/rater combinations within a group are rejected. Aggregate deliberately before analysis if repeated ratings are scientifically intended.
- Groups are partitioned without phantom rows from missing comparisons. Missing groups are distinct from observed values such as `NA`, including display-label collisions.
- Requests record each group's raw identity, original row indices, ID/rater order, complete subjects and originating rating rows, category order, classes and coercion details.

## Outputs

Reliability uses the [shared run contract](../run-contract.md). Each completed run publishes project-local `.nlss/runs/<run-id>/request.json`, `result.json` and deterministic `output.md`, plus the frozen template. It supports verified replay. This statistical output is not a semantic final research report; authored reports use freely chosen visible Markdown paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report with reliability table and narrative.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `--log FALSE` disables only optional standalone logging, not the mandatory bundle. Saved results remain complete even when legacy output logging is disabled.
- `summary_df` retains its numeric estimate/CI/p/F/df/sample/missing fields and adds `group_missing`, `n_categories`, `estimate_status`, `ci_status`, `inference_status` and `status_reason`. CI-bound/F status fields distinguish legitimate infinities from unavailable values, which JSON otherwise represents as null.
- If any requested group has no estimable primary reliability coefficient, the entire run fails with an explanation rather than publishing a success-shaped null estimate. A valid point coefficient with unavailable inference (for example zero-error ICC) is retained with warnings and status fields. Kappa CI/p values are explicitly `not_implemented`.

### Statistical definitions and limits

- ICC uses balanced complete-subject ANOVA formulas. Agreement confidence intervals use the estimated denominator degrees of freedom, not the simpler consistency interval. One-way and consistency intervals retain their F transformations. Random/mixed model interpretation differs even when a selected formula is numerically identical. These calculations are independently checked against [psych::ICC](https://personality-project.org/r/psych/help/ICC.html); psych is a test dependency, not a new runtime requirement.
- Pearson estimates, p values and confidence intervals come from [stats::cor.test](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html). Fisher-z intervals require at least four complete pairs. Perfect correlations are not clipped to 0.999999.
- Spearman uses `cor.test(exact = FALSE)` for the estimate and approximate p value. The retained Fisher-z confidence interval is explicitly an approximation, not an exact rank interval. At three complete pairs the coefficient/p value may be available, but the interval is not.
- Kappa point estimates remain available for unweighted, linear and quadratic weighting; this module does not implement kappa confidence intervals or p values. Agreement and test-retest correlation answer different questions; do not interpret high correlation alone as agreement.

## NLSS format Templates

Use the Markdown template at `assets/reliability/default-template.md` when assembling reliability reports. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.reliability.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`analysis`, `group`, `method_label`, `icc_label`, `model`, `type`, `unit`, `weight`, `var1`, `var2`, `estimate`, `ci`, `p`, `f`, `df1`, `df2`, `n`, `n_raters`, `missing_n`, `missing_pct`, `n_categories`, `ci_status`, `inference_status`, `status_reason`.

Use `drop_if_empty: true` to remove a column if all values are blank. `ci` is computed from `ci_low`/`ci_high`.

### Note Tokens

Available note tokens include:

`ci_label`, `missing_note`, `icc_note`, `kappa_note`, `retest_note`, `ci_note`, `note_default`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`analysis`, `analysis_label`, `group`, `group_label`, `method_label`, `icc_label`, `estimate`, `ci`, `ci_text`, `p`, `n`, `n_raters`, `var1`, `var2`, `missing_text`, `full_sentence`.

## NLSS format Reporting Guidance

- Report the ICC model, type (agreement vs. consistency), and unit (single vs. average).
- For kappa, state whether weights are used (none/linear/quadratic).
- For test-retest, report the correlation method and confidence interval.
- Always note missing-data handling and the number of subjects/raters.

## Parquet Support

Parquet inputs (`--parquet`) require the R package `arrow`.
