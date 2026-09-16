---
name: nonparametric
description: Nonparametric tests (Wilcoxon one-sample/paired, Mann-Whitney, Kruskal-Wallis, Friedman) with auto selection, effect sizes, post-hoc pairwise tests, and NLSS format outputs.
license: Apache-2.0
---

# Nonparametric Tests (Base R, NLSS format)

## Overview

Run nonparametric tests in base R and generate NLSS format-ready tables and narratives. Supported tests include Wilcoxon signed-rank (one-sample and paired), Mann-Whitney *U* (two independent groups), Kruskal-Wallis (*k* independent groups), and Friedman (*k* repeated measures). Optional post-hoc pairwise Wilcoxon comparisons are available for Kruskal-Wallis and Friedman.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Choose a test (`--test auto` or an explicit type) and specify variables/grouping.
3. Run `scripts/R/nonparametric.R` with the correct flags.
4. Review the saved request, actual methods, case selection, interval coverage and warnings before interpreting the results. Use deterministic per-run output as evidence for a semantically written final report, not as a report-writing limit.

## Script: `scripts/R/nonparametric.R`

Run with `Rscript`. Statistical methods use base R; input, configuration and audit dependencies follow the shared [import](../import-contract.md) and [run](../run-contract.md) contracts.

### One-Sample Wilcoxon (CSV)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --mu 0
```

### Mann-Whitney U (CSV, Two Groups)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --group condition --test mann_whitney
```

### Kruskal-Wallis (CSV, 3+ Groups) With Post-Hoc

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --group condition --test kruskal --posthoc pairwise --p-adjust holm
```

### Wilcoxon Paired (CSV, Repeated Measures)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --x pre_score --y post_score --test wilcoxon
```

### Friedman (Wide Format)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --within pre,mid,post --subject-id id --test friedman
```

### Parquet Input

```bash
Rscript <path to scripts/R/nonparametric.R> --parquet data.parquet --vars score --group condition
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/nonparametric.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--test` uses `modules.nonparametric.test` (`auto`, `wilcoxon`, `mann_whitney`, `kruskal`, `friedman`).
- `--vars` defaults to `modules.nonparametric.vars_default` (typically numeric columns).
- `--group` is required for Mann-Whitney and Kruskal-Wallis.
- `--x` and `--y` are required for paired Wilcoxon and must be the same length.
- `--within` and `--subject-id` are required for Friedman (wide format).
- `--mu` uses `modules.nonparametric.mu` for one-sample Wilcoxon.
- `--alternative` uses `modules.nonparametric.alternative` (`two.sided`, `greater`, `less`).
- `--exact` uses `modules.nonparametric.exact` (`auto`, `TRUE`, `FALSE`).
- `--continuity` uses `modules.nonparametric.continuity` (`TRUE`, `FALSE`).
- `--conf-level` uses `modules.nonparametric.conf_level`.
- `--posthoc` uses `modules.nonparametric.posthoc` (`none`, `pairwise`).
- `--p-adjust` uses `modules.nonparametric.p_adjust` (e.g., `holm`, `bonferroni`, `BH`).
- `--effect-size` uses `modules.nonparametric.effect_size` (`r`, `rb`, `eta_H_sq`, `kendall_w`). The existing `epsilon_sq` aliases remain accepted with an explicit naming warning; their numerical estimand has not changed.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Outcomes must be numeric scores, including numeric labelled SPSS values. Factor/character values are not silently converted to internal codes; clarify their scoring first. Shared import normalizes user-missing values and retains labels. Non-missing infinite values and overflowing signed differences are errors rather than silent exclusions.
- Auto mode chooses Friedman when `--within` is supplied, paired Wilcoxon for `--x/--y`, Mann-Whitney for 2-group `--group`, Kruskal-Wallis for >2 groups, and one-sample Wilcoxon otherwise.
- Mann-Whitney requires exactly two non-missing group levels.
- Kruskal-Wallis requires at least two non-missing group levels (auto mode uses Kruskal when >2).
- Friedman requires distinct `--within` measures (>=2) and a separate `--subject-id`. Each non-missing subject ID must occur once; duplicated subjects are not independent blocks. Incomplete blocks, including missing IDs, are excluded jointly. Every Friedman post-hoc comparison uses the same retained blocks.
- Groups follow declared observed factor levels, otherwise sorted non-missing raw values. Run-local IDs and source-row lists identify groups; labels and rounded displays do not. Numeric/time identifiers include exact hexadecimal values in the request. Identical display labels are disambiguated with group IDs. A literal `NA` group label remains distinct from excluded missing group values.
- Missing values are removed separately for each outcome or paired-variable pair, including its required group. Kruskal-Wallis uses the groups actually represented after outcome-wise exclusions; it requires at least two. Mann-Whitney requires observations in both original groups.
- `--exact`, `--continuity`, `--alternative` and `--conf-level` apply to Wilcoxon/Mann-Whitney and requested post-hoc comparisons. Omnibus tests do not have one-sided alternatives. A nonzero `--mu` is one-sample-only. Contradictory designs, unknown option choices and invalid numeric domains fail explicitly; `--interactive FALSE` never prompts.

## Inference, effects and intervals

Primary inference comes from `stats::wilcox.test`, `stats::kruskal.test` or `stats::friedman.test`; it is not reconstructed from effect sizes. For independent comparisons, R's returned `W` is already the offset rank sum, here labelled `U`. It must not have the rank offset subtracted again. The first group/measure defines the positive direction. [R Wilcoxon documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html)

- `rb`: signed-rank balance `(positive ranks - negative ranks) / total ranks`, excluding zero differences; for independent samples `2U/(n1*n2)-1`, equivalent to pairwise superiority minus inferiority with ties contributing zero.
- `r`: tie-adjusted standardized rank score `z/sqrt(N)`, **without continuity correction**, even when the inferential p-value uses correction or an exact distribution. For signed ranks, `N` counts nonzero differences and the rank variance is `sum(ranks^2)/4`; for independent samples, `N=n1+n2` and pooled-rank tie correction is used. It is not obtained by transforming an exact p-value.
- `eta_H_sq`: the existing untruncated rank effect `(H-k+1)/(n-k)`, with effective sample size and group count. It can be negative and is unavailable when `n<=k`. The previous `epsilon_sq` name was misleading: rank epsilon squared is `H/(n-1)`, a different estimand. NLSS preserves the old values while recording the corrected name and the requested compatibility alias. [Reference implementation distinguishing rank eta and epsilon](https://github.com/easystats/effectsize/blob/main/R/rank_ANOVA.R)
- `kendall_w`: tie-corrected Friedman `Q/[n(k-1)]`. Kruskal-Wallis and Friedman select their respective omnibus effect; post-hoc `r` or `rb` follows an explicitly requested rank effect, otherwise `r`.

One-sample CIs describe the population pseudomedian, paired CIs the pseudomedian of paired differences, and independent CIs a population location shift. These are neither CIs for `r`/`rb` nor generally for the difference of medians. The returned location estimate and R estimate label are saved separately from descriptive medians. [R Wilcoxon interval definitions](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html)

The primary test and interval are evaluated separately. A valid test can have an unavailable interval or reduced attainable coverage; the actual level and reason/warning are retained, and the narrative does not label reduced coverage with the requested percentage. Infinite one-sided endpoints are displayed as unbounded; JSON represents them as `null` plus `positive_infinity` or `negative_infinity` status, distinct from unavailable values.

Exact/approximate inference follows the **installed** R version and is recorded per test, not inferred from the requested flag. In R 4.5.2, automatic Wilcoxon exact inference requires fewer than 50 nonzero signed differences (or fewer than 50 in each independent sample), no ties and, for signed ranks, no zero differences. Ties/zeros trigger the R approximation and warnings when exact inference was requested. Newer R versions can differ; do not apply R-devel policies retroactively to a saved run. No Monte Carlo simulation or seed is introduced.

Post-hoc comparisons are all pairwise Wilcoxon tests within each outcome's omnibus family; no significance-based selection is performed. Adjustments use `p.adjust(..., n = number_of_planned_pairs)`, including unavailable pairs in the family size. Unavailable secondary comparisons retain explicit status/reason and do not invalidate an estimable omnibus test. Their intervals are unadjusted; a Holm/BH p-value does not make an interval simultaneous. Requested primary tests without a finite statistic and p-value fail the run instead of publishing success-shaped missing values.

## Outputs

This migrated entrypoint also publishes `.nlss/runs/<run-id>/request.json`, `result.json` and deterministic `output.md`, with frozen templates and verified replay. The request contains source rows, raw group ordering, signed zero-difference rows, paired rows, Friedman subject mapping, planned comparison families and actual inference diagnostics. The working Parquet is read-only. Failed runs have no normal output and leave prior report/log bytes unchanged.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing the nonparametric table and narrative.
- `result.json`: Machine-readable results and options, always retained in the saved run. Logged results include `summary_df`, `posthoc_df`, and `diagnostics_df`.
- `result.json` always retains unrounded results, independently of `--log FALSE` or legacy `logging.include_outputs`. Only completed runs can be replayed as completed analyses.

## NLSS format Templates (YAML)

Templates are stored under `assets/nonparametric/` and mapped in `scripts/config.yml`:

- `templates.nonparametric.default`: `nonparametric/default-template.md`
- `templates.nonparametric.posthoc`: `nonparametric/posthoc-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table Column Keys (Summary)

Available column keys for `table.columns` include:

`test_type`, `variable`, `measure_1`, `measure_2`, `group`, `group_1`, `group_2`, `n_1`, `n_2`, `n_total`,
`median_1`, `median_2`, `iqr_1`, `iqr_2`, `median`, `iqr`, `median_diff`, `iqr_diff`, `group_summary`, `within_summary`,
`stat_label`, `statistic`, `df`, `p`, `effect_size_label`, `effect_size_value`, `ci_low`, `ci_high`.

Machine-readable rows additionally include `test_id`, group IDs, `status`, `method`, `exact_used`, `continuity_used`, `ties`, `zero_diff_n`, `n_nonzero`, `z`, `rank_variance`, `effect_size_status`, `ci_status`, `ci_low_status`, `ci_high_status`, `actual_ci_level`, `ci_estimand`, `location_estimate` and `estimate_label`. Post-hoc rows include `comparison_id`, `family_size` and unavailable `reason`. Default narratives expose the effective method and interval coverage/unavailability without requiring a custom template.

### Table Column Keys (Post-Hoc)

`variable`, `group`, `group_1`, `group_2`, `n_1`, `n_2`, `median_1`, `median_2`, `iqr_1`, `iqr_2`,
`stat_label`, `statistic`, `p`, `p_adj`, `effect_size_label`, `effect_size_value`, `ci_low`, `ci_high`.

### Note Tokens

Available note tokens include:

`note_default`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `test_type`, `variable`, `measure_1`, `measure_2`, `group`, `group_1`, `group_2`,
`n_1`, `n_2`, `n_total`, `median_1`, `median_2`, `iqr_1`, `iqr_2`, `median`, `iqr`, `median_diff`, `iqr_diff`,
`stat_label`, `statistic`, `df`, `p`, `effect_size_label`, `effect_size_value`, `ci`, `conf_level`.

## NLSS format Reporting Guidance

- Report the test type, test statistic, df (where applicable), p-value, and effect size.
- For two-group tests, report group medians and IQRs.
- For Kruskal-Wallis or Friedman, include post-hoc results when requested and note the p-value adjustment. Distinguish location-shift assumptions from a general claim about medians; choosing ranks does not remove design, independence or symmetry assumptions.
- Final research reports remain contextual semantic syntheses: integrate hypotheses, design, magnitude, uncertainty and limitations beyond the deterministic output templates.

## Dependencies

- Parquet input requires the R package `arrow`.
