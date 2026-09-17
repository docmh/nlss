---
name: anova
description: Between/within/mixed ANOVA and ANCOVA with effect sizes, post-hoc tests, planned contrasts (emmeans/custom), sphericity checks, optional bootstrap CIs, and NLSS format outputs.
license: Apache-2.0
---

# ANOVA (Base R, NLSS format)

## Overview

Run between-subjects, within-subjects (repeated measures), or mixed ANOVA using R's established model implementations. Optional covariates (ANCOVA), post-hoc comparisons, estimated marginal means and planned contrasts (custom JSON or built-in `emmeans` methods) remain available. Outputs include sums of squares, df, F, p, effect sizes and the shared [audit/replay contract](../run-contract.md).

Post-hoc behavior: Tukey HSD is used for between-subjects factors; paired t-tests are used for within-subjects comparisons (and for mixed designs, within comparisons are computed separately per between-group combination).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose a design (between/within/mixed) and specify variables.
3. Optionally request planned contrasts (`--emmeans`, `--contrasts`, `--contrast-file`).
4. Run the `anova` operation through `run_nlss.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `result.json`) for NLSS format reporting and diagnostics.

## Execution: `anova`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

Run with `Rscript`. Between-subjects Type I uses `stats::anova`; Type II/III requires `car`. A missing package or non-estimable model is an explicit failure, never a fallback to a different requested hypothesis.

### Between-Subjects ANOVA (CSV)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --dv outcome --between group
```

### Factorial Between-Subjects ANOVA + Covariates

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --dv outcome --between group,gender --covariates age
```

### Within-Subjects ANOVA (Wide Format)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --within pre,mid,post --subject-id id
```

### Mixed ANOVA (Within + Between)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --within pre,mid,post --between group --subject-id id
```

### Planned Contrasts (Custom JSON)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --dv outcome --between group3 --emmeans group3 --contrasts custom --contrast-file contrasts.json
```

### Planned Contrasts (Built-in Method)

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --csv <path to CSV file> --dv outcome --between group3 --emmeans group3 --contrasts trt.vs.ctrl
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --parquet <path to parquet file> --dv outcome --between group
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" anova --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--dv` required for between-subjects designs; do not combine it with `--within`.
- `--between` is optional, but at least one of `--between` or `--within` is required.
- `--within` uses wide-format repeated measures (one column per measurement).
- `--subject-id` is required when `--within` is present and rejected for between-only models. Interactive repeated designs offer Type I as the design-specific default.
- `--covariates` is optional (numeric).
- `--type` uses `modules.anova.type` (`I`, `II`, `III`). Type III uses sum-to-zero factor contrasts. Repeated/mixed ANOVA retains `stats::aov`'s sequential Type I error-stratum analysis: an implicit Type II/III default is disclosed as between-only; an explicit `--type II/III` for a repeated design is rejected.
- `--effect-size` uses `modules.anova.effect_size` (`eta_sq`, `partial_eta`, `omega_sq`, `partial_omega`).
- `--posthoc` uses `modules.anova.posthoc` (`none`, `tukey`, `pairwise`).
- `--emmeans` uses `modules.anova.emmeans` (`none` or a factor term such as `group3` or `within*group3`). It publishes the actual reference-grid means and pointwise CIs, also when no contrasts are requested. Covariates use reference-grid values and factor levels are equally weighted.
- `--contrasts` uses `modules.anova.contrasts` (`none`, `pairwise`, `custom`, or any `emmeans` method string) and requires `--emmeans` unless the contrast JSON specifies `term`.
- `--contrast-file` provides a JSON contrast spec (custom weights or a method plus optional arguments).
- `--p-adjust` uses `modules.anova.p_adjust` (e.g., `holm`, `bonferroni`). Ordinary pairwise post-hoc tests accept `stats::p.adjust` methods; emmeans-only contrasts additionally accept `tukey`, `scheffe`, `sidak`, `mvt` and `dunnettx`. Tukey HSD itself always uses its simultaneous Tukey correction.
- `--conf-level` uses `modules.anova.conf_level`.
- `--sphericity` uses `modules.anova.sphericity` (`auto`, `none`).
- `--bootstrap` uses `modules.anova.bootstrap` (TRUE/FALSE).
- `--bootstrap-samples` uses `modules.anova.bootstrap_samples` (default: 1000; integer at least 2).
- `--seed` fixes resampling and potentially stochastic `emmeans` calculations; absent a supplied seed, these use `modules.anova.seed` (default: 1). Seed and RNG state are preserved for replay.
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Between-subjects factors retain existing factor order or sorted observed raw-value order. Numeric/timestamp values with identical printed labels remain distinct; mapping entries retain source rows, local level IDs and hexadecimal numeric identity.
- Responses and covariates are explicitly converted to numeric values (factor labels, not internal codes). Conversion losses warn with source rows; non-finite values are rejected. [SPSS labels and user-missings](../import-contract.md) retain the shared import semantics.
- Within-subjects designs use wide format with one column per repeated measure and one unique row per retained subject. Subject IDs are categorical identities regardless of their source storage type; duplicate IDs are rejected. Internal response/occasion names avoid collisions with source columns.
- Missing values are removed jointly across required variables **after** numeric conversion. The request records included/excluded source rows, classes, factor levels/contrasts, formula, model rank, and the subject/long-data mapping. Rank-deficient or unestimable primary effects fail explicitly.
- Planned contrasts require `emmeans`. Custom weights follow the `emmeans` row order or use named weights keyed to level labels (for interactions, labels use `factor=level, factor2=level2`).

### Contrast JSON Format

Custom contrasts can be specified as named weight vectors. Weights may be ordered numeric arrays (matching the `emmeans` row order) or named weights keyed by level labels.

Example (custom weights):

```json
{
  "term": "group3",
  "contrasts": {
    "A_vs_B": {"A": 1, "B": -1, "C": 0},
    "A_vs_C": [1, 0, -1]
  }
}
```

Example (built-in method with args):

```json
{
  "term": "group3",
  "method": "trt.vs.ctrl",
  "args": {"ref": "A"}
}
```

## Outputs

Every resolved dataset-backed attempt has a run bundle. Successful runs contain `request.json`, `result.json`, frozen templates and deterministic `output.md`; failed analyses have no normal output and leave existing canonical report/log projections unchanged. `--log FALSE` disables only the optional standalone logging projection. Replay uses the immutable data and resolved contrast specification, even after the original contrast JSON is changed or deleted; no external contrast file is required.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing the ANOVA table and narrative.
- `result.json`: unrounded `summary_df`, `posthoc_df`, `contrasts_df`, `emmeans_df` and `assumptions_df`.
- When `--bootstrap TRUE`, `summary_df` includes `boot_ci_low/high`, `boot_valid`, `boot_discarded` and `boot_ci_status` for the selected effect size; the same counts/status are visible in its Markdown narrative. Complete rows are jointly resampled between subjects; repeated designs resample whole complete subjects and assign new categorical IDs to duplicate draws. Changed effect df or unavailable fits/estimates are excluded and counted. Percentile intervals require at least two finite draws; intervals with omitted draws are explicitly conditional on estimable resamples, not an assurance of adequate bootstrap coverage.
- Assumption diagnostics (Shapiro-Wilk residual normality, homogeneity tests, and Mauchly for sphericity when applicable) are recorded in `result.json`.

### Estimands and interpretation limits

- `eta_sq = SS_effect / SS_total` uses the centered total response SS on the actual analysis cases; Type II/III effect SS are not added to construct a fictitious total. `omega_sq = (SS_effect - df_effect*MSE)/(SS_total + MSE)`. Negative omega estimates are retained, not truncated.
- Partial eta uses `SS_effect/(SS_effect + SS_error)`; partial omega uses `(SS_effect - df_effect*MSE)/(SS_effect + SS_error + MSE)`. Repeated designs use each effect's error stratum; ordinary eta/omega are not generalized eta/omega.
- Repeated residual normality includes subject identity and the requested fixed design. These dependent residuals are a screening diagnostic, not proof of independent normal errors; Shapiro-Wilk is unavailable outside 3–5000 residuals (or the configured lower limit).
- Exactly constant observed responses (including nonzero constants), responses constant within every between-factor cell, and repeated within-difference profiles constant across subjects within every cell are rejected before fitting. These are exact data-pattern guards, also applied to bootstrap draws. Positive but very small residual variation is not automatically rounded to zero or discarded by a variance threshold; R's numerical warnings are preserved in JSON and the Markdown notes.
- Mauchly tests the within-occasion contrast space (`X=~1`) after the full between/covariate model. Greenhouse–Geisser and Huynh–Feldt corrections use that residual contrast covariance, with effective error df and HF capped at 1. Singular covariance or insufficient subjects is explicitly unavailable. Corrected dfs and p values are saved as `df1_gg`, `df2_gg`, `p_gg`, `df1_hf`, `df2_hf`, `p_hf`; original F and uncorrected inference remain available.
- Between pairwise post-hoc tests are marginal Welch comparisons, not covariate-adjusted contrasts; repeated post-hoc tests are paired within each observed between-cell. P adjustment is per factor/cell over the **full planned family**, including unavailable tests; pairwise t intervals remain unadjusted. Unavailable comparisons retain result rows with `status`, `reason`, `family_size` and an explicit Markdown explanation. Tukey intervals honor `--conf-level` and keep group identities independent of punctuation in labels. Use `emmeans` for model-based adjusted contrasts.
- `emmeans` retains its aovlist limitations for unbalanced repeated designs and covariates; review package warnings and the saved reference grid. Requested versus effective contrast adjustments and package messages are saved as `contrast_adjustment` and shown in Markdown (for example, emmeans replaces inappropriate Tukey adjustment for polynomial contrasts with Sidak). Its confidence-interval adjustment can differ from the p-value method; the package messages distinguish these. A recovered grid or omnibus significance does not justify interpreting an inappropriate marginal contrast.

## NLSS format Templates (YAML)

Templates are stored under `assets/anova/` and mapped in `scripts/config.yml`:

- `templates.anova.default`: `anova/default-template.md`
- `templates.anova.posthoc`: `anova/posthoc-template.md`
- `templates.anova.contrasts`: `anova/contrasts-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table Column Keys (Omnibus ANOVA)

Available column keys include:

`model`, `term`, `df1`, `df2`, `ss`, `ms`, `f`, `p`, `eta_sq`, `partial_eta_sq`, `omega_sq`, `partial_omega_sq`, `boot_ci_low`, `boot_ci_high`, `df1_gg`, `df2_gg`, `p_gg`, `df1_hf`, `df2_hf`, `p_hf`.

### Table Column Keys (Post-Hoc)

`term`, `group`, `group_1`, `group_2`, `contrast`, `mean_diff`, `se`, `t`, `df`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`.

### Table Column Keys (Planned Contrasts)

`term`, `contrast`, `estimate`, `se`, `df`, `t`, `p`, `p_adj`, `ci_low`, `ci_high`, `method`.

### Note Tokens

Available note tokens include:

`note_default`, `assumption_note`.

### Narrative Tokens

Omnibus narrative row tokens include:

`full_sentence`, `model`, `term`, `df1`, `df2`, `f`, `p`, `effect_size_label`, `effect_size_value`, `boot_ci`, `boot_ci_low`, `boot_ci_high`.

Post-hoc narrative row tokens include:

`full_sentence`, `term`, `group`, `group_1`, `group_2`, `mean_diff`, `t`, `df`, `p`, `p_adj`, `ci`.

Contrast narrative row tokens include:

`full_sentence`, `term`, `contrast`, `estimate`, `se`, `df`, `t`, `p`, `p_adj`, `ci`.

## NLSS format Reporting Guidance

- Report F, df, p, and effect sizes for each omnibus effect (include bootstrap CIs when enabled).
- Indicate post-hoc method and p-value adjustment when reported.
- For repeated measures, report sphericity results (Mauchly) and note any violations.
- Run output is SPSS-like statistical evidence, not a final research-report template. Interpret the design, contrasts, uncertainty and substantive context semantically; synthesize findings freely across procedures rather than reusing canned significance sentences.

## Dependencies

- Parquet input requires the R package `arrow`.
- Type II/III sums of squares require the R package `car` for that requested analysis.
- Estimated marginal means and planned contrasts require the R package `emmeans`.
