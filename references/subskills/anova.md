---
name: anova
description: Between/within/mixed ANOVA and ANCOVA with effect sizes, post-hoc tests, planned contrasts (emmeans/custom), sphericity checks, optional bootstrap CIs, and NLSS format outputs.
---

# ANOVA (Base R, NLSS format)

## Overview

Run between-subjects, within-subjects (repeated measures), or mixed ANOVA in base R and produce NLSS format-ready tables and narratives. Optional covariates (ANCOVA), post-hoc comparisons, and planned contrasts (custom JSON or built-in `emmeans` methods) are supported. Outputs include sums of squares, *df*, *F*, *p*, and effect sizes.

Post-hoc behavior: Tukey HSD is used for between-subjects factors; paired t-tests are used for within-subjects comparisons (and for mixed designs, within comparisons are computed separately per between-group combination).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose a design (between/within/mixed) and specify variables.
3. Optionally request planned contrasts (`--emmeans`, `--contrasts`, `--contrast-file`).
4. Run `scripts/R/anova.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for NLSS format reporting and diagnostics.

## Script: `scripts/R/anova.R`

Run with `Rscript` and base R only. Type II/III sums of squares require `car` (optional); if not installed, the script falls back to Type I.

### Between-Subjects ANOVA (CSV)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group
```

### Factorial Between-Subjects ANOVA + Covariates

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group,gender --covariates age
```

### Within-Subjects ANOVA (Wide Format)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --within pre,mid,post --subject-id id
```

### Mixed ANOVA (Within + Between)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --within pre,mid,post --between group --subject-id id
```

### Planned Contrasts (Custom JSON)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group3 --emmeans group3 --contrasts custom --contrast-file contrasts.json
```

### Planned Contrasts (Built-in Method)

```bash
Rscript <path to scripts/R/anova.R> --csv <path to CSV file> --dv outcome --between group3 --emmeans group3 --contrasts trt.vs.ctrl
```

### Parquet Input

```bash
Rscript <path to scripts/R/anova.R> --parquet <path to parquet file> --dv outcome --between group
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/anova.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--dv` required for between-subjects designs.
- `--between` is optional, but at least one of `--between` or `--within` is required.
- `--within` uses wide-format repeated measures (one column per measurement).
- `--subject-id` is required when `--within` is present.
- `--covariates` is optional (numeric).
- `--type` uses `modules.anova.type` (`I`, `II`, `III`); Type II/III requires `car` and apply to between-subjects models only.
- `--effect-size` uses `modules.anova.effect_size` (`eta_sq`, `partial_eta`, `omega_sq`, `partial_omega`).
- `--posthoc` uses `modules.anova.posthoc` (`none`, `tukey`, `pairwise`).
- `--emmeans` uses `modules.anova.emmeans` (`none` or a factor term such as `group3` or `time*group3`).
- `--contrasts` uses `modules.anova.contrasts` (`none`, `pairwise`, `custom`, or any `emmeans` method string) and requires `--emmeans` unless the contrast JSON specifies `term`.
- `--contrast-file` provides a JSON contrast spec (custom weights or a method plus optional arguments).
- `--p-adjust` uses `modules.anova.p_adjust` (e.g., `holm`, `bonferroni`).
- `--conf-level` uses `modules.anova.conf_level`.
- `--sphericity` uses `modules.anova.sphericity` (`auto`, `none`).
- `--bootstrap` uses `modules.anova.bootstrap` (TRUE/FALSE).
- `--bootstrap-samples` uses `modules.anova.bootstrap_samples` (default: 1000).
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Between-subjects factors are coerced to factors.
- Covariates are coerced to numeric where possible.
- Within-subjects designs must use wide format with one column per repeated measure.
- Missing values are removed listwise across all required columns per analysis.
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

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).
- `report_canonical.md`: NLSS format report containing the ANOVA table and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `summary_df`, `posthoc_df`, `contrasts_df`, and `assumptions_df`.
- When `--bootstrap TRUE`, `summary_df` includes `boot_ci_low` and `boot_ci_high` for the selected effect size.
- Assumption diagnostics (Shapiro-Wilk residual normality, homogeneity tests, and Mauchly for sphericity when applicable) are recorded in `analysis_log.jsonl`.

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

- Report *F*, *df*, *p*, and effect sizes for each omnibus effect (include bootstrap CIs when enabled).
- Indicate post-hoc method and *p*-value adjustment when reported.
- For repeated measures, report sphericity results (Mauchly) and note any violations.

## Dependencies

- Parquet input requires the R package `arrow`.
- Type II/III sums of squares require the R package `car` (optional).
- Planned contrasts require the R package `emmeans`.