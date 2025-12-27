---
name: nonparametric
description: Nonparametric group and repeated-measures tests (Wilcoxon signed-rank, Mann-Whitney U, Kruskal-Wallis, Friedman) with APA-ready tables, narratives, and post-hoc comparisons.
---

# Nonparametric tests (Base R, APA 7)

## Overview

Run nonparametric tests in base R and generate APA 7-ready tables and narratives. Supported tests include Wilcoxon signed-rank (one-sample and paired), Mann-Whitney U (two independent groups), Kruskal-Wallis (k independent groups), and Friedman (k repeated measures). Optional post-hoc pairwise Wilcoxon comparisons are available for Kruskal-Wallis and Friedman.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions, select the appropriate analysis, document decisions, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose a test (`--test auto` or an explicit type) and specify variables/grouping.
3. Run `scripts/R/nonparametric.R` with the correct flags, or use the PowerShell wrapper on Windows.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for APA reporting.

## Script: `scripts/R/nonparametric.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\nonparametric.R> --csv <path to CSV file> --vars score --group condition
```

### One-sample Wilcoxon (CSV)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --mu 0
```

### Mann-Whitney U (CSV, two groups)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --group condition --test mann_whitney
```

### Kruskal-Wallis (CSV, 3+ groups) with post-hoc

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --vars score --group condition --test kruskal --posthoc pairwise --p-adjust holm
```

### Wilcoxon paired (CSV, repeated measures)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --x pre_score --y post_score --test wilcoxon
```

### Friedman (wide format)

```bash
Rscript <path to scripts/R/nonparametric.R> --csv data.csv --within pre,mid,post --subject-id id --test friedman
```

### Parquet input

```bash
Rscript <path to scripts/R/nonparametric.R> --parquet data.parquet --vars score --group condition
```

### Interactive prompts

```bash
Rscript <path to scripts/R/nonparametric.R> --interactive
```

## Options

Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

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
- `--effect-size` uses `modules.nonparametric.effect_size` (`r`, `rb`, `epsilon_sq`, `kendall_w`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Variables are treated as numeric/ordinal; non-numeric variables raise an error.
- Auto mode chooses Friedman when `--within` is supplied, paired Wilcoxon for `--x/--y`, Mann-Whitney for 2-group `--group`, Kruskal-Wallis for >2 groups, and one-sample Wilcoxon otherwise.
- Mann-Whitney requires exactly two non-missing group levels.
- Kruskal-Wallis requires at least two non-missing group levels (auto mode uses Kruskal when >2).
- Friedman requires `--within` (>=2 measures) and `--subject-id`; listwise complete cases are used.
- `--exact` and `--continuity` apply to Wilcoxon and Mann-Whitney only.
- Missing values are removed listwise within each test.

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- `report_canonical.md`: APA 7 report containing the nonparametric table and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled). Logged results include `summary_df`, `posthoc_df`, and `diagnostics_df`.

## APA 7 Templates (YAML)

Templates are stored under `nlss/assets/nonparametric/` and mapped in `nlss/scripts/config.yml`:

- `templates.nonparametric.default`: `nonparametric/default-template.md`
- `templates.nonparametric.posthoc`: `nonparametric/posthoc-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table column keys (summary)

Available column keys for `table.columns` include:

`test_type`, `variable`, `measure_1`, `measure_2`, `group`, `group_1`, `group_2`, `n_1`, `n_2`, `n_total`,
`median_1`, `median_2`, `iqr_1`, `iqr_2`, `median`, `iqr`, `median_diff`, `iqr_diff`, `group_summary`, `within_summary`,
`stat_label`, `statistic`, `df`, `p`, `effect_size_label`, `effect_size_value`, `ci_low`, `ci_high`.

### Table column keys (post-hoc)

`variable`, `group`, `group_1`, `group_2`, `n_1`, `n_2`, `median_1`, `median_2`, `iqr_1`, `iqr_2`,
`stat_label`, `statistic`, `p`, `p_adj`, `effect_size_label`, `effect_size_value`, `ci_low`, `ci_high`.

### Note tokens

Available note tokens include:

`note_default`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `test_type`, `variable`, `measure_1`, `measure_2`, `group`, `group_1`, `group_2`,
`n_1`, `n_2`, `n_total`, `median_1`, `median_2`, `iqr_1`, `iqr_2`, `median`, `iqr`, `median_diff`, `iqr_diff`,
`stat_label`, `statistic`, `df`, `p`, `effect_size_label`, `effect_size_value`, `ci`, `conf_level`.

## APA 7 Reporting Guidance

- Report the test type, test statistic, df (where applicable), p-value, and effect size.
- For two-group tests, report group medians and IQRs.
- For Kruskal-Wallis or Friedman, include post-hoc results when requested and note the p-value adjustment.

## Dependencies

- Parquet input requires the R package `arrow`.
