---
name: t-test
description: One-sample, independent-samples, and paired-samples t-tests with APA-ready tables, narratives, and bootstrap options.
---

# t-tests (Base R, APA 7)

## Overview

Run one-sample, independent-samples, or paired-samples t-tests in base R and generate APA 7-ready tables and narratives. Outputs include means, standard deviations, mean differences, t, df, p, and Cohen's d. Optional bootstrap confidence intervals are available.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose the t-test type and specify variables (one-sample/independent) or paired measures (paired).
3. Run `scripts/R/t_test.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) for APA reporting.

## Script: `scripts/R/t_test.R`

Run with `Rscript` and base R only.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\t_test.R> --csv <path to CSV file> --vars age,score --mu 0
```

### One-sample t-test (CSV)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --vars age,score --mu 0
```

### Independent-samples t-test (CSV, two groups)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --vars stress --group condition
```

### Paired-samples t-test (CSV, repeated measures)

```bash
Rscript <path to scripts/R/t_test.R> --csv <path to CSV file> --x pre_score --y post_score
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/t_test.R> --rds <path to RDS file> --vars age,score
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/t_test.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet input

```bash
Rscript <path to scripts/R/t_test.R> --parquet <path to parquet file> --vars age,score
```

### Interactive prompts

```bash
Rscript <path to scripts/R/t_test.R> --interactive
```

### Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--vars` defaults to `modules.t_test.vars_default` (typically numeric columns).
- `--mu` uses `modules.t_test.mu` for one-sample tests.
- `--alternative` uses `modules.t_test.alternative` (`two.sided`, `greater`, `less`).
- `--var-equal` uses `modules.t_test.var_equal` for independent tests.
- `--conf-level` uses `modules.t_test.conf_level`.
- `--bootstrap` uses `modules.t_test.bootstrap` and `--bootstrap-samples` uses `modules.t_test.bootstrap_samples`.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).
- `--expect-two-groups` prints an informational message and exits successfully when `--group` has != 2 levels (useful for negative checks).

## Inputs and handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- Independent tests require a `--group` variable with exactly two non-missing levels.
- Paired tests require `--x` and `--y` lists of equal length (paired by position).
- Missing values are removed listwise per test (within each variable or pair).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- `report_canonical.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).
- Diagnostics (Shapiro-Wilk per sample and variance tests for independent samples) are included in the JSONL output.
- Invalid input conditions (e.g., incompatible flags) are logged with `results.status = invalid_input`.

## APA 7 Templates

Use the Markdown template in `nlss/assets/t-test/default-template.md` when assembling t-test reports.

### YAML template controls

- Template path can be overridden via `templates.t_test.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`test_type`, `variable`, `measure_1`, `measure_2`, `group_1`, `group_2`, `n_1`, `n_2`, `mean_1`, `mean_2`,
`sd_1`, `sd_2`, `mean_diff`, `t`, `df`, `p`, `d`, `ci_low`, `ci_high`, `boot_ci_low`, `boot_ci_high`,
`boot_d_ci_low`, `boot_d_ci_high`.

Use `drop_if_empty: true` to remove columns that are unused (for example, `group_1` in one-sample tests).

### Note tokens

Available note tokens include:

`note_default`.

### Narrative tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `test_type`, `variable`, `measure_1`, `measure_2`, `group_1`, `group_2`, `n_1`, `n_2`,
`mean_1`, `mean_2`, `sd_1`, `sd_2`, `mean_diff`, `t`, `df`, `p`, `d`, `ci`, `conf_level`, `mu`.

## APA 7 Reporting Guidance

- Report test type, sample sizes, means/SDs, t, df, p, and Cohen's d.
- Include confidence intervals for the mean difference or mean when available.
- For independent samples, report group labels and whether equal variances were assumed.
- For paired tests, report both measures and the mean difference.
