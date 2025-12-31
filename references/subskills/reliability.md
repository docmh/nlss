---
name: reliability
description: Inter-rater/test-retest reliability with ICC (oneway/twoway, agreement/consistency), kappa (weighted), or test-retest correlations; wide/long formats, CIs, grouping, and NLSS format outputs.
license: Apache-2.0
metadata:
  nlss.author: "Mike Hammes"
  nlss.copyright: "Copyright (c) 2025 Mike Hammes"
  nlss.version: "1.0.0"
---

# Reliability Analysis (Base R, NLSS format)

## Overview

Compute reliability for ratings or repeated measurements:

- **ICC** for continuous ratings (inter-rater or repeated measures).
- **Kappa** for categorical ratings (two raters).
- **Test-retest** correlations for stability across two time points.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose `--analysis` (icc/kappa/test_retest).
3. Choose `--format` (wide/long) and supply the required variables.
4. Run `scripts/R/reliability.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

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
- For ICC, missing handling is forced to complete cases even if `pairwise` is requested.
- `--icc-model` selects `oneway`, `twoway-random`, or `twoway-mixed` (default: `modules.reliability.icc_model`).
- `--icc-type` selects `agreement` or `consistency` (default: `modules.reliability.icc_type`).
- `--icc-unit` selects `single` or `average` (default: `modules.reliability.icc_unit`).
- `--kappa-weight` selects `none`, `linear`, or `quadratic` (default: `modules.reliability.kappa_weight`).
- `--method` selects `pearson` or `spearman` for test-retest (default: `modules.reliability.method`).
- `--conf-level` sets the confidence level for CIs (default: `modules.reliability.conf_level`).
- `--coerce` coerces non-numeric inputs for numeric analyses (default: `modules.reliability.coerce`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs.
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--expect-invalid` treats invalid input as expected (default: `FALSE`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).
- `report_canonical.md`: NLSS format report with reliability table and narrative.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## NLSS format Templates

Use the Markdown template at `assets/reliability/default-template.md` when assembling reliability reports. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.reliability.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`analysis`, `group`, `method_label`, `icc_label`, `model`, `type`, `unit`, `weight`, `var1`, `var2`, `estimate`, `ci`, `p`, `f`, `df1`, `df2`, `n`, `n_raters`, `missing_n`, `missing_pct`.

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