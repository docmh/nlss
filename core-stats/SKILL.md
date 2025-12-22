---
name: core-stats
description: Run APA 7-ready statistical analyses in R (descriptives, frequencies/crosstabs, correlations, t-tests, assumption checks, scale reliability, data exploration, missingness handling, data transforms) from CSV/RDS/RData/SAV with JSONL logs and templated reports.
license: Apache-2.0
compatibility: R 4.0+, Windows, WSL (Ubuntu), Linux
metadata:
  author: Prof. Dr. Mike Hammes, ISM International School of Management, Germany (mike.hammes@ism.de)
  version: 0.1.0
  created: 2025-12-20
  backend: R
  style: APA 7
---

# Statistic Skills (Shared Workflow)

## Overview

Central guidance for all statistic skills in this repo, plus shared conventions for running R scripts and placing outputs.

## Configuration Defaults and Overrides

All modules load defaults from `scripts/config.yml` (requires the R package `yaml`; otherwise built-in defaults in `scripts/R/lib/config.R` apply). Use the standard configuration unless the user specifies other parameter flags or the requested analysis implies them (for example, cross-correlations imply `--x` and `--y`, partial correlations imply `--controls`).

CLI flags always override `scripts/config.yml` defaults at runtime.

## Shared wrapper: `scripts/run_rscript.ps1`

On Windows, the wrapper prefers WSL (Ubuntu/Linux) and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

Example:

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/<subskill-name>.R> --csv <path to CSV file> --vars <variables>
```

If the script path is omitted, the wrapper falls back to the default configured inside the wrapper script.

## WSL/Linux direct usage (optional)

Inside WSL or Linux, run `Rscript` directly with the same arguments.

Example:

```bash
Rscript <path to scripts/R/<subskill-name>.R> --csv <path to CSV file> --vars <variables>
```

## Common inputs (data sources)

All scripts accept one of the following input types:

- `--csv <path>`: CSV file (use `--sep` and `--header` if needed).
- `--sav <path>`: SPSS `.sav` file.
- `--rds <path>`: RDS file containing a data frame.
- `--rdata <path>`: RData file; also pass `--df <data_frame_name>` to select the data frame.
- `--interactive`: Prompt for inputs if you want a guided run.

## Common flags

- `--sep <char>`: CSV separator (default from `scripts/config.yml` -> `defaults.csv.sep`).
- `--header TRUE/FALSE`: CSV header row (default from `scripts/config.yml` -> `defaults.csv.header`).
- `--log TRUE/FALSE`: Append to `analysis_log.jsonl` (default from `scripts/config.yml` -> `defaults.log`).
- `--user-prompt <text>`: Store the original AI user prompt in the JSONL log (required: always pass the last user message when an analysis is requested).
- `--digits <n>`: Rounding for APA output where supported (default from `scripts/config.yml` -> `defaults.digits`).

Module-specific analysis options (variables, grouping, method choices, etc.) are described in each subskill reference.

## Output conventions

- Use `defaults.output_dir` from `scripts/config.yml` for scratch outputs (defaults to `<working directory>/outputs/tmp`, relative to the working directory where the script runs).
- The output directory is fixed to `defaults.output_dir` and is not user-overridable.
- Each analysis appends `apa_report.md` (APA table + narrative) and appends `analysis_log.jsonl` when logging is enabled.
- For `apa_report.md`, templates in `core-stats/assets` must always be used when available.
- Keep outputs as plain text, Markdown, or JSONL so Codex can summarize them.

## APA Template System (YAML)

APA templates are Markdown files with optional YAML front matter and `{{token}}` placeholders. They can control table columns, notes, and narrative text.

- Template selection is configurable in `scripts/config.yml` under `templates.*` (e.g., `templates.descriptive_stats.default`, `templates.crosstabs.grouped`, `templates.correlations.cross`).
- YAML front matter supports:
  - `tokens`: static or derived tokens that can be referenced in the template body.
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text; defaults to `{{note_default}}` if omitted.
  - `narrative.template` or `narrative.row_template`: overrides narrative text. `row_template` renders one line per result row; it can be combined with `narrative.join` and `narrative.drop_empty`.
- Base tokens available in all templates: `analysis_label`, `analysis_flags`, `table_number`, `table_body`, `note_body`, `note_default`, `narrative`, `narrative_default`.
- Module-specific tokens (e.g., correlation CI labels or cross-tab test fragments) are documented in each subskill reference.
- Modules without template mappings fall back to the built-in APA report format (no YAML template).

## Subskills

- [descriptive-stats](references/descriptive-stats.md): Numeric descriptive statistics with optional grouping, missingness, distribution metrics, and APA tables/narratives.
- [frequencies](references/frequencies.md): Categorical frequency tables with percentages, grouping, and APA-ready tables/narratives.
- [crosstabs](references/crosstabs.md): Contingency tables with chi-square/Fisher tests, effect sizes, residuals, and APA outputs.
- [correlations](references/correlations.md): Correlation matrices, cross/partial correlations, p-adjustments, optional grouping, and APA outputs.
- [scale](references/scale.md): Scale item analysis and reliability (alpha/omega), reverse scoring, grouped results, APA outputs.
- [data-explorer](references/data-explorer.md): Data dictionary summaries (types, levels, missingness, value counts) with APA output.
- [data-transform](references/data-transform.md): Create/modify variables (compute, recode, standardize, rename, drop) with change logs and updated datasets.
- [assumptions](references/assumptions.md): Assumption checks for t-tests, ANOVA, and regression (including diagnostics).
- [t-test](references/t-test.md): One-sample, independent-samples, and paired-samples t-tests with APA outputs.
- [missings](references/missings.md): Missing-data pattern summaries, method selection, and handled datasets with APA outputs.
