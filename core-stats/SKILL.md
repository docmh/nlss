---
name: core-stats
description: Run APA 7-ready statistical analyses in R (descriptives, frequencies/crosstabs, correlations, regression, mixed models, SEM/CFA/mediation, ANOVA, t-tests, nonparametric tests, assumption checks, scale reliability, data exploration, plotting, missingness handling, data transforms, workspace initialization) from CSV/RDS/RData/SAV/Parquet with JSONL logs and templated reports.
license: Apache-2.0
compatibility: R 4.0+, Windows, WSL (Ubuntu), Linux
metadata:
  author: Prof. Dr. Mike Hammes, ISM International School of Management, Germany (mike.hammes@ism.de)
  version: 0.1.0
  created: 2025-12-20
  backend: R
  style: APA 7
---

# NLSS - Natural Language Statistics Suite

## Overview

Central guidance for all statistic skills in this repo, plus shared conventions for running R scripts and placing outputs.

## Stateful workspace workflow (required)

Treat the workspace root as the current working directory, its parent, or a one-level child containing `core-stats-workspace.yml` (fallback: `defaults.output_dir` from `scripts/config.yml`). It should only contain dataset subfolders.

1. Ensure the workspace root exists (manifest in current dir, parent, or child; fallback to `defaults.output_dir`).
2. For each dataset, ensure a dataset workspace folder exists at `<workspace-root>/<dataset-name>/` containing `scratchpad.md` and `apa_report.md`. If missing, run the `init-workspace` subskill first.
3. Confirm a workspace copy exists as `<workspace-root>/<dataset-name>/<dataset-name>.parquet` (dataset name = filename stem or `--df`, sanitized). If missing, create it via `init-workspace` before running analyses.
4. All subskills must operate on the workspace `.parquet` copy (prefer `--parquet` pointing to the workspace copy, or rely on auto-copy behavior).
5. Direct workspace runs (no input flags) should load the dataset from the current dataset folder if applicable; otherwise use `active_dataset` from the manifest.
6. Workspaces must be non-nested and unique per parent folder; if nested or sibling manifests are detected, stop and ask the user to resolve them.
7. Before running any `.R` analysis script, check the dataset’s `analysis_log.jsonl` for an exact prior run (same module + same command/flags + same input dataset; ignore differences in `--user-prompt`). When searching JSONL logs in PowerShell, use single quotes for the pattern and path; do not backslash-escape quotes (PowerShell treats `\` literally). Examples: `rg -F '"module"' -- 'C:\path\to\analysis_log.jsonl'` or `rg -F '"module":"scale"' -- 'C:\path\to\analysis_log.jsonl'`. If a match exists, do not rerun; report results from the prior outputs (`apa_report.md` and the matching log entry) instead.
8. Before analysis: read and update the dataset’s `scratchpad.md` with the analysis plan and dataset considerations.
9. After analysis: update the dataset’s `scratchpad.md` again with decisions, transformations, missing-handling actions, and derived variables/scales.

Note: `data-transform` and `missings` update the workspace `.parquet` copy in place and create a backup at `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting. Undo = replace the current parquet with the latest backup.

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

### PowerShell wrapper cheat sheet

- First argument must be the `.R` script path; all other flags come after it.
- Lists use comma-separated values with no spaces: `x1,x2,x3`.
- Quote values that contain spaces or semicolons (for example `--blocks "x1,x2;x3,mediator"`).
- Relative paths are resolved from the current PowerShell working directory; use absolute paths when in doubt.

Examples:

```powershell
# From workspace root, using the manifest + active dataset
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --vars "x1,x2,x3"

# Absolute CSV path with spaces
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --csv "C:\Users\me\My Data\study.csv" --vars "x1,x2,x3"

# Workspace parquet copy (preferred)
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --parquet "C:\workspaces\core-stats\study\study.parquet" --vars "x1,x2,x3"

# Blocks with semicolons need quotes
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/regression.R> --dv outcome --blocks "x1,x2;x3,mediator"
```

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
- `--parquet <path>`: Parquet file (preferred workspace format).
- `--interactive`: Prompt for inputs if you want a guided run.

Notes:

- Inputs must be local filesystem paths accessible to R. URLs or cloud share links are not supported; download first.
- On Windows, the PowerShell wrapper converts Windows paths to WSL paths automatically; for WSL direct runs use `/mnt/<drive>/...`.

## Common flags

- `--sep <char>`: CSV separator (default from `scripts/config.yml` -> `defaults.csv.sep`).
- `--header TRUE/FALSE`: CSV header row (default from `scripts/config.yml` -> `defaults.csv.header`).
- `--log TRUE/FALSE`: Append to `analysis_log.jsonl` (default from `scripts/config.yml` -> `defaults.log`).
- `--user-prompt <text>`: Store the original AI user prompt in the JSONL log (required: always pass the last user message when an analysis is requested).
- `--digits <n>`: Rounding for APA output where supported (default from `scripts/config.yml` -> `defaults.digits`).
- `--template <ref|path>`: Select a template key (e.g., `default`, `grouped`) or a direct template path; falls back to default selection when not found.

Module-specific analysis options (variables, grouping, method choices, etc.) are described in each subskill reference.

## Output conventions

- Use the workspace root in the current directory, its parent, or a one-level child if `core-stats-workspace.yml` is present; otherwise fall back to `defaults.output_dir` from `scripts/config.yml`.
- The output directory is fixed to the resolved workspace root and is not user-overridable.
- Each analysis appends `apa_report.md` (APA table + narrative) and `analysis_log.jsonl` inside `<workspace-root>/<dataset-name>/` when logging is enabled.
- Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- For `apa_report.md`, templates in `core-stats/assets` must always be used when available.
- Keep outputs as plain text, Markdown, or JSONL so Codex can summarize them.

## APA Template System (YAML)

APA templates are Markdown files with optional YAML front matter and `{{token}}` placeholders. They can control table columns, notes, and narrative text.

- Template selection is configurable in `scripts/config.yml` under `templates.*` (e.g., `templates.descriptive_stats.default`, `templates.crosstabs.grouped`, `templates.correlations.cross`).
- CLI runs can override the selection with `--template <ref|path>` when needed.
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
- [plot](references/plot.md): APA-ready figures (histograms, bar charts, box/violin, scatter/line, QQ, correlation heatmaps) with numbered captions.
- [data-transform](references/data-transform.md): Create/modify variables (compute, recode, standardize, rename, drop) with change logs and updated datasets.
- [assumptions](references/assumptions.md): Assumption checks for t-tests, ANOVA, and regression (including diagnostics).
- [regression](references/regression.md): Multiple and hierarchical regression with interactions, bootstrap options, and APA outputs.
- [mixed-models](references/mixed-models.md): Linear mixed-effects models with random effects, marginal means, and APA outputs.
- [sem](references/sem.md): Structural equation modeling (SEM), CFA, path analysis, mediation, and invariance with APA outputs.
- [anova](references/anova.md): Between-subjects, within-subjects, and mixed ANOVA with APA outputs and post-hoc comparisons.
- [t-test](references/t-test.md): One-sample, independent-samples, and paired-samples t-tests with APA outputs.
- [nonparametric](references/nonparametric.md): Wilcoxon, Mann-Whitney, Kruskal-Wallis, and Friedman tests with APA outputs.
- [missings](references/missings.md): Missing-data pattern summaries, method selection, and handled datasets with APA outputs.
- [init-workspace](references/init-workspace.md): Initialize per-dataset workspace folders with scratchpad.md, apa_report.md, analysis_log.jsonl, and .parquet dataset copies.
