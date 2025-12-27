---
name: nlss
description: Run APA 7-ready statistical analyses in R via subskills and metaskills (Markdown pseudoscripts such as describe-sample, check-instruments, explore-data, prepare-data, plan-power, generate-r-script, and test-hypotheses) covering descriptives, frequencies/crosstabs, correlations, power analysis, regression, mixed models, SEM/CFA/mediation, ANOVA, t-tests, nonparametric tests, assumption checks, scale reliability, inter-rater reliability/ICC, data exploration, plotting, missingness handling, imputation, data transforms, and workspace initialization from CSV/RDS/RData/SAV/Parquet with JSONL logs and templated reports.
license: Apache-2.0
compatibility: R 4.5.2, Windows, WSL (Ubuntu), Linux, Codex/GPT-5.2-Codex
metadata:
  author: Prof. Dr. Mike Hammes, ISM International School of Management, Germany (mike.hammes@ism.de)
  version: 1.0.0
  created: 2025-12-20
  updated: 2025-12-27
  backend: R
  style: APA 7
---

# nlss - Natural Language Statistics Suite

## Overview

Central guidance for NLSS as an assistant researcher, plus shared conventions for running R scripts and placing outputs.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Metaskills Overview

Metaskills are Markdown pseudoscripts that orchestrate subskills based on user intent (for example, "describe the sample"). The agent is the runner: it starts with a dataset inspection, asks clarifying questions when needed, and then runs the listed subskills while updating the dataset scratchpad.

**NLSS-first principle:** for reliability and auditability, prefer existing subskills whenever they cover the request; only use custom script generation as a last resort.

## Stateful workspace workflow (required)

Treat the workspace root as the current working directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir` from `scripts/config.yml`). It should only contain dataset subfolders.

1. Ensure the workspace root exists (manifest in current dir, parent, or child; fallback to `defaults.output_dir`).
2. For each dataset, ensure a dataset workspace folder exists at `<workspace-root>/<dataset-name>/` containing `scratchpad.md` and `report_canonical.md`. If missing, run the `init-workspace` subskill first.
3. Confirm a workspace copy exists as `<workspace-root>/<dataset-name>/<dataset-name>.parquet` (dataset name = filename stem or `--df`, sanitized). If missing, create it via `init-workspace` before running analyses.
4. All subskills must operate on the workspace `.parquet` copy (prefer `--parquet` pointing to the workspace copy, or rely on auto-copy behavior).
5. Direct workspace runs (no input flags) should load the dataset from the current dataset folder if applicable; otherwise use `active_dataset` from the manifest.
6. Workspaces must be non-nested and unique per parent folder; if nested or sibling manifests are detected, stop and ask the user to resolve them.
7. Before running any `.R` analysis script, check the dataset’s `analysis_log.jsonl` for an exact prior run (same module + same command/flags + same input dataset; ignore differences in `--user-prompt`). When searching JSONL logs in PowerShell, use single quotes for the pattern and path; do not backslash-escape quotes (PowerShell treats `\` literally). Examples: `rg -F '"module"' -- 'C:\path\to\analysis_log.jsonl'` or `rg -F '"module":"scale"' -- 'C:\path\to\analysis_log.jsonl'`. If a match exists, do not rerun; report results from the prior outputs (`report_canonical.md` and the matching log entry) instead.
8. For metaskills, inspect the dataset first and write a step-by-step plan to `scratchpad.md` before running subskills; update the plan after each step.
9. Before analysis: read and update the dataset’s `scratchpad.md` with the analysis plan and dataset considerations.
10. After analysis: update the dataset’s `scratchpad.md` again with decisions, transformations, missing-handling actions, and derived variables/scales.

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
- For paths with non-ASCII characters (for example, umlauts), the wrapper prefers Windows Rscript when available; set `NLSS_FORCE_WSL=1` to keep WSL, or `NLSS_SKIP_WSL=1` to always skip WSL.
- If a path arrives with mangled characters (for example `?`), the wrapper attempts to repair it by matching on-disk names.

Examples:

```powershell
# From workspace root, using the manifest + active dataset
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --vars "x1,x2,x3"

# Absolute CSV path with spaces
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --csv "C:\Users\me\My Data\study.csv" --vars "x1,x2,x3"

# Workspace parquet copy (preferred)
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/descriptive_stats.R> --parquet "C:\workspaces\nlss\study\study.parquet" --vars "x1,x2,x3"

# Blocks with semicolons need quotes
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/R/regression.R> --dv outcome --blocks "x1,x2;x3,mediator"
```

## WSL/Linux direct usage (optional)

Inside WSL or Linux, run `Rscript` directly with the same arguments.

Example:

```bash
Rscript <path to scripts/R/<subskill-name>.R> --csv <path to CSV file> --vars <variables>
```

## Metaskills Execution

- Metaskills live as Markdown pseudoscripts under `nlss/references/metaskills/` and are selected by the agent from the user prompt or an explicitly named metaskill.
- The agent inspects the dataset first, infers candidate variables, and asks clarifying questions only when needed.
- Enforce the NLSS-first principle: only use `generate-r-script` when the request is out of NLSS scope and explicit permission is granted; save generated scripts to `<workspace-root>/<dataset-name>/scripts/` and document the path in `scratchpad.md`.
- Each metaskill step calls the existing subskill scripts so templates, JSONL logs, and workspace conventions are reused.
- On completion, log metaskill finalization, append a `# Synopsis` section to `report_canonical.md`, and generate `report_<YYYYMMDD>_<metaskill>_<intent>.md` with APA 7-ready, journal-ready narrative, tables, and plots when helpful.
- The agent writes a plan to `scratchpad.md` and marks progress after each step.

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

## Metaskill Inputs

Metaskills use the same data sources as subskills (CSV/SAV/RDS/RData/Parquet or workspace context). The agent should capture:

- User intent (prompt text or explicit metaskill name).
- Dataset source (file path or workspace context).
- Any clarifications (grouping variables, Likert handling, etc.) provided in the prompt or follow-ups.

## Common flags

- `--sep <char>`: CSV separator (default from `scripts/config.yml` -> `defaults.csv.sep`).
- `--header TRUE/FALSE`: CSV header row (default from `scripts/config.yml` -> `defaults.csv.header`).
- `--log TRUE/FALSE`: Append to `analysis_log.jsonl` (default from `scripts/config.yml` -> `defaults.log`).
- `--user-prompt <text>`: Store the original AI user prompt in the JSONL log (required: always pass the last user message when an analysis is requested).
- `--digits <n>`: Rounding for APA output where supported (default from `scripts/config.yml` -> `defaults.digits`).
- `--template <ref|path>`: Select a template key (e.g., `default`, `grouped`) or a direct template path; falls back to default selection when not found.

Module-specific analysis options (variables, grouping, method choices, etc.) are described in each subskill reference.

## Output conventions

- Use the workspace root in the current directory, its parent, or a one-level child if `nlss-workspace.yml` is present; otherwise fall back to `defaults.output_dir` from `scripts/config.yml`.
- The output directory is fixed to the resolved workspace root and is not user-overridable.
- Each analysis appends `report_canonical.md` (APA table + narrative) and `analysis_log.jsonl` inside `<workspace-root>/<dataset-name>/` when logging is enabled.
- All artifacts (reports, tables, figures, scripts) must be created inside the dataset workspace folder; do not create files or folders outside the workspace root.
- Subskills do not create separate report files; they only extend `report_canonical.md`. Standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.
- The agent logs a meta entry in `analysis_log.jsonl` and each subskill run logs its own entry as usual.
- Metaskill finalization appends a `# Synopsis` section to `report_canonical.md` and creates `report_<YYYYMMDD>_<metaskill>_<intent>.md` inside the dataset workspace.
- When `defaults.log_nlss_checksum` is true, log entries include a `checksum` field that XOR-combines the `nlss/` folder checksum with a checksum of the log entry content (excluding the checksum field), so it can be reverted for tamper checks.
- Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- For `report_canonical.md`, templates in `nlss/assets` must always be used when available.
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
- Metaskills do not define APA templates for `report_canonical.md`; APA output is produced by their underlying subskills. Final metaskill reports should follow `nlss/assets/metaskills/report-template.md` unless a different structure is warranted.

## Subskills

- [descriptive-stats](references/subskills/descriptive-stats.md): Numeric descriptives with grouping, missingness, robust/percentile/outlier metrics, and APA templates (default/robust/distribution).
- [frequencies](references/subskills/frequencies.md): Categorical frequency tables with percentages, grouping, and APA-ready tables/narratives.
- [crosstabs](references/subskills/crosstabs.md): Contingency tables with chi-square/Fisher tests, effect sizes, residuals, and APA outputs.
- [correlations](references/subskills/correlations.md): Correlation and cross-correlation matrices, partial correlations, p-adjustments, bootstrap CIs, Fisher r-to-z tests, optional grouping, and APA outputs.
- [scale](references/subskills/scale.md): Scale item analysis and reliability (alpha/omega), reverse scoring, grouped results, APA outputs.
- [reliability](references/subskills/reliability.md): Inter-rater and test-retest reliability (ICC, kappa, correlations) with APA outputs.
- [data-explorer](references/subskills/data-explorer.md): Data dictionary summaries (types, levels, missingness, value counts) with APA output.
- [plot](references/subskills/plot.md): APA-ready figures (histograms, bar charts, box/violin, scatter/line, QQ, correlation heatmaps) with numbered captions.
- [data-transform](references/subskills/data-transform.md): Create/modify variables (compute, recode, standardize, rename, drop) with change logs and updated datasets.
- [assumptions](references/subskills/assumptions.md): Assumption checks for t-tests, ANOVA, and regression (including diagnostics).
- [regression](references/subskills/regression.md): Multiple and hierarchical regression with interactions, bootstrap options, and APA outputs.
- [power](references/subskills/power.md): Power analysis for t-tests, ANOVA, correlations, regression, and SEM with APA outputs.
- [mixed-models](references/subskills/mixed-models.md): Linear mixed-effects models with random effects, marginal means, and APA outputs.
- [sem](references/subskills/sem.md): Structural equation modeling (SEM), CFA, path analysis, mediation, and invariance with APA outputs.
- [anova](references/subskills/anova.md): Between-subjects, within-subjects, and mixed ANOVA with APA outputs and post-hoc comparisons.
- [t-test](references/subskills/t-test.md): One-sample, independent-samples, and paired-samples t-tests with APA outputs.
- [nonparametric](references/subskills/nonparametric.md): Wilcoxon, Mann-Whitney, Kruskal-Wallis, and Friedman tests with APA outputs.
- [missings](references/subskills/missings.md): Missing-data pattern summaries, method selection, and handled datasets with APA outputs.
- [impute](references/subskills/impute.md): Imputation into new _imp columns with optional mice/VIM engines and APA outputs.
- [init-workspace](references/subskills/init-workspace.md): Initialize per-dataset workspace folders with scratchpad.md, report_canonical.md, analysis_log.jsonl, and .parquet dataset copies.
- [metaskill-runner](references/subskills/metaskill-runner.md): Log metaskill activations to report_canonical.md and analysis_log.jsonl.

## Metaskills

### General Approach

- Run the specified pseudoscript and ask clarifying questions if needed.
- Inspect the dataset first to infer likely variable candidates and defaults.
- Log the metaskill activation using the `metaskill-runner` subskill.
- Execute the listed subskills in order, reusing the workspace `.parquet` copy.
- Update the dataset `scratchpad.md` with the plan and progress after each step.

### Metaskill Report Requirements

- `report_canonical.md` is an audit trail; never copy it as the final metaskill report.
- `report_<YYYYMMDD>_<metaskill>_<intent>.md` must be newly written, APA 7–aligned, and journal-ready.
- Use `nlss/assets/metaskills/report-template.md` as the default structure; omit Introduction and Keywords if the theoretical context is not available.
- Use standard journal subsections when they fit (Methods: Participants/Measures/Procedure/Analytic Strategy; Results: Preliminary/Primary/Secondary; Discussion: Summary/Limitations/Implications/Future Directions), but rename or replace them when the metaskill warrants it.
- Synthesize results across subskills with interpretation; do not just list outputs.
- Craft tables and figures specifically for the report; do not copy/paste from `report_canonical.md`. Include them only when they improve comprehension, and reference them in text with captions.
- Keep all metaskill artifacts inside the dataset workspace folder; never write outside the workspace root.

### Available Metaskills

- [describe-sample](references/metaskills/describe-sample.md): Demographic-first sample description with inferred variables, descriptive stats, and frequency tables for vague "describe the sample" requests.
- [generate-r-script](references/metaskills/generate-r-script.md): Generate a custom R script for analyses not covered by NLSS (permission required).
- [check-instruments](references/metaskills/check-instruments.md): Instrument quality checks via item analysis, reverse scoring, and reliability (alpha/omega, ICC/kappa) with defaults for vague requests.
- [test-hypotheses](references/metaskills/test-hypotheses.md): Map vague hypotheses to tests with clarifications, run the appropriate analyses, and report APA-ready outputs.
- [plan-power](references/metaskills/plan-power.md): A priori power analysis to plan minimal sample size for t-tests, ANOVA, regression, correlations, and SEM.
- [explore-data](references/metaskills/explore-data.md): Dataset overview with variable summaries, missingness, distributions, and correlations for vague "overview" requests.
- [prepare-data](references/metaskills/prepare-data.md): Data cleaning and preparation (missing handling, recodes, transformations) based on dataset inspection and user intent.
- check-assumptions (planned): Assumption checks for planned analyses (t-tests, ANOVA, regression).
- full-analysis (planned): Comprehensive analysis from data exploration to hypothesis testing with full reporting.

## Utilities

- [calc](references/utilities/calc.md): Lightweight calculator CLI for quick numeric expressions.
