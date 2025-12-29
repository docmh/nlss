---
name: efa
description: Exploratory factor analysis with PCA/EFA extraction, rotation, eigenvalue retention, KMO/Bartlett diagnostics, and APA outputs.
---

# Exploratory Factor Analysis (psych, APA 7)

## Overview

Run exploratory factor analysis (EFA) or PCA with rotation using psych, and generate APA 7-style tables and narrative text (loadings + diagnostics).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose item variables and optional grouping variable.
3. Choose extraction method, rotation, factor retention rule, and correlation type.
4. Run `scripts/R/efa.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/efa.R`

### CSV input

```bash
Rscript <path to scripts/R/efa.R> --csv <path to CSV file> --vars item1,item2,item3
```

### Parquet input

```bash
Rscript <path to scripts/R/efa.R> --parquet <path to parquet file> --vars item1,item2,item3
```

### Interactive prompts

```bash
Rscript <path to scripts/R/efa.R> --interactive
```

## Options

Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.efa.vars_default` (typically numeric columns) if omitted.
- `--group` is optional and produces grouped EFA results.
- `--method` sets extraction method (default: `modules.efa.method`, e.g., `pca`).
- `--rotation` sets rotation (default: `modules.efa.rotation`, e.g., `varimax`).
- `--n-factors` sets factor count (default: `modules.efa.n_factors`, `eigen` for eigen > 1).
- `--eigen-threshold` sets the eigenvalue cutoff (default: `modules.efa.eigen_threshold`).
- `--cor` selects the correlation type (default: `modules.efa.cor`): `pearson`, `spearman`, `polychoric`, `tetrachoric`.
- `--missing` selects missing handling (default: `modules.efa.missing`): `pairwise` or `complete`.
- `--loading-cutoff` suppresses loadings below the cutoff (default: `modules.efa.loading_cutoff`).
- `--sort-loadings` sorts rows by primary loading (default: `modules.efa.sort_loadings`).
- `--coerce` coerces non-numeric columns to numeric (default: `modules.efa.coerce`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- `report_canonical.md`: APA 7 report containing loadings table and narrative summary.
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA 7 Template (YAML)

Use the Markdown template at `nlss/assets/efa/default-template.md` when assembling EFA outputs. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.efa.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table column keys

Available column keys for `table.columns` include:

`item`, `group`, `factor`, `loading`, `h2`, `u2`, `complexity`, `cross_loading`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note tokens

Available note tokens include:

`note_default`, `method`, `rotation`, `correlation`, `missing`, `eigen_threshold`, `loading_cutoff`, `kmo`, `bartlett`.

### Narrative tokens

Use `narrative.row_template` for per-group lines. Available row tokens include:

`group`, `group_label`, `n_obs`, `n_items`, `n_factors`, `variance_explained`, `kmo`, `bartlett`, `full_sentence`.

## APA 7 Reporting Guidance

- Report KMO and Bartlett tests before interpreting loadings.
- Report the factor retention rule (eigen > 1 vs fixed), rotation type, and total variance explained.
- Use a loading cutoff (e.g., .30) and note cross-loadings.

## Notes

- EFA requires the `psych` package.
- Parquet inputs require the `arrow` package.
