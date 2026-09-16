---
name: efa
description: Exploratory factor analysis with PCA/EFA extraction, rotation, eigenvalue retention, KMO/Bartlett diagnostics, and NLSS format outputs.
license: Apache-2.0
---

# Exploratory Factor Analysis (Psych, NLSS format)

## Overview

Run exploratory factor analysis (EFA) or PCA with rotation using psych, and generate NLSS format tables and narrative text (loadings + diagnostics).

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Choose item variables and optional grouping variable.
3. Choose extraction method, rotation, factor retention rule, and correlation type.
4. Run `scripts/R/efa.R` with the correct flags.
5. Use outputs (`report_canonical.md`, `result.json`) to craft the response.

## Script: `scripts/R/efa.R`

### CSV Input

```bash
Rscript <path to scripts/R/efa.R> --csv <path to CSV file> --vars item1,item2,item3
```

### Parquet Input

```bash
Rscript <path to scripts/R/efa.R> --parquet <path to parquet file> --vars item1,item2,item3
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/efa.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.efa.vars_default` (typically numeric columns) if omitted.
- `--group` is optional and produces grouped EFA results.
- `--method` sets extraction method: `pca`, `pa`, `minres`, `ml`, `uls`, `gls`, `wls`, or `alpha` (default: `modules.efa.method`). PCA is a component reduction, not a common-factor measurement model; choose deliberately.
- `--rotation` sets rotation (default: `modules.efa.rotation`, e.g., `varimax`). Existing psych rotations remain available, including `none`, `promax`, `oblimin`, `quartimax`, `equamax`, `bentlerQ` and `geominQ`. Canonical case matters to psych and is preserved. Rotations requiring additional target specifications remain subject to psych's existing argument requirements; NLSS does not invent targets or silently use no rotation after a failed request.
- `--n-factors` sets a positive integer not exceeding the item count, or `eigen` (default: `modules.efa.n_factors`). The retained legacy eigen rule uses at least one factor; if no eigenvalue exceeds the threshold, that fallback is explicitly warned and recorded. It is not evidence supporting a one-factor model.
- `--eigen-threshold` sets the eigenvalue cutoff (default: `modules.efa.eigen_threshold`).
- `--cor` selects the correlation type (default: `modules.efa.cor`): `pearson`, `spearman`, `polychoric`, `tetrachoric`.
- `--missing` selects missing handling (default: `modules.efa.missing`): `pairwise` or `complete`.
- `--loading-cutoff` suppresses displayed loadings whose unrounded absolute value is below the cutoff (default: `modules.efa.loading_cutoff`). It never removes raw pattern or structure loadings from the saved result.
- `--sort-loadings` sorts rows by primary loading (default: `modules.efa.sort_loadings`).
- `--coerce` coerces non-numeric columns to numeric (default: `modules.efa.coerce`).
- `--seed` fixes psych extraction/rotation randomness (default: `modules.efa.seed`, 1). The installed psych rotation-start count is retained and recorded (psych 2.6.5 uses 20 starts in `fa`, 1 in `principal`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Outputs

This migrated adapter uses the [shared run contract](../run-contract.md). Each run preserves `request.json`, unrounded `result.json`, copied templates and deterministic `output.md` under `.nlss/runs/<id>/`. Verified replay needs no language model. `--log FALSE` does not suppress the run bundle or root protocol. An analysis failure must not publish a normal output or partially append successful groups to the canonical report.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing loadings table and narrative summary.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- Legacy `loadings_df`, `summary_df` and `eigen_df` remain. `groups` additionally retains full correlation, pattern, structure, Phi, reproduced/residual and variance-accounted matrices, thresholds where available, smoothing, KMO/Bartlett availability and fit warnings. Matrix records contain row/column names and row-wise values. `F1`, `F2`, etc. are output labels; original psych factor names are recorded separately.

## Data roles and audit boundaries

- Numeric SPSS labels do not alter numerical scores or reorder categories. The import contract masks user-missing codes before EFA, with the original dictionary preserved. Pearson/Spearman require numeric items unless `--coerce TRUE`; conversion losses are warned and recorded with source-row indices. Selected infinite values are errors, not missing observations.
- Polychoric/tetrachoric analysis uses ascending numeric codes or declared factor-level order. Text categories retain the legacy locale-sorted order with an explicit warning; confirm their substantive order first. All categories receive consecutive analysis codes, so numeric labels such as 10/20/30 are three categories rather than a spurious 21-category range. Original values, analysis codes and order metadata are saved. Every group/item must contain at least two observed categories; tetrachoric requires exactly two.
- Groups are partitioned by their actual values, not label strings. Missing groups remain distinct from a real category named `NA`; nearby numeric identities remain distinct. The request stores typed group identity, source rows, complete/item-valid rows, actual per-pair counts and rows used for estimation.
- Complete deletion uses the joint complete cases. Pairwise analysis retains pair-specific counts; its nominal `n_obs` counts rows with at least one observed item and is not a claim of one common sample across correlations.
- Categorical correlations preserve psych's continuity correction (.5), global-threshold behavior and smoothing, recording raw and effective matrices. If psych switches from requested global to local thresholds because category counts differ, the warning and effective setting are preserved. A non-positive-semidefinite Pearson/Spearman matrix fails explicitly. Singular matrices are not rejected categorically when psych can estimate the requested solution, but unavailable diagnostics are disclosed.
- The conventional Bartlett formula is flagged as approximate for pairwise, rank or latent correlations; its common-sample Pearson assumptions are not established by the nominal row count. KMO/Bartlett failure is not a successful diagnostic result. Fit warnings and improper communalities/uniquenesses remain visible.
- Communalities include factor correlations: `diag(L %*% Phi %*% t(L))`; squared pattern loadings alone are insufficient for oblique solutions. For common-factor extraction, `variance_explained` is the extracted common variance divided by the number of items. The original correlation eigenvalues remain in `eigen_df`, not mislabeled as the extracted factor solution's variance. Under oblique rotation, per-factor variance components overlap and should not be interpreted as independent causal contributions.

## NLSS format Template (YAML)

Use the Markdown template at `assets/efa/default-template.md` when assembling EFA outputs. If the template exists, it must be used for `report_canonical.md`.

- Template paths can be overridden via `templates.efa.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`item`, `group`, `factor`, `loading`, `h2`, `u2`, `complexity`, `cross_loading`.

Use `drop_if_empty: true` to remove a column if all values are blank.

### Note Tokens

Available note tokens include:

`note_default`, `method`, `rotation`, `correlation`, `missing`, `eigen_threshold`, `loading_cutoff`, `kmo`, `bartlett`.

### Narrative Tokens

Use `narrative.row_template` for per-group lines. Available row tokens include:

`group`, `group_label`, `n_obs`, `n_items`, `n_factors`, `variance_explained`, `kmo`, `bartlett`, `full_sentence`.

## NLSS format Reporting Guidance

- Report KMO and Bartlett tests before interpreting loadings.
- Report the factor retention rule (eigen > 1 vs fixed), rotation type, and total variance explained.
- Use a loading cutoff (e.g., .30) and note cross-loadings.
- Distinguish component reduction from common-factor analysis and pattern from structure loadings. Inspect Phi, inadmissible estimates, smoothing and sample limitations before interpreting factors.
- These deterministic tables are evidence for a freely written final report, not a report template that determines the interpretation. Factor naming, theoretical coherence, competing structures and the practical meaning of cross-loadings require semantic judgment. Do not infer construct validity from KMO, Bartlett significance or an eigenvalue rule alone.

## Notes

- EFA requires the `psych` package.
- Parquet inputs require the `arrow` package.
- Authoritative method references: [psych factor analysis](https://personality-project.org/r/psych/help/fa.html), [principal components](https://personality-project.org/r/psych/help/principal.html), and [categorical correlations](https://personality-project.org/r/psych/help/tetrachor.html). Installed package formals and saved package versions determine the executed defaults when an online manual differs.
