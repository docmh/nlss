---
name: sem-lavaan
description: SEM/CFA/path/mediation/invariance via lavaan with model builders, multi-group support, robust or bootstrapped SEs/CIs, fit indices, standardized estimates, and APA outputs.
---

# SEM (lavaan, APA 7)

## Overview

Run SEM/CFA/path/mediation models with lavaan and produce APA 7-style tables and narrative text. Supports multi-group models, robust estimators, bootstrap CIs, and invariance sequences.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Provide a lavaan model (`--model` / `--model-file`) or use a builder (`--factors`, `--dv`/`--ivs`, or `--x`/`--m`/`--y`).
3. Choose estimator, missing handling, SE/CI, and fit indices.
4. Run `scripts/R/sem.R` with the correct flags, or use the PowerShell wrapper on Windows.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/sem.R`

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\sem.R> --csv <path to CSV> --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

### CSV input

```bash
Rscript <path to scripts/R/sem.R> --csv <path to CSV> --analysis sem --model "y ~ x1 + x2"
```

### Parquet input

```bash
Rscript <path to scripts/R/sem.R> --parquet <path to parquet> --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

### Interactive prompts

```bash
Rscript <path to scripts/R/sem.R> --interactive
```

## Options

Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

### Inputs

- `--csv`, `--sav`, `--rds`, `--rdata`, `--df`, `--parquet`: standard inputs (see shared workflow).
- `--sep`, `--header`: CSV parsing; defaults from `defaults.csv.sep` and `defaults.csv.header`.

### Model selection

- `--analysis`: `sem`, `cfa`, `path`, `mediation`, or `invariance` (default: `modules.sem.analysis`).
- `--model`: lavaan syntax in a quoted string.
- `--model-file`: path to a lavaan syntax file.
- `--paths`: alias for `--model` (convenience for path models).
- `--factors`: CFA builder string: `F1=item1,item2;F2=item3,item4`.
- `--dv` and `--ivs`: path builder (`dv ~ iv1 + iv2`).
- `--x`, `--m`, `--y`: mediation builder (`--m` supports multiple mediators).
- `--covariates`: optional covariates added to mediator/outcome equations.
- `--serial`: serial mediation (currently supports two mediators).

### Grouping and invariance

- `--group`: multi-group variable.
- `--group-equal`: lavaan `group.equal` constraints.
- `--invariance`: steps for invariance (default: `modules.sem.invariance`), e.g. `configural,metric,scalar,strict`.
- `--analysis invariance` requires `--group`.

### Estimation

- `--estimator`: ML, MLR, MLM, MLMV, MLMVS, WLSMV, ULSMV, DWLS (default: `modules.sem.estimator`).
- `--missing`: `fiml`, `listwise`, `pairwise` (default: `modules.sem.missing`).
- `--se`: `standard`, `robust`, `bootstrap` (default: `modules.sem.se`).
- `--ci`: `standard`, `bootstrap`, `bca` (default: `modules.sem.ci`).
- `--conf-level`: confidence level (default: `modules.sem.conf_level`).
- `--bootstrap`: enable bootstrap SEs/CIs (default: `modules.sem.bootstrap`).
- `--bootstrap-samples`: resamples (default: `modules.sem.bootstrap_samples`).
- `--ordered`: ordered categorical variables (required for WLSMV with ordinal data).

### Reporting

- `--std`: `none`, `std.lv`, `std.all` (default: `modules.sem.std`).
- `--fit`: fit indices list (default: `modules.sem.fit`).
- `--r2`: report R2 (default: `modules.sem.r2`).
- `--modindices`: modification index cutoff (default: `modules.sem.modindices`).
- `--residuals`: include standardized residuals in the JSONL log (default: `modules.sem.residuals`).
- `--digits`: rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for APA outputs (falls back to defaults).
- `--log`: JSONL logging (default: `defaults.log`).
- `--user-prompt`: original AI prompt for logging.

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- `report_canonical.md`: APA 7 report containing analysis type, table, and narrative text.
- `analysis_log.jsonl`: machine-readable results and options (appended per run when logging is enabled).

## APA 7 Template (YAML)

Templates live under `nlss/assets/sem/` and are selected via `templates.sem.*` in `nlss/scripts/config.yml`.

- Default SEM: `templates.sem.default` -> `sem/default-template.md`
- CFA: `templates.sem.cfa` -> `sem/cfa-template.md`
- Mediation: `templates.sem.mediation` -> `sem/mediation-template.md`
- Invariance: `templates.sem.invariance` -> `sem/invariance-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table column keys (SEM/CFA/mediation)

`group`, `path`, `label`, `est`, `se`, `z`, `p`, `ci_low`, `ci_high`, `std`

### Table column keys (invariance)

`step`, `group_equal`, `chisq`, `df`, `p`, `cfi`, `tli`, `rmsea`, `srmr`, `delta_cfi`, `delta_rmsea`

### Note tokens

`note_default`, `fit_summary`, `r2_summary`, `indirect_summary`, `estimator`, `missing`, `se`, `ci`

### Narrative tokens

`narrative_default`, `fit_summary`, `r2_summary`, `indirect_summary`

## APA 7 Reporting Guidance

- Report fit indices (chi-square, df, CFI, TLI, RMSEA, SRMR) and the estimator/missing handling used.
- For mediation, report indirect effects with bootstrap CIs and note the model type (parallel vs serial).
- For CFA, report standardized loadings and factor correlations if relevant.
- For invariance, report delta CFI/RMSEA and the step at which invariance holds.

## Notes

- SEM/CFA/mediation requires the `lavaan` package.
- Parquet inputs require the `arrow` package.
