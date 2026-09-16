---
name: impute
description: Generate single-completion columns with simple methods or mice/VIM kNN; preserve original data and mids artifacts, verified before/after versions, seeded replay and explicit inference limitations.
license: Apache-2.0
---

# Impute (Base R With Optional Mice/VIM, NLSS format)

## Overview

Impute missing values into new columns (suffix `_imp`) while preserving the original variables. Supports base R methods (mean/median/mode/random/constant) and optional `mice` or `VIM::kNN` engines when installed. Indicator columns are created only when `--indicator TRUE` is passed.

**Current inference boundary:** this command generates single-completion columns. With `mice`, numeric values are averaged across the generated imputations and categorical values use their mode. This preserves the existing completion behavior, but it is **not Rubin-pooled multiple-imputation inference**. Analyses of those single columns do not account for between-imputation uncertainty. The unchanged `mids` object is also preserved. Use the separate [mi-regression](mi-regression.md) adapter for its explicitly supported model-per-imputation inference; do not infer pooling support for other modules.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables and an imputation engine (`simple`, `mice`, `knn`, or `auto`).
3. Run `scripts/R/impute.R` with the correct flags.
4. Use outputs (workspace parquet, `report_canonical.md`, `result.json`) in your response.

## Script: `scripts/R/impute.R`

Run with `Rscript` in base R. Optional engines require R packages `mice` or `VIM` when selected. Parquet I/O requires the `arrow` package.

### CSV Input

```bash
Rscript <path to scripts/R/impute.R> --csv <path to CSV file> --vars age,gender
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/impute.R> --rds <path to RDS file> --vars age,gender
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/impute.R> --rdata <path to RData file> --df <data frame name> --vars age,gender
```

### Parquet Input

```bash
Rscript <path to scripts/R/impute.R> --parquet <path to parquet file> --vars age,gender
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/impute.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--vars` defaults to `modules.impute.vars_default` (typically `all`).
- `--engine` defaults to `modules.impute.engine` (`auto` chooses `mice` if installed, else `VIM`, else `simple`).
- `--numeric-method` defaults to `modules.impute.numeric_method` (for `simple` engine).
- `--categorical-method` defaults to `modules.impute.categorical_method` (for `simple` engine).
- `--method-map` overrides per-variable methods (e.g., `age=median|income=mean`).
- `--value-map` overrides per-variable constants (e.g., `income=0|status=missing`).
- `--constant` supplies a global constant for `constant` method when `value-map` is not set.
- `--suffix` sets the imputed column suffix (default: `modules.impute.suffix`); original variables are never overwritten.
- `--indicator` toggles missingness indicators (default: `modules.impute.indicator`).
- `--indicator-suffix` sets indicator suffix (default: `modules.impute.indicator_suffix`).
- `--skew-threshold` controls mean vs. median when `numeric-method` is `auto` (default: `modules.impute.skew_threshold`).
- `--m` and `--maxit` control generation of `mice` imputations (default: `modules.impute.m` and `modules.impute.maxit`); when using `mice`, `m` is coerced to at least 2. They do not request or enable pooled model inference.
- `--k` controls `VIM::kNN` neighbors (default: `modules.impute.k`).
- `--seed` sets the random seed; otherwise `modules.impute.seed` (default 1)
  supplies a reproducible start. Requested and effective engine, seed and RNG
  state are recorded; replay uses the resolved engine, not a new `auto` choice.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Outputs

Imputation appends to `report_canonical.md` and also preserves a mandatory
run bundle under `.nlss/runs/<run-id>/` as specified by the
[run contract](../run-contract.md). Its deterministic `output.md` is not a
semantic final research report. Authored reports use freely chosen visible Markdown paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- Visible working Parquet: updated through shared publication using the frozen
  before-version for recovery, without a permanent backup family. Working data,
  current metadata and root protocol share protection against ordinary
  publication errors; originals and existing derived columns are not overwritten.
- `.nlss/runs/<run-id>/request.json`, `result.json`, `output.md`, `data-change.json`
  and `codebook.md`: resolved input/options, actual results and authenticated
  before/after lineage, retained even when `--log FALSE` disables optional standalone logging.
- `.nlss/imputations/mice-<sha256>/mids.rds`: Exact, round-trip-verified `mids` object returned by `mice`, including all `m` imputations, original engine data, methods, predictor matrix, and available diagnostics. The content-addressed directory is published only after successful writing and verification; an existing artifact is never overwritten. If every selected variable is entirely missing and no mice run can be made, no `mids` artifact is claimed.
- Before calling mice, its input frame receives a deterministic
  `nlss_input_version_id` attribute. This binds generation to the actual input
  version without changing any statistical column or modifying the returned
  object. Repeating the same seed after adding completion columns can therefore
  retain a distinct, correctly bound artifact instead of colliding with an old
  artifact whose selected input values happen to be identical.
- `.nlss/imputations/mice-<sha256>/metadata.json`: Artifact schema, relative path, SHA-256, number of imputations, iteration count, R/mice versions, seed when supplied, and available input dataset reference. The data in `mids.rds` are research data and require the same access protections as the source dataset.
- `report_canonical.md`: NLSS format report containing analysis type, table, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run. Results explicitly contain `completion_mode: "single_completion"`, `inference_pooled: false`, and `completion_aggregation` (`"mean_numeric_mode_categorical"` for mice; `"none"` otherwise). The mice result also records `imputation_artifact` and `inference_warning`.

Mice runs always emit the inference warning to the console and append a mandatory Markdown notice after template rendering. Custom templates cannot hide that notice; it remains in saved output and the root protocol. Preserving the `mids` artifact does not depend on optional standalone logging.

Recovery: identify the intended before-version in the completed run and verify
its preserved bytes/hash before proposing restoration of the data and matching metadata.
Obtain user approval; never blindly select the newest filename.

## Data, replay and interpretation safeguards

- Original columns, row order, labels and historical missing definitions are
  preserved. New completion/indicator columns are derived variables; their
  provenance addresses rows in the immediate input version. Source missing
  codes and tags are not masks to reapply to newly filled values.
- Numeric codes with labels remain numeric unless the researcher explicitly
  supplies categorical data. Labels do not establish a measurement level;
  justify arithmetic or categorical treatment in context.
- Counts describe values actually filled and still missing, not merely attempted
  replacements. Explicit constants can fill an all-missing column; methods
  requiring observed donors cannot. Unsupported selected classes and invalid
  option domains fail explicitly rather than silently changing methods.
  Repeated keys within a method/value map are refused as ambiguous; repeated
  variable selections are deduplicated in first-occurrence order.
- Random simple imputation samples observed donor positions, including when
  only one numeric donor exists. Mode ties use first occurrence. Automatic
  mean/median choice is a skewness heuristic, not a missing-mechanism test.
- Replay verifies original data/dictionary, templates, configuration, code and
  environment, recomputes the output, and never activates it or creates a new
  working-data backup. It verifies and reuses the preserved `mids` artifact;
  its exact bytes remain immutable. Scientific payload comparison excludes
  only top-level runtime `call` and `date`, not draws, methods, predictor matrix,
  diagnostics or RNG state. This is not a promise of identical newly serialized
  RDS bytes across sessions.
- An authenticated `imputation-artifact.json` binds the run to both mids and
  adjacent metadata hashes. A fully verified but unreferenced immutable artifact
  can remain after a later publication failure, as can an output snapshot or
  preserved recovery input. Only a completed published run establishes successful data
  application. This is not crash-atomic recovery or protection against external
  writers that ignore locks.
- Explain missing-data assumptions, variable roles, predictors, diagnostics and
  uncertainty in a contextual final report. Templates and engine completion
  cannot certify MAR/MCAR/MNAR, model adequacy or convergence. Simple and kNN
  completion likewise do not propagate imputation uncertainty into later tests.

## NLSS format Templates

Use the Markdown template at `assets/impute/default-template.md` when assembling imputation reports. If the template exists, `impute.R` uses it for `report_canonical.md`.

### YAML Template Controls

- The template path can be overridden via `templates.impute.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `note.template`: overrides the note text (defaults to `{{note_default}}` if omitted).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`variable`, `type`, `missing_n`, `missing_pct`, `engine`, `method`, `impute_value`, `imputed_n`, `target`, `indicator`, `note`.

Use `drop_if_empty: true` to remove a column if all values are blank (for example, `indicator` or `note`).

### Note Tokens

Available note tokens include:

`note_default`, `engine_note`, `indicator_note`, `completion_note`, `inference_warning`, `seed_note`, `skipped_note`, `map_note`. The legacy `pool_note` token remains as a compatibility alias with explicit single-completion wording; its name must not be interpreted as evidence of pooled inference.

### Narrative Tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`variable`, `type`, `missing_n`, `missing_pct`, `engine`, `method`, `impute_value`, `imputed_n`, `target`, `indicator`, `note`, `full_sentence`.

## NLSS format Reporting Guidance

- Report missingness range and the imputation engine used (including `m`/`maxit` for `mice` or `k` for `kNN`).
- State that imputed values were written to new `_imp` columns and originals were preserved.
- When indicators are requested, note the indicator suffix and variables that received indicators.
- For mice, state that the columns are single completions, identify the preserved `mids` artifact and its hash, and explain the missing propagation of between-imputation uncertainty. Do not describe later tests on `_imp` columns as Rubin-pooled multiple-imputation inference, even when `m > 1` or an artifact exists.

## Separate Phase 2 Model-per-Imputation Inference

The separate `mi_regression.R` adapter validates the preserved artifact/hash and its input dataset reference, applies the same resolved model to **each** completed dataset, and pools model estimates **and their uncertainty** through `mice::pool`. It preserves per-imputation fits and diagnostics and explicitly rejects failed or unsupported fits. Read [its supported model contract](../mi-pooling.md) before using it. Averaging completed data values is never a substitute for these steps.

Only the independently checked, successful supported inference output declares `inference_pooled: true`; the current `impute` command still makes no such claim. No model families or statistical engines are removed by this boundary. Unsupported pooled inference remains explicit rather than silently falling back to tests on averaged columns. The original `mids` object can also be inspected with established mice functions, subject to the normal NLSS permission boundary for bespoke analysis scripts.
