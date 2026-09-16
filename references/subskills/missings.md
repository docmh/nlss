---
name: missings
description: Missingness summaries and pattern tables with auto or manual handling (listwise, impute, indicator, drop), threshold rules, and versioned working-data updates plus NLSS format outputs.
license: Apache-2.0
---

# Missings (Base R, NLSS format)

## Overview

Analyze missing-data patterns for selected variables, choose a handling strategy, and export a transformed dataset alongside NLSS format-ready tables and narratives.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose variables for missingness analysis.
3. Confirm that handling, not only inspection, is intended: this module can change
   working data. For inspection only, use `data-explorer` or descriptive summaries.
4. Run `scripts/R/missings.R` with the agreed variables/method and interpret the
   recorded decisions, retained cases and remaining missingness in context.

## Script: `scripts/R/missings.R`

Calculations use base R. Shared IO/run infrastructure also requires `arrow`,
`yaml`, `digest` and `jsonlite`; SAV import requires `haven`. All shared import
options and source-byte checks apply; see [import contract](../import-contract.md).

### CSV Input

```bash
Rscript <path to scripts/R/missings.R> --csv <path to CSV file> --vars age,score
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/missings.R> --rds <path to RDS file> --vars age,score
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/missings.R> --rdata <path to RData file> --df <data frame name> --vars age,score
```

### Parquet Input

```bash
Rscript <path to scripts/R/missings.R> --parquet <path to parquet file> --vars age,score
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/missings.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--vars` defaults to `modules.missings.vars_default` (typically all columns).
- Repeated variable names are handled once, preserving their first selected order.
- `--method` controls handling strategy (`auto`, `listwise`, `impute`, `indicator`, `drop`; default: `modules.missings.method`).
- `auto` selection uses the maximum missingness across selected variables to choose listwise (<= low), impute (<= moderate), indicator (<= high), or drop (otherwise).
- Automatic thresholds are heuristics, not a missing-data mechanism test or
  justification for inferential use. The defaults remain .05/.20/.40.
- Thresholds are proportions in `[0, 1]`:
  - `--low-threshold` (`modules.missings.low_threshold`)
  - `--moderate-threshold` (`modules.missings.moderate_threshold`)
  - `--high-threshold` (`modules.missings.high_threshold`)
  - `--drop-threshold` (`modules.missings.drop_threshold`)
  - `--indicator-threshold` (`modules.missings.indicator_threshold`)
- Thresholds must be finite and low <= moderate <= high. `drop` removes selected
  columns with missing proportion >= drop threshold, adds indicators for remaining
  selected columns >= indicator threshold, and fills the remaining selected
  missing values. `indicator` likewise fills missing values, not only adds flags.
- `--indicator-suffix` sets the suffix for missingness indicators (default: `modules.missings.indicator_suffix`).
- Existing columns are never overwritten by an indicator: collisions receive
  `_1`, `_2`, etc. The actual source-to-indicator mapping is recorded.
- `--skew-threshold` controls mean vs. median imputation for numeric variables (default: `modules.missings.skew_threshold`).
- `--max-patterns` caps the number of patterns shown (default: `modules.missings.max_patterns`).
- It is a positive integer. When truncated, an additional "Other patterns" row
  aggregates the remainder; percentages still use the full input N.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

### Deterministic handling and limits

Listwise deletion uses only selected variables to choose complete rows, retaining
all other columns and input order. The map records input-version row indices, not
participant identifiers. Empty inputs/zero remaining rows have unavailable
percentages where the denominator is zero and support no inference.

Numeric and temporal filling uses the mean unless absolute sample-SD-based
skewness `mean((x-mean(x))^3)/sd(x)^3` exceeds the chosen cutoff, then the median.
Computation standardizes before cubing and rescales when necessary, preventing
floating-point underflow/overflow from changing the method solely through units.
Fewer than three donors use the mean; constants have skewness zero. Temporal
classes, timezones and units are retained. Infinite observed donors cannot be
used for numeric filling and trigger an explicit failure; untouched infinities
are retained and reported, not silently classified as missing.

Factors, ordered factors, strings and logicals use the mode. Ties follow factor
level order, or the captured locale's table order for other categories. Numeric
SPSS value labels do not establish categorical measurement level: numeric codes
retain numeric behavior. Choose an explicit prior type transformation when the
research meaning requires categorical filling. All-missing columns have no
invented fill value; they remain missing unless the chosen drop rule removes them.
Dropping every column is refused. Selected matrix/list/custom-class columns
require an explicit supported representation rather than accidental coercion;
compatible unselected columns pass through unchanged.

## Outputs

Each call preserves a mandatory `.nlss/runs/<run-id>/` bundle and extends the
root protocol. Semantic research reports remain separate
metaskill outputs, not automatically justified by a completed numerical run.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- Visible working Parquet: updated through shared publication, preserving the
  input version for recovery without a permanent backup-copy family.
- `.nlss/runs/<run-id>/request.json`, `result.json`, `output.md`, frozen templates,
  `data-change.json` and `codebook.md`: effective settings, raw results, warnings,
  input/output references and authenticated handling evidence.
- `report_canonical.md`: NLSS format report containing analysis type, tables, and narrative text.
- `result.json`: Machine-readable results and options, always retained in the saved run.

`--log FALSE` suppresses only optional standalone logging, never the run bundle.
`results.handling_audit` records raw fill values, temporal storage units, missing
counts/positions before and after, and affected input rows. Rounded display fills
are not numerical evidence. `data_change.source_rows` addresses the immutable
input version; source missing codes/tags remain explicitly historical provenance,
not current row masks. Output dictionary/codebook track retained columns and
indicator labels. Exact no-ops create neither a new working copy nor a backup.

If a valid imputed value equals an original user-missing code with a display
label such as "No response", only that conflicting active label is removed.
The numeric fill and other valid labels remain. `label_conflicts` in the result
and output dictionary's `missing_handling` preserve the removed entry and reason;
the immutable input dictionary retains the original definitions. Reloading or
transforming the output must not resurrect the misleading active label.

Publication protects working data, current metadata, report/log and manifest
against ordinary controlled failures; see [run contract](../run-contract.md).
Replay recomputes from the preserved input and verifies its output version and
row map without replacing current data/metadata or creating a backup. Later
analyses therefore retain their current working state.

Undo is a separate, user-approved data change: identify the intended run/input
version, verify its preserved bytes and restore consistent metadata.
Do not choose a version only by filename recency, edit immutable versions, or
automatically reimport a source over later legitimate working changes. Interrupted
locks/pending runs require inspection; multi-file crash recovery is not guaranteed.

## NLSS format Templates

Use the Markdown template at `assets/missings/default-template.md` when assembling missingness reports. If the template exists, `missings.R` uses it for `report_canonical.md`.

### YAML Template Controls

- The template path can be overridden via `templates.missings.default` in `scripts/config.yml`.
- Quote column keys in YAML (for example `key: "n"`); the R YAML reader can
  interpret an unquoted `n` as a Boolean rather than a column name.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
  - `patterns_table.columns`: ordered column definitions for the pattern table.
  - `note.template`: overrides the note text (defaults to `{{note_default}}`).
  - `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Summary Table Column Keys

Available column keys for `table.columns` include:

`variable`, `type`, `total_n`, `missing_n`, `missing_pct`, `decision`, `impute_method`, `impute_value`, `indicator`.

### Pattern Table Column Keys

Available column keys for `patterns_table.columns` include:

`pattern`, `missing_vars`, `missing_count`, `n`, `pct_total`.

### Note Tokens

Available note tokens include:

`summary_note_body`, `patterns_note_body`.

### Other Tokens

Additional tokens available for custom templates include:

`summary_table_body`, `patterns_table_body`, `pattern_limit`.

### Narrative Tokens

Use `narrative.row_template` for per-variable lines. Available row tokens include:

`variable`, `missing_n`, `missing_pct`, `decision`, `impute_method`, `impute_value`, `indicator`, `full_sentence`.

## NLSS format Reporting Guidance

- Report the overall missingness range and complete-case percentage.
- Describe the selected handling method and any dropped variables or indicator columns.
- For each variable, report missingness and the imputation approach used (mean/median/mode) where applicable.
- Explain retained/excluded cases, all-missing variables, measurement level and
  study-specific consequences. Distinguish the deterministic decision from the
  researcher's rationale; combine substantive context, uncertainty and sensitivity
  considerations in a freely structured final report beyond the template.
- A low missingness percentage does not establish MCAR, MAR or MNAR. Simple
  filling and indicators do not propagate imputation uncertainty or guarantee
  unbiased estimates/valid standard errors. Consider the inferential model and
  missingness process, not just the cutoff. See van Buuren's
  [missingness mechanisms](https://stefvanbuuren.name/fimd/sec-MCAR.html) and
  [limitations of simple handling methods](https://stefvanbuuren.name/fimd/sec-simplesolutions.html).
  These cautions appear in saved results and a template-independent output block.
