---
name: calc
description: Lightweight calculator CLI for evaluating numeric expressions to derive subskill parameters without dataset context.
---

# Calc (Utility)

## Overview

Evaluate simple numeric expressions from the CLI to derive parameters for subskill flags (for example, alpha adjustments or effect size conversions) without loading a dataset or writing workspace outputs.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions, select the appropriate analysis, document decisions, and produce a detailed, APA 7-aligned, journal-ready report.

## Intent/Triggers

Use this utility when the request is a quick numeric computation rather than a statistical analysis.

Example prompts:
- "Compute f2 from R2 = 0.12."
- "Adjust alpha for 3 tests."
- "Convert r = 0.30 to d."

## Inputs

- One or more numeric expressions to evaluate.
- Optional named constants to reuse across expressions.

## Script: `nlss/scripts/R/calc.R`

Run with `Rscript` and base R only. This utility is standalone and does not read datasets or write APA reports.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\calc.R> --expr "0.05/3"
```

### WSL/Linux (Rscript directly)

```bash
Rscript <path to scripts/R/calc.R> --expr "0.05/3"
```

## Options

- `--expr <text>`: Required. One or more expressions separated by `|` (for example `--expr "0.05/3|sqrt(0.2)"`).
- `--set <text>`: Optional named constants, `name=value` pairs separated by `|` (for example `--set "r=0.3|k=3"`).
- `--digits <n>`: Rounding for printed results (default: `defaults.digits` from `nlss/scripts/config.yml`).
- `--format <plain|json|csv>`: Output format (default: `plain`).
- `--template <ref|path>`: Template path or key for `report_canonical.md` (optional).
- `--user-prompt <text>`: Original AI user prompt for logging (optional).
- `--log TRUE/FALSE`: Write `analysis_log.jsonl` (default: `defaults.log`).
- `--unsafe TRUE/FALSE`: Allow full R evaluation in the global environment (default: `FALSE`).
- `--interactive`: Prompt for inputs instead of flags.

## Behavior

- Evaluates expressions in a restricted environment by default (no file/system access).
- Supports basic numeric operators and a curated set of math/stat functions, such as `abs`, `sqrt`, `log`, `exp`, `round`, `min`, `max`, `sum`, `mean`, `sd`, `var`, `pnorm`, `qnorm`, `pt`, `qt`, `pf`, `qf`, and constants `pi`, `e`.
- `--set` variables are available to all expressions; `name=expr` inside `--expr` is also supported.
- If any expression fails to parse or evaluate, exit with non-zero status and write the error to stderr.

## Outputs

- Prints results to stdout and appends `report_canonical.md` plus `analysis_log.jsonl` in the resolved output directory (workspace root if a manifest is present; otherwise `defaults.output_dir`).
- `plain` format prints one `name = value` per line; unnamed expressions are labeled `expr_1`, `expr_2`, etc.
- `json` format prints a JSON object of name/value pairs.
- `csv` format prints `name,value` rows with a header.

## Examples

```bash
# Adjust alpha for 3 tests
Rscript <path to scripts/R/calc.R> --expr "0.05/3"

# Convert r to d and compute r2
Rscript <path to scripts/R/calc.R> --set "r=0.3" --expr "d=2*r/sqrt(1-r^2)|r2=r^2"

# Plain output without assignments
Rscript <path to scripts/R/calc.R> --expr "sqrt(0.2)|log(10)"
```

## Template (YAML)

The default template lives at `nlss/assets/calc/default-template.md` and can be overridden via `templates.calc.default` in `nlss/scripts/config.yml` or the `--template` flag.

### Table column keys

Available column keys for `table.columns` include:

`name`, `expression`, `value`.

### Narrative row tokens

Available row tokens include:

`name`, `expression`, `value`, `full_sentence`.

### Additional tokens

`expression_count` provides the number of evaluated expressions.

## Non-goals

- No dataset loading or workspace parquet handling.
- Not a subskill or metaskill; use only as a convenience utility.

## Implementation Notes

- Source `nlss/scripts/R/lib/config.R` to pull `defaults.digits` and logging defaults.
- Generate `report_canonical.md` using `nlss/assets/calc/default-template.md` unless overridden.
