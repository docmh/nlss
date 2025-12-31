---
name: metaskill-runner
description: Log metaskill activation/finalization (meta name, intent, phase, dataset, notes) to report_canonical.md and analysis_log.jsonl; no analysis is performed.
license: Apache-2.0
---

# Metaskill Runner (Base R, NLSS format)

## Overview

Logs the activation or finalization of a metaskill (the agent-run pseudoscript) to the dataset workspace. This subskill does not perform analyses; it records the metaskill name, intent, dataset, and timestamp for traceability.

Metaskill specs themselves live under `references/metaskills/`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, SAV, or workspace).
2. Provide the metaskill name (`--meta`), optional phase (`--phase`), and optional intent/notes.
3. Ensure the metaskill report `report_<YYYYMMDD>_<metaskill>_<intent>.md` exists before finalization; the runner will fail if it is missing.
4. Run `scripts/R/metaskill_runner.R`.
5. Use outputs (`report_canonical.md`, `analysis_log.jsonl`) to confirm the activation/finalization log entry.

## Script: `scripts/R/metaskill_runner.R`

### CSV Input

```bash
Rscript <path to scripts/R/metaskill_runner.R> --csv <path to CSV file> --meta sample-description
```

### Parquet Input

```bash
Rscript <path to scripts/R/metaskill_runner.R> --parquet <path to parquet file> --meta sample-description --intent "describe the sample"
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/metaskill_runner.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--meta` sets the metaskill name (required; default: `modules.metaskill_runner.meta_default`).
- `--phase` records the metaskill phase (for example `activation` or `finalization`; optional).
- `--intent` records a short intent summary (optional).
- `--notes` records free-form notes (optional).
- `--synopsis` includes a synopsis section in the finalization report (optional).
- `--label` overrides the analysis label in the NLSS format report (default: `modules.metaskill_runner.analysis_label`).
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).
- `--interactive` prompts for inputs.

### Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills and must exist before logging finalization.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).

- `report_canonical.md`: NLSS format report containing the activation/finalization log table and narrative.
- `analysis_log.jsonl`: Machine-readable activation/finalization log entry (appended per run when logging is enabled).
- Finalization logs include the metaskill report as `metaskill_report_block_b64` for reconstruction and can embed a synopsis in the canonical report when `--synopsis` is provided.

## NLSS format Templates

Use the Markdown template in `assets/metaskill-runner` when assembling the activation/finalization report.

- Default template: `assets/metaskill-runner/default-template.md` (activation).
- Finalization template: `assets/metaskill-runner/finalization-template.md`.
- Template paths can be overridden via `templates.metaskill_runner.default` and `templates.metaskill_runner.finalization` in `scripts/config.yml`.

### YAML Template Controls

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides narrative text.

### Table Column Keys

Available column keys for `table.columns` include:

`item`, `value`

### Tokens

Available template tokens include:

`metaskill_name`, `intent`, `dataset`, `timestamp`, `notes`, `synopsis_text`, `analysis_label`, `analysis_flags`, `table_number`, `table_body`, `note_body`, `note_default`, `narrative`, `narrative_default`.

## NLSS format Reporting Guidance

- Treat this entry as a traceability log; analyses are reported by the subskills that follow.
- If the metaskill triggers multiple analyses, keep their statistical details in those subskill outputs.