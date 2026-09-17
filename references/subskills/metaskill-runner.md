---
name: metaskill-runner
description: Log metaskill activation/finalization (meta name, intent, phase, dataset, notes) as explicit utility evidence and in the root protocol; not required for ordinary report delivery.
license: Apache-2.0
---

# Metaskill Runner (Base R, NLSS format)

## Overview

Use only when an explicit lifecycle event is wanted, not as a compulsory report workflow.
For ordinary authored-report delivery use [project-report](../utilities/project-report.md).
This utility records activation/finalization through the common project publisher. This subskill does not perform analyses; it records the metaskill name, intent, dataset, and timestamp for traceability.

Metaskill specs themselves live under `references/metaskills/`.

Current-project selection uses the common resolver: `--project` wins over the
invocation directory, and `--dataset` selects registered working data instead of
the active default. Without either flag, use the nearest ancestor project and
its active dataset. Invalid explicit selections fail without fallback; a source
flag cannot be combined with `--dataset`. No old manifest fields or project
conversion are used. The utility releases its acquired input locks on exit,
including ordinary errors, but never removes a foreign lock.

Ordinary report delivery still uses `project-report` and does not need this utility.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, SAV, or workspace).
2. Provide the metaskill name (`--meta`), optional phase (`--phase`), and optional intent/notes.
3. For explicit finalization, write the substantive UTF-8 report `report_<YYYYMMDD>_<metaskill>_<intent>.md` at the current project root (outside `.nlss/`); standalone input mode retains its dataset output directory. A missing, empty, or invalid-UTF-8 report fails before dataset import. Local and UTC current-date filenames are accepted, with the local date preferred. This utility-specific convention is not required for ordinary report delivery.
4. Run the `metaskill-runner` operation through `run_nlss.R`.
5. Use outputs (root `report_canonical.md` and `.nlss/utility-runs/`) to confirm the activation/finalization log entry.

## Execution: `metaskill-runner`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

### Current project

```bash
Rscript "<skill>/scripts/R/run_nlss.R" metaskill-runner --project /path/to/study --dataset sample --meta sample-description
```

The same invocation works from outside the project or within another project.
Omit both selectors when running inside the intended project with its active data.

### CSV Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" metaskill-runner --csv <path to CSV file> --meta sample-description
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" metaskill-runner --parquet <path to parquet file> --meta sample-description --intent "describe the sample"
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" metaskill-runner --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--project` selects a current project directory/marker, otherwise nearest-ancestor discovery applies.
- `--dataset` selects a registered dataset, otherwise the active dataset is used when no source flag is supplied. Explicit data-file input retains its own meaning and uses the selected project's output route.
- `--meta` sets the metaskill name (required; default: `modules.metaskill_runner.meta_default`).
- `--phase` records the metaskill phase (for example `activation` or `finalization`; optional).
- `--intent` records a short intent summary (optional).
- `--notes` records free-form notes (optional).
- `--synopsis` includes a synopsis section in the finalization report (optional).
- `--label` overrides the analysis label in the NLSS format report (default: `modules.metaskill_runner.analysis_label`).
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--template` selects a template key or file path for the lifecycle log. An explicitly missing template is an error; omission uses the configured default. It does not constrain or rewrite the substantive metaskill report.
- `--log` controls optional standalone logging; current-project evidence/protocol is always published without a parallel JSONL journal.
- `--user-prompt` stores the original AI prompt in the saved request (optional).
- `--interactive` prompts for inputs.

### Parquet Support

Parquet input/output requires the R package `arrow` (install with `install.packages("arrow")`).

## Outputs

The common publisher appends the root protocol and saves utility evidence.
The filename convention above belongs only to this explicit lifecycle utility;
it is not required for ordinary authored reports saved through `project-report`.

- Current projects use `.nlss/utility-runs/` and the root protocol; selection is explicit or via the nearest ancestor marker.

- `report_canonical.md`: NLSS format report containing the activation/finalization log table and narrative.
- Explicit finalization may include a synopsis in the root protocol when requested; it is not a prerequisite for normal report delivery.
- `.nlss/utility-runs/metaskill_runner-<id>/`: mandatory lifecycle `request.json`, `result.json`, `output.md`, and the selected log template, even with `--log FALSE`. The event binds the exact verified dataset version and explicitly disclaims statistical replay.
- Finalization additionally preserves the original authored report bytes as `semantic-report.md`, with its SHA-256 in the result and artifact registry. The original file is not modified, including its front matter, line endings, or headings.

Root protocol publication uses the shared publication lock and recovery boundary. Failed publication retains diagnostic evidence without a successful `output.md`; preflight failure appends no success log. Dataset imports, when explicitly requested, retain the separate import-contract commit boundary.

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
- Final reports need semantic interpretation: integrate the research question, design, assumptions, effect estimates, uncertainty, and competing explanations. Neither a lifecycle template nor archived report bytes establish scientific validity. The logger preserves an authored interpretation; it does not regenerate or validate it.
