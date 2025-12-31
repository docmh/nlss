---
name: reconstruct-reports
description: Rebuild report_canonical_reconstructed.md and metaskill reports from analysis_log.jsonl report_block entries (gzip+base64).
license: Apache-2.0
---

# Reconstruct-Reports (Utility)

## Overview

Rebuild `report_canonical_reconstructed.md` by replaying compressed `report_block` entries stored in `analysis_log.jsonl`. When metaskill finalization entries include a metaskill report block, this utility emits `report_<YYYYMMDD>_<metaskill>_<intent>.md` at the point of finalization.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this utility when you want to reconstruct canonical reports or metaskill reports from an existing `analysis_log.jsonl` without loading the dataset or workspace.

Example prompts:

- "Rebuild the canonical report from analysis_log.jsonl."
- "Reconstruct reports from this log file."
- "Generate metaskill reports from the analysis log."

## Inputs

- Path to an `analysis_log.jsonl` file (any filename accepted), or set `NLSS_RECONSTRUCT_LOG`.

## Script: `scripts/R/reconstruct_reports.R`

### Rscript

```bash
Rscript <path to scripts/R/reconstruct_reports.R> <path to analysis_log.jsonl>
```

## Options

- `-h`, `--help`: Print usage and exit.
- `--out-dir <path>`: Output directory for reconstructed reports (default: directory of the log file).
- `NLSS_RECONSTRUCT_LOG`: Optional environment fallback for the log path when the CLI argument is missing or invalid.

## Behavior

- Expects `report_block_b64` with `report_block_encoding = "gzip+base64"` in each log entry.
- If no report blocks are found, the utility exits with a non-zero status (older logs are not supported).
- Invalid JSON lines, missing blocks, or decode failures are skipped with warnings.
- Metaskill finalization entries (`module = metaskill_runner`, `phase = finalization`) use:
  - `metaskill_report_block_b64` when available (new logs).
  - `report_block_b64` when `results.report_block_source = "metaskill_report"` (legacy logs).
  - `report_block_full_b64` as a fallback when no explicit metaskill block is present.
- When `results.report_block_source = "metaskill_report"`, the canonical report block for that entry is read from `report_block_full_b64`.
- Operates outside the workspace model; no manifest or dataset file is required.

## Outputs

- `report_canonical_reconstructed.md` in the output directory.
- `report_<YYYYMMDD>_<metaskill>_<intent>_reconstructed.md` for each metaskill finalization entry when a metaskill report block is available.

## Examples

```bash
Rscript scripts/R/reconstruct_reports.R outputs/workspace/example/analysis_log.jsonl
Rscript scripts/R/reconstruct_reports.R --out-dir outputs/rebuild outputs/workspace/example/analysis_log.jsonl
```

## Non-Goals

- Does not validate report blocks against datasets or rerun analyses.
- Does not read or update workspace manifests.

## Implementation Notes

- Requires the R package `jsonlite` for base64 decoding.
- Report blocks are stored as gzip-compressed base64 strings (`report_block_b64`).