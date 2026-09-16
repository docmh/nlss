---
name: reconstruct-reports
description: Rebuild report_canonical_reconstructed.md and metaskill reports from analysis_log.jsonl report_block entries (gzip+base64).
license: Apache-2.0
---

# Reconstruct-Reports (Utility)

## Overview

This is a standalone JSONL decoder, not the current project's evidence browser
or automatic root-protocol publisher. Current marked projects do not emit that
JSONL file. Use [project-inspect](project-inspect.md) to find their saved outputs
and [project-report](project-report.md) to browse preserved authored revisions.
Do not create a parallel log merely to feed this utility.

Rebuild `report_canonical_reconstructed.md` by decoding compressed `report_block` entries stored in `analysis_log.jsonl`. Metaskill finalization entries produce separate `_reconstructed.md` copies; the authored reports remain untouched. Decoding is not statistical replay or a new semantic synthesis.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

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
- `--overwrite TRUE|FALSE`: Explicitly permit replacing conflicting reconstructed files (default FALSE). Byte-identical existing outputs are already safe to reuse.
- `NLSS_RECONSTRUCT_LOG`: Optional log path when no positional path was supplied. An explicit invalid path is an error.

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
- Validates and decodes all blocks before opening output files. An empty or
  unusable log cannot truncate a previous reconstruction. Unknown/duplicate
  flags, missing option values and malformed Booleans fail explicitly.
- Preserves decoded UTF-8 text bytes, including line endings; does not insert
  new frontmatter or current timestamps. An absent historical date is named
  `undated`, not inferred from today.
- Repeated finalizations with the same date/name receive `_line<N>` suffixes
  before `_reconstructed.md`; later revisions do not erase earlier text.
- Rejects output-file symlinks, directories and source-log/output collisions.
  A short output-local lock and protected copies restore existing files after
  ordinary publication errors. This is not crash-atomic recovery or protection
  against uncooperative external writers.

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
