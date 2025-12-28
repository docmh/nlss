---
name: check-integrity
description: Recover and summarize XOR-based NLSS checksums from analysis_log.jsonl entries to spot mismatches or tampering.
---

# Check-Integrity (Utility)

## Overview

Recover the NLSS checksum embedded in `analysis_log.jsonl` entries by reversing the XOR-based checksum scheme. This is the R port of `cmdscripts/check_log_checksum.py`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Intent/Triggers

Use this utility when you want to verify that log entries share the same recovered NLSS checksum (for example, to spot inconsistent or tampered entries).

Example prompts:
- "Check integrity for this analysis_log.jsonl."
- "Recover the NLSS checksum from the log entries."

## Inputs

- Path to an `analysis_log.jsonl` file.

## Script: `nlss/scripts/R/check_integrity.R`

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\check_integrity.R> <path to analysis_log.jsonl>
```

### WSL/Linux (Rscript directly)

```bash
Rscript <path to scripts/R/check_integrity.R> <path to analysis_log.jsonl>
```

## Options

- `-h`, `--help`: Print usage and exit.

## Behavior

- Reads the JSONL file in binary mode and preserves line endings (which affect checksum calculation).
- Ignores lines without a `checksum` field or invalid JSON.
- Reconstructs the NLSS checksum by XOR-reverting each entry checksum; when `checksum_version` is 2, it also XOR-reverts the checksum of the previous complete log line to preserve chain integrity.
- Prints each recovered checksum plus its count to stdout.
- Prints `No checksum entries found.` if no valid checksums are present.
- Emits a warning to stderr if multiple different recovered checksums are found.

## Outputs

- Stdout only (checksum counts).
- No `report_canonical.md` or `analysis_log.jsonl` output is created.

## Examples

```bash
Rscript nlss/scripts/R/check_integrity.R outputs/test-runs/20251228181418/workspace/golden_dataset/analysis_log.jsonl
```

## Non-goals

- Does not validate against the current NLSS source tree; it only reports recovered checksums.
- Not a subskill or metaskill; no dataset workspace required.

## Implementation Notes

- Requires the R package `jsonlite`.
- Compatible with `analysis_log.jsonl` entries written by NLSS when `defaults.log_nlss_checksum` is enabled.
