---
name: check-integrity
description: Recover and summarize XOR-based NLSS checksums from analysis_log.jsonl entries to spot mismatches or tampering.
license: Apache-2.0
---

# Check-Integrity (Utility)

## Overview

Recover the NLSS checksum embedded in `analysis_log.jsonl` entries by reversing the XOR-based checksum scheme. This is the R port of `cmdscripts/check_log_checksum.py`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

## Intent/Triggers

Use this utility when you want to verify that log entries share the same recovered NLSS checksum (for example, to spot inconsistent or tampered entries).

Example prompts:

- "Check integrity for this analysis_log.jsonl."
- "Recover the NLSS checksum from the log entries."

## Inputs

- Path to an `analysis_log.jsonl` file (or set `NLSS_INTEGRITY_LOG` in the environment).

## Script: `scripts/R/check_integrity.R`

### Rscript

```bash
Rscript <path to scripts/R/check_integrity.R> <path to analysis_log.jsonl>
```

## Options

- `-h`, `--help`: Print usage and exit.
- `NLSS_INTEGRITY_LOG`: Optional environment fallback for the log path when the CLI argument is missing or invalid.
- `--diagnose TRUE|FALSE`: Emit per-line diagnostics (default: TRUE).

## Behavior

- Reads the JSONL file in binary mode and preserves line endings (which affect checksum calculation).
- Ignores lines without a `checksum` field or invalid JSON.
- Reconstructs the NLSS checksum by XOR-reverting each entry checksum; when `checksum_version` is 2, it also XOR-reverts the checksum of the previous complete log line to preserve chain integrity; when `checksum_version` is 3, it additionally XOR-reverts the checksum of `log_seq`.
- The NLSS checksum includes `SKILL.md`, `scripts/` (excluding `scripts/config.yml`), and `references/`, so asset/template edits do not change the recovered checksum.
- `log_seq` is stored in each entry and tracked in `nlss-workspace.yml` as `analysis_log_seq`; if `analysis_log.jsonl` is missing, the sequence restarts at 1.
- Prints each recovered checksum plus its count to stdout.
- Prints `No checksum entries found.` if no valid checksums are present.
- Emits a warning to stderr if multiple different recovered checksums are found.
- When `--diagnose` is enabled, prints `DIAG` lines to stderr with `line`, `log_seq`, `status`, `seq`, `inferred`, and `checksum` fields to help distinguish edits (two-line mismatch) from deletions (seq gaps).
- `status=mismatch` with `seq=ok` often indicates a codebase checksum change during the log, not a deletion.

## Outputs

- Stdout only (checksum counts).
- No `report_canonical.md` or `analysis_log.jsonl` output is created.

## Examples

```bash
Rscript scripts/R/check_integrity.R outputs/test-runs/20251228181418/workspace/golden_dataset/analysis_log.jsonl
```

## Non-Goals

- Does not validate against the current NLSS source tree; it only reports recovered checksums.
- Not a subskill or metaskill; no dataset workspace required.

## Implementation Notes

- Requires the R package `jsonlite`.
- Compatible with `analysis_log.jsonl` entries written by NLSS when `defaults.log_nlss_checksum` is enabled.
