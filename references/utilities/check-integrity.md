---
name: check-integrity
description: Inspect recovered MD5/XOR checksums in existing analysis_log.jsonl entries for inconsistencies without modifying evidence; not signature verification or statistical replay.
license: Apache-2.0
---

# Check-Integrity (Utility)

## Overview

This checks explicitly selected standalone checksum logs. Current marked
projects do not create a parallel JSONL log or record its sequence in the root
marker. Use [project-inspect](project-inspect.md) for current project navigation;
this utility does not certify current run/report evidence or statistical validity.

Recover the NLSS checksum embedded in `analysis_log.jsonl` entries by reversing the XOR-based checksum scheme. This is the R port of `cmdscripts/check_log_checksum.py`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Intent/Triggers

Use this utility when you want to verify that log entries share the same recovered NLSS checksum (for example, to spot inconsistent or tampered entries).

Example prompts:

- "Check integrity for this analysis_log.jsonl."
- "Recover the NLSS checksum from the log entries."

## Inputs

- Path to an `analysis_log.jsonl` file (or set `NLSS_INTEGRITY_LOG` in the environment).

## Execution: `check-integrity`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

### Rscript

```bash
Rscript "<skill>/scripts/R/run_nlss.R" check-integrity <path to analysis_log.jsonl>
```

## Options

- `-h`, `--help`: Print usage and exit.
- `NLSS_INTEGRITY_LOG`: Optional log path when no positional path was supplied. An explicit missing path is an error, never permission to inspect a different file.
- `--diagnose TRUE|FALSE`: Emit per-line diagnostics (default: TRUE).

## Behavior

- Reads the JSONL file in binary mode and preserves line endings (which affect checksum calculation).
- Ignores lines without a `checksum` field or invalid JSON.
- Reconstructs the NLSS checksum by XOR-reverting each entry checksum; when `checksum_version` is 2, it also XOR-reverts the checksum of the previous complete log line to preserve chain integrity; when `checksum_version` is 3, it additionally XOR-reverts the checksum of `log_seq`.
- The NLSS checksum includes `SKILL.md`, `scripts/` (excluding `scripts/config.yml`), and `references/`, so asset/template edits do not change the recovered checksum.
- Standalone entries may contain `log_seq`; current project markers do not track a parallel `analysis_log_seq` journal.
- Prints each recovered checksum plus its count to stdout.
- Prints `No checksum entries found.` if no valid checksums are present.
- Emits a warning to stderr if multiple different recovered checksums are found.
- When `--diagnose` is enabled, prints `DIAG` lines to stderr with `line`, `log_seq`, `status`, `seq`, `inferred`, and `checksum` fields to help distinguish edits (two-line mismatch) from deletions (seq gaps).
- `status=mismatch` with `seq=ok` often indicates a codebase checksum change during the log, not a deletion.
- Uses the mandatory shared bootstrap and Boolean validation, with a small
  positional compatibility adapter shared with reconstruction. Unknown or
  duplicate flags, malformed Booleans and extra paths are errors. `--diag`,
  `--no-diagnose` and `--no-diagnostic` remain supported; help exits successfully.
- Malformed JSON shapes and unsupported checksum fields do not become trusted
  checksums. Original bytes and line endings are never rewritten.

## Outputs

- Stdout only (checksum counts).
- No `report_canonical.md` or `analysis_log.jsonl` output is created.

## Examples

```bash
Rscript "<skill>/scripts/R/run_nlss.R" check-integrity outputs/test-runs/20251228181418/workspace/golden_dataset/analysis_log.jsonl
```

## Non-Goals

- Does not validate against the current NLSS source tree; it only reports recovered checksums.
- A recovered MD5/XOR value is neither a cryptographic signature nor the SHA-256
  input/environment verification performed by statistical `replay-run`.
- Not a subskill or metaskill; no dataset workspace required.

## Implementation Notes

- Requires the R package `jsonlite`.
- Compatible with `analysis_log.jsonl` entries written by NLSS when `defaults.log_nlss_checksum` is enabled.
