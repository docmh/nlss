---
title: Reconstruct-Reports Deliberate Tests
---

# Reconstruct-Reports Deliberate Test Plan

## Scope

- Generate a real `analysis_log.jsonl` with compressed `report_block` entries.
- Confirm `report_canonical_reconstructed.md` is rebuilt from the log.
- Confirm metaskill finalization logs emit a metaskill report file from the log.
- Confirm synopsis text passed to metaskill finalization appears in the reconstructed canonical report.
- Exercise broken entry handling (invalid JSON + invalid base64) with warnings.
- Confirm older logs without report blocks fail fast.

## How to Run

```bash
bash tests/run_reconstruct_reports_tests.sh
```

## Expected Coverage

- Reconstruction succeeds with valid report blocks and writes `report_canonical_reconstructed.md`.
- Metaskill finalization generates `report_<YYYYMMDD>_<metaskill>_<intent>.md` from the metaskill report block.
- Invalid JSON and invalid report blocks are skipped with warnings.
- Logs without report blocks exit non-zero with an "older logs are not supported" message.