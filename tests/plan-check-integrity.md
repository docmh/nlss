---
title: Check-Integrity Deliberate Tests
---

# Check-Integrity Deliberate Test Plan

## Scope

- Generate a real `analysis_log.jsonl` with `log_seq` enabled.
- Confirm `analysis_log_seq` is stored in `nlss-workspace.yml`.
- Validate `check_integrity.R --diagnose TRUE` outputs diagnostics.
- Exercise tamper scenarios: edit, delete (gap), duplicate (out-of-order seq).
- Simulate a temporary codebase change and confirm checksum mismatch diagnostics.

## How to Run

```bash
bash tests/run_check_integrity_tests.sh
```

## Expected Coverage

- Clean log yields a single dominant checksum and no warning.
- Edit yields `edited_candidate` + `post_edit_chain`.
- Delete yields `post_delete_gap` with `missing_seq=`.
- Duplicate yields `seq_out_of_order`.
- Codebase change yields `status=mismatch seq=ok` diagnostics plus a warning.