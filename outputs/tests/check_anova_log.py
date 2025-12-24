#!/usr/bin/env python3
import json
import sys
from pathlib import Path

def parse_arg(value):
    if value in ("", "-", "none", "None"):
        return None
    return value

if len(sys.argv) < 9:
    sys.stderr.write("Usage: check_anova_log.py LOG START MODE POSTHOC POSTHOC_ROWS EFFECT_SIZE SPHERICITY STATUS [BOOT_CI]\n")
    sys.exit(2)

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
expect_mode = parse_arg(sys.argv[3])
expect_posthoc = parse_arg(sys.argv[4])
expect_posthoc_rows = parse_arg(sys.argv[5])
expect_effect_size = parse_arg(sys.argv[6])
expect_sphericity = parse_arg(sys.argv[7])
expect_status = parse_arg(sys.argv[8])
expect_boot_ci = parse_arg(sys.argv[9]) if len(sys.argv) > 9 else None

if not log_path.exists():
    sys.exit(1)

index = 0
found = False
with log_path.open("r", encoding="utf-8") as handle:
    for line in handle:
        if not line.strip():
            continue
        index += 1
        if index <= start_count:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if entry.get("module") != "anova":
            continue
        results = entry.get("results", {})
        options = entry.get("options", {})

        if expect_status is not None:
            if results.get("status") != expect_status:
                continue
            found = True
            break

        summary_df = results.get("summary_df")
        if not isinstance(summary_df, list) or len(summary_df) == 0:
            continue

        if expect_mode is not None and options.get("mode") != expect_mode:
            continue
        if expect_posthoc is not None and options.get("posthoc") != expect_posthoc:
            continue
        if expect_effect_size is not None and options.get("effect_size") != expect_effect_size:
            continue

        if expect_posthoc_rows is not None:
            posthoc_df = results.get("posthoc_df")
            posthoc_len = len(posthoc_df) if isinstance(posthoc_df, list) else 0
            if expect_posthoc_rows == "gt0" and posthoc_len <= 0:
                continue
            if expect_posthoc_rows.isdigit() and posthoc_len != int(expect_posthoc_rows):
                continue

        if expect_sphericity is not None:
            assumptions_df = results.get("assumptions_df")
            if not isinstance(assumptions_df, list):
                continue
            has_sphericity = any(row.get("assumption") == "Sphericity" for row in assumptions_df if isinstance(row, dict))
            if expect_sphericity == "present" and not has_sphericity:
                continue
            if expect_sphericity == "absent" and has_sphericity:
                continue

        if expect_boot_ci is not None:
            has_boot_ci = any(
                row.get("boot_ci_low") is not None and row.get("boot_ci_high") is not None
                for row in summary_df if isinstance(row, dict)
            )
            if expect_boot_ci == "present" and not has_boot_ci:
                continue
            if expect_boot_ci == "absent" and has_boot_ci:
                continue

        found = True
        break

sys.exit(0 if found else 1)
