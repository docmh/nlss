#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
import csv
import json
import math
import sys
from pathlib import Path


def fail(message: str) -> None:
    print(message)
    sys.exit(1)


def parse_float(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def parse_bool(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    val = str(value).strip().lower()
    if val in ("1", "true", "t", "yes", "y"):
        return True
    if val in ("0", "false", "f", "no", "n"):
        return False
    return None


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def normalize_controls(value):
    if value is None:
        return ""
    if isinstance(value, list):
        return ",".join(str(item).strip() for item in value if str(item).strip())
    return str(value).strip()


def load_golden(path: Path):
    if not path.exists():
        fail(f"Missing golden file: {path}")
    rows = {}
    with path.open("r", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            case_id = normalize_text(row.get("case_id"))
            if case_id:
                rows[case_id] = row
    return rows


def load_entries(path: Path, start_count: int):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def find_entry(entries):
    for entry in reversed(entries):
        if entry.get("module") == "correlations":
            return entry
    return None


def find_row(summary_df, expected):
    exp_var1 = normalize_text(expected.get("var1"))
    exp_var2 = normalize_text(expected.get("var2"))
    exp_group = normalize_text(expected.get("group"))
    exp_method = normalize_text(expected.get("method"))
    exp_alt = normalize_text(expected.get("alternative"))
    exp_controls = normalize_controls(expected.get("controls"))
    exp_missing = normalize_text(expected.get("missing_method"))
    exp_p_adjust = normalize_text(expected.get("p_adjust_method"))
    exp_partial = parse_bool(expected.get("partial"))

    for row in summary_df:
        if normalize_text(row.get("var1")) != exp_var1:
            continue
        if normalize_text(row.get("var2")) != exp_var2:
            continue
        if normalize_text(row.get("group")) != exp_group:
            continue
        if exp_method and normalize_text(row.get("method")) != exp_method:
            continue
        if exp_alt and normalize_text(row.get("alternative")) != exp_alt:
            continue
        if exp_controls != normalize_controls(row.get("controls")):
            continue
        if exp_missing and normalize_text(row.get("missing_method")) != exp_missing:
            continue
        if exp_p_adjust and normalize_text(row.get("p_adjust_method")) != exp_p_adjust:
            continue
        if exp_partial is not None:
            if parse_bool(row.get("partial")) != exp_partial:
                continue
        return row
    return None


def compare_numeric(actual, expected, label, rel_tol=1e-6, abs_tol=1e-6):
    if expected is None:
        return
    if actual is None:
        fail(f"Missing actual value for {label}")
    try:
        actual_val = float(actual)
    except (TypeError, ValueError):
        fail(f"Non-numeric actual value for {label}: {actual}")
    if not math.isclose(actual_val, expected, rel_tol=rel_tol, abs_tol=abs_tol):
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_correlations_golden.py <log_path> <start_count> <golden_csv> <case_id>")

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    golden_path = Path(sys.argv[3])
    case_id = sys.argv[4]

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    golden_rows = load_golden(golden_path)
    expected = golden_rows.get(case_id)
    if expected is None:
        fail(f"Golden values missing for case_id: {case_id}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = find_entry(entries)
    if entry is None:
        fail("No correlations log entry found.")

    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    row = find_row(summary_df, expected)
    if row is None:
        fail("Expected summary row not found")

    numeric_keys = [
        "n",
        "total_n",
        "missing_n",
        "missing_pct",
        "r",
        "p_value",
        "ci_low",
        "ci_high",
        "boot_ci_low",
        "boot_ci_high",
        "p_adjusted",
        "r0",
        "z_r0",
        "p_r0",
        "conf_level",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
