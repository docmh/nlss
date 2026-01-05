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


def normalize_text(value) -> str:
    if value in (None, "", "NA", "NaN", "None"):
        return ""
    return str(value).strip()


def parse_float(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


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
        if entry.get("module") == "reliability":
            return entry
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


def find_row(summary_df, expected):
    exp_analysis = normalize_text(expected.get("analysis"))
    exp_group = normalize_text(expected.get("group"))
    exp_model = normalize_text(expected.get("model"))
    exp_type = normalize_text(expected.get("type"))
    exp_unit = normalize_text(expected.get("unit"))
    exp_weight = normalize_text(expected.get("weight"))
    exp_method = normalize_text(expected.get("method"))
    exp_var1 = normalize_text(expected.get("var1"))
    exp_var2 = normalize_text(expected.get("var2"))

    for row in summary_df:
        if exp_analysis and normalize_text(row.get("analysis")) != exp_analysis:
            continue
        if exp_group and normalize_text(row.get("group")) != exp_group:
            continue
        if exp_model and normalize_text(row.get("model")) != exp_model:
            continue
        if exp_type and normalize_text(row.get("type")) != exp_type:
            continue
        if exp_unit and normalize_text(row.get("unit")) != exp_unit:
            continue
        if exp_weight and normalize_text(row.get("weight")) != exp_weight:
            continue
        if exp_method and normalize_text(row.get("method")) != exp_method:
            continue
        if exp_var1 and normalize_text(row.get("var1")) != exp_var1:
            continue
        if exp_var2 and normalize_text(row.get("var2")) != exp_var2:
            continue
        return row
    return None


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_reliability_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No reliability log entry found.")

    expected_format = normalize_text(expected.get("format"))
    if expected_format:
        options = entry.get("options", {}) or {}
        actual_format = normalize_text(options.get("format"))
        if actual_format and actual_format != expected_format:
            fail(f"Expected format {expected_format}, got {actual_format}")

    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    row = find_row(summary_df, expected)
    if row is None:
        fail("Expected summary row not found")

    numeric_keys = [
        "estimate",
        "ci_low",
        "ci_high",
        "p_value",
        "f_stat",
        "df1",
        "df2",
        "n",
        "n_raters",
        "missing_n",
        "missing_pct",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
