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


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def is_missing(value) -> bool:
    if value is None:
        return True
    if isinstance(value, float) and math.isnan(value):
        return True
    text = str(value).strip().lower()
    return text in ("", "na", "nan", "none", "null")


def parse_float(value):
    if is_missing(value):
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


def normalize_group(value):
    if value is None:
        return ""
    text = str(value).strip()
    if text.lower() in ("none", "null"):
        return ""
    return text


def match_entry(entries, expected):
    exp_group_var = normalize_text(expected.get("group_var"))
    exp_trim = parse_float(expected.get("trim"))
    exp_iqr = parse_float(expected.get("iqr_multiplier"))
    exp_outlier_z = parse_float(expected.get("outlier_z"))

    for entry in reversed(entries):
        if entry.get("module") != "descriptive_stats":
            continue
        options = entry.get("options", {}) or {}
        actual_group_var = normalize_group(options.get("group"))
        if exp_group_var:
            if actual_group_var != exp_group_var:
                continue
        else:
            if actual_group_var not in ("", "None"):
                continue
        if exp_trim is not None:
            actual_trim = parse_float(options.get("trim"))
            if actual_trim is None or not math.isclose(actual_trim, exp_trim, rel_tol=1e-6, abs_tol=1e-6):
                continue
        if exp_iqr is not None:
            actual_iqr = parse_float(options.get("iqr_multiplier"))
            if actual_iqr is None or not math.isclose(actual_iqr, exp_iqr, rel_tol=1e-6, abs_tol=1e-6):
                continue
        if exp_outlier_z is not None:
            actual_z = parse_float(options.get("outlier_z"))
            if actual_z is None or not math.isclose(actual_z, exp_outlier_z, rel_tol=1e-6, abs_tol=1e-6):
                continue
        return entry
    return None


def find_row(summary_df, expected):
    exp_var = normalize_text(expected.get("variable"))
    exp_group = normalize_group(expected.get("group"))

    for row in summary_df:
        if normalize_text(row.get("variable")) != exp_var:
            continue
        row_group = normalize_group(row.get("group"))
        if exp_group == "":
            if row_group in ("", "None"):
                return row
        else:
            if row_group == exp_group:
                return row
    return None


def compare_numeric(actual, expected, label, rel_tol=1e-6, abs_tol=1e-6):
    if expected is None:
        if not is_missing(actual):
            fail(f"Expected {label} to be NA, got {actual}")
        return
    if actual is None or is_missing(actual):
        fail(f"Missing actual value for {label}")
    try:
        actual_val = float(actual)
    except (TypeError, ValueError):
        fail(f"Non-numeric actual value for {label}: {actual}")
    if not math.isclose(actual_val, expected, rel_tol=rel_tol, abs_tol=abs_tol):
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_descriptive_stats_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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

    entry = match_entry(entries, expected)
    if entry is None:
        fail("No descriptive_stats log entry found for expected options")

    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    row = find_row(summary_df, expected)
    if row is None:
        fail("Expected summary row not found")

    numeric_keys = [
        "total_n",
        "n",
        "missing_n",
        "missing_pct",
        "mean",
        "sd",
        "median",
        "min",
        "max",
        "variance",
        "range",
        "q1",
        "q3",
        "iqr",
        "mad",
        "cv",
        "trimmed_mean",
        "p5",
        "p10",
        "p90",
        "p95",
        "outliers_tukey",
        "outliers_z",
        "mode",
        "n_unique",
        "se",
        "ci_low",
        "ci_high",
        "skewness",
        "kurtosis",
    ]

    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
