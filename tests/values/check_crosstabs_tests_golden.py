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


def normalize_group(value):
    if value is None:
        return ""
    text = str(value).strip()
    if text.lower() == "none":
        return ""
    return text


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
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        if value == 1:
            return True
        if value == 0:
            return False
    val = str(value).strip().lower()
    if val in ("1", "true", "t", "yes", "y"):
        return True
    if val in ("0", "false", "f", "no", "n"):
        return False
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
        if entry.get("module") == "crosstabs":
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


def compare_bool(actual, expected, label):
    if expected is None:
        return
    actual_val = parse_bool(actual)
    if actual_val is None:
        fail(f"Missing actual value for {label}")
    if actual_val != expected:
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def find_row(tests_df, expected):
    exp_row = normalize_text(expected.get("row_var"))
    exp_col = normalize_text(expected.get("col_var"))
    exp_group = normalize_group(expected.get("group"))
    for row in tests_df:
        if normalize_text(row.get("row_var")) != exp_row:
            continue
        if normalize_text(row.get("col_var")) != exp_col:
            continue
        if normalize_group(row.get("group")) != exp_group:
            continue
        return row
    return None


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_crosstabs_tests_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No crosstabs log entry found.")

    results = entry.get("results", {}) or {}
    tests_df = results.get("tests_df") or []
    if not tests_df:
        fail("tests_df is empty")

    row = find_row(tests_df, expected)
    if row is None:
        fail("Expected tests row not found")

    numeric_keys = [
        "valid_n",
        "total_n",
        "missing_n",
        "missing_pct",
        "row_missing_n",
        "col_missing_n",
        "both_missing_n",
        "n_rows",
        "n_cols",
        "chi_square",
        "chi_df",
        "chi_p",
        "fisher_p",
        "fisher_odds_ratio",
        "fisher_ci_low",
        "fisher_ci_high",
        "phi",
        "cramers_v",
        "contingency_c",
    ]

    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)

    compare_bool(row.get("yates_applied"), parse_bool(expected.get("yates_applied")), "yates_applied")
    compare_bool(row.get("fisher_simulated"), parse_bool(expected.get("fisher_simulated")), "fisher_simulated")

    options = entry.get("options", {}) or {}
    compare_bool(options.get("chisq"), parse_bool(expected.get("opt_chisq")), "opt_chisq")
    compare_bool(options.get("yates"), parse_bool(expected.get("opt_yates")), "opt_yates")
    compare_bool(options.get("fisher"), parse_bool(expected.get("opt_fisher")), "opt_fisher")
    compare_bool(options.get("fisher_simulate"), parse_bool(expected.get("opt_fisher_simulate")), "opt_fisher_simulate")


if __name__ == "__main__":
    main()
