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
    if value in (None, "", "NA", "NaN", "None"):
        return ""
    return str(value).strip()


def normalize_list(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(v).strip() for v in value if str(v).strip()]
    if isinstance(value, str):
        return [v.strip() for v in value.split(",") if v.strip()]
    return [str(value).strip()]


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
        if entry.get("module") == "scale":
            return entry
    return None


def find_row(rel_df, expected):
    exp_group = normalize_text(expected.get("group"))
    for row in rel_df:
        if normalize_text(row.get("group")) != exp_group:
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


def compare_text(actual, expected, label):
    if expected == "":
        return
    if normalize_text(actual) != expected:
        fail(f"Mismatch for {label}: expected {expected}, got {normalize_text(actual)}")


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_scale_reliability_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No scale log entry found.")

    options = entry.get("options", {}) or {}
    expected_missing = normalize_text(expected.get("missing_method"))
    if expected_missing:
        compare_text(options.get("missing"), expected_missing, "missing_method")

    expected_score = normalize_text(expected.get("score_method"))
    if expected_score:
        compare_text(options.get("score"), expected_score, "score_method")

    expected_omega = parse_bool(expected.get("omega"))
    if expected_omega is not None:
        actual_omega = options.get("omega")
        if actual_omega is None or bool(actual_omega) != expected_omega:
            fail(f"Mismatch for omega: expected {expected_omega}, got {actual_omega}")

    expected_group_var = normalize_text(expected.get("group_var"))
    if expected_group_var:
        compare_text(options.get("group"), expected_group_var, "group_var")

    expected_reverse = normalize_list(expected.get("reverse_items"))
    actual_reverse = normalize_list(options.get("reverse"))
    if expected_reverse != actual_reverse:
        fail(f"Mismatch for reverse_items: expected {expected_reverse}, got {actual_reverse}")

    results = entry.get("results", {}) or {}
    rel_df = results.get("reliability_df") or []
    if not rel_df:
        fail("reliability_df is empty")

    row = find_row(rel_df, expected)
    if row is None:
        fail("Expected reliability row not found")

    expected_status = normalize_text(expected.get("omega_status"))
    if expected_status:
        compare_text(row.get("omega_status"), expected_status, "omega_status")

    numeric_keys = [
        "n_items",
        "n_total",
        "n_complete",
        "missing_n",
        "missing_pct",
        "alpha",
        "alpha_std",
        "omega_total",
        "r_bar",
        "r_min",
        "r_max",
        "score_mean",
        "score_sd",
        "score_min",
        "score_max",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
