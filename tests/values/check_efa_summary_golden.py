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
    return text


def parse_float(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def parse_int(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    try:
        return int(float(value))
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
            if not case_id:
                continue
            rows.setdefault(case_id, []).append(row)
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
        if entry.get("module") == "efa":
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


def find_summary_row(summary_df, expected):
    exp_group = normalize_group(expected.get("group"))
    for row in summary_df:
        if normalize_group(row.get("group")) == exp_group:
            return row
    return None


def check_options(options, expected_row):
    expected_method = normalize_text(expected_row.get("method"))
    expected_rotation = normalize_text(expected_row.get("rotation"))
    expected_cor = normalize_text(expected_row.get("cor"))
    expected_missing = normalize_text(expected_row.get("missing"))
    expected_rule = normalize_text(expected_row.get("n_factors_rule"))
    expected_group_var = normalize_text(expected_row.get("group_var"))
    expected_n_factors = parse_float(expected_row.get("n_factors_option"))

    if expected_method and options.get("method") != expected_method:
        fail(f"Expected method {expected_method}, got {options.get('method')}")
    if expected_rotation and options.get("rotation") != expected_rotation:
        fail(f"Expected rotation {expected_rotation}, got {options.get('rotation')}")
    if expected_cor and options.get("cor") != expected_cor:
        fail(f"Expected cor {expected_cor}, got {options.get('cor')}")
    if expected_missing and options.get("missing") != expected_missing:
        fail(f"Expected missing {expected_missing}, got {options.get('missing')}")
    if expected_rule and options.get("n_factors_rule") != expected_rule:
        fail(f"Expected n_factors_rule {expected_rule}, got {options.get('n_factors_rule')}")

    if expected_group_var:
        if options.get("group") != expected_group_var:
            fail(f"Expected group {expected_group_var}, got {options.get('group')}")
    else:
        if options.get("group") not in (None, "", "None"):
            fail(f"Expected empty group, got {options.get('group')}")

    if expected_n_factors is not None:
        actual = options.get("n_factors")
        if actual is None:
            fail(f"Expected n_factors {expected_n_factors}, got None")
        try:
            actual_val = float(actual)
        except (TypeError, ValueError):
            fail(f"Non-numeric n_factors {actual}")
        if not math.isclose(actual_val, expected_n_factors, rel_tol=1e-6, abs_tol=1e-6):
            fail(f"Expected n_factors {expected_n_factors}, got {actual_val}")


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_efa_summary_golden.py <log_path> <start_count> <golden_csv> <case_id>")

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    golden_path = Path(sys.argv[3])
    case_id = sys.argv[4]

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    golden_rows = load_golden(golden_path)
    expected_rows = golden_rows.get(case_id)
    if not expected_rows:
        fail(f"Golden values missing for case_id: {case_id}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = find_entry(entries)
    if entry is None:
        fail("No efa log entry found.")

    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    options = entry.get("options", {}) or {}
    check_options(options, expected_rows[0])

    numeric_keys = [
        "n_obs",
        "n_items",
        "n_factors",
        "eigen_threshold",
        "kmo",
        "bartlett_chi2",
        "bartlett_df",
        "bartlett_p",
        "variance_explained",
    ]

    for expected in expected_rows:
        row = find_summary_row(summary_df, expected)
        if row is None:
            fail(f"Expected summary row not found for group={expected.get('group')}")
        # Check descriptive fields when provided.
        for field in ("method", "rotation", "cor", "missing"):
            expected_text = normalize_text(expected.get(field))
            if expected_text:
                if normalize_text(row.get(field)) != expected_text:
                    fail(f"Expected {field} {expected_text}, got {row.get(field)}")
        for key in numeric_keys:
            expected_val = parse_float(expected.get(key))
            compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
