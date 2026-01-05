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


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def parse_list(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return []
    if isinstance(value, list):
        return [str(v).strip() for v in value if str(v).strip()]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return [str(value)]


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


def match_options(options, expected):
    exp_formula = normalize_text(expected.get("formula"))
    exp_dv = normalize_text(expected.get("dv"))
    exp_fixed = parse_list(expected.get("fixed"))
    exp_random = parse_list(expected.get("random"))

    if exp_formula and normalize_text(options.get("formula")) != exp_formula:
        return False
    if exp_dv and normalize_text(options.get("dv")) != exp_dv:
        return False

    if exp_fixed:
        actual_fixed = parse_list(options.get("fixed"))
        if set(actual_fixed) != set(exp_fixed):
            return False

    if exp_random:
        actual_random = parse_list(options.get("random"))
        if set(actual_random) != set(exp_random):
            return False

    return True


def find_entry(entries, expected):
    for entry in reversed(entries):
        if entry.get("module") != "mixed_models":
            continue
        options = entry.get("options", {}) or {}
        if not match_options(options, expected):
            continue
        return entry
    return None


def find_row(random_df, expected):
    group = normalize_text(expected.get("group"))
    term = normalize_text(expected.get("term"))
    for row in random_df:
        if normalize_text(row.get("group")) != group:
            continue
        if normalize_text(row.get("term")) != term:
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
        fail("Usage: check_mixed_models_random_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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

    entry = find_entry(entries, expected)
    if entry is None:
        fail("No mixed_models log entry found for random effects check")

    results = entry.get("results", {}) or {}
    random_df = results.get("random_effects_df") or []
    if not random_df:
        fail("random_effects_df is empty")

    row = find_row(random_df, expected)
    if row is None:
        fail("Expected random effects row not found")

    numeric_keys = ["variance", "stddev", "corr"]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
