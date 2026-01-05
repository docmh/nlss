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
    return str(value).strip()


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


def find_eigen_row(eigen_df, expected):
    exp_group = normalize_group(expected.get("group"))
    exp_component = parse_int(expected.get("component"))
    for row in eigen_df:
        if normalize_group(row.get("group")) != exp_group:
            continue
        row_component = parse_int(row.get("component"))
        if row_component != exp_component:
            continue
        return row
    return None


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_efa_eigen_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
    eigen_df = results.get("eigen_df") or []
    if not eigen_df:
        fail("eigen_df is empty")

    for expected in expected_rows:
        row = find_eigen_row(eigen_df, expected)
        if row is None:
            fail(f"Expected eigen row not found for component={expected.get('component')} group={expected.get('group')}")
        compare_numeric(row.get("eigenvalue"), parse_float(expected.get("eigenvalue")), "eigenvalue")
        compare_numeric(row.get("proportion"), parse_float(expected.get("proportion")), "proportion")
        compare_numeric(row.get("cumulative"), parse_float(expected.get("cumulative")), "cumulative")


if __name__ == "__main__":
    main()
