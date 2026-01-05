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
    if exp_formula and normalize_text(options.get("formula")) != exp_formula:
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
        fail("Usage: check_mixed_models_r2_icc_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No mixed_models log entry found for r2/icc check")

    results = entry.get("results", {}) or {}
    r2_df = results.get("r2_df") or []
    icc_df = results.get("icc_df") or []

    if r2_df:
        r2_row = r2_df[0]
        compare_numeric(r2_row.get("r2_marginal"), parse_float(expected.get("r2_marginal")), "r2_marginal")
        compare_numeric(r2_row.get("r2_conditional"), parse_float(expected.get("r2_conditional")), "r2_conditional")
    else:
        fail("r2_df is empty")

    if icc_df:
        icc_row = icc_df[0]
        compare_numeric(icc_row.get("icc"), parse_float(expected.get("icc")), "icc")
    else:
        fail("icc_df is empty")


if __name__ == "__main__":
    main()
