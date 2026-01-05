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


def find_entry(entries):
    for entry in reversed(entries):
        if entry.get("module") == "sem":
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
        fail("Usage: check_sem_fit_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No sem log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}
    analysis = results.get("analysis") or options.get("analysis")
    exp_analysis = normalize_text(expected.get("analysis"))
    if exp_analysis and normalize_text(analysis) != exp_analysis:
        fail(f"Expected analysis {exp_analysis}, got {analysis}")

    fit = results.get("fit")
    if not isinstance(fit, dict):
        fail("Expected fit metrics dict in results")

    numeric_keys = ["chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(fit.get(key), expected_val, key)


if __name__ == "__main__":
    main()
