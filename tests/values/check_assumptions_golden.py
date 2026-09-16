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
        number = float(value)
    except (TypeError, ValueError):
        fail(f"Invalid numeric golden value: {value!r}")
    if not math.isfinite(number):
        fail(f"Non-finite numeric golden value: {value!r}")
    return number


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def normalize_arg(value):
    if value in (None, "", "-", "none", "None"):
        return None
    return str(value).strip()


def load_golden(path: Path):
    if not path.exists():
        fail(f"Missing golden file: {path}")
    rows = {}
    with path.open("r", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            case_id = normalize_text(row.get("case_id"))
            if not case_id or case_id in rows:
                fail(f"Missing or duplicate golden case ID: {case_id!r}")
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
                except json.JSONDecodeError as error:
                    fail(f"Malformed JSONL at line {idx}: {error}")
    return entries


def find_entry(entries, expected_analysis: str | None, expected_mode: str | None):
    for entry in reversed(entries):
        if entry.get("module") != "assumptions":
            continue
        options = entry.get("options", {}) or {}
        if expected_analysis and options.get("analysis") != expected_analysis:
            continue
        if expected_mode and options.get("mode") != expected_mode:
            continue
        return entry
    return None


def find_row(checks_df, golden_row):
    fields = ["analysis_type", "model", "assumption", "test", "target", "group"]
    expected = {key: normalize_text(golden_row.get(key)) for key in fields}
    matches = []
    for row in checks_df:
        if not isinstance(row, dict):
            continue
        match = True
        for key in fields:
            if normalize_text(row.get(key)) != expected[key]:
                match = False
                break
        if match:
            matches.append(row)
    if len(matches) > 1:
        fail("Multiple assumptions rows match one golden case")
    return matches[0] if matches else None


def compare_numeric(actual, expected, label, rel_tol=1e-6, abs_tol=1e-6):
    if expected is None:
        if actual is not None:
            fail(f"Expected JSON null for {label}, got {actual!r}")
        return
    if isinstance(actual, bool) or not isinstance(actual, (int, float)):
        fail(f"Non-numeric actual value for {label}: {actual}")
    actual_val = float(actual)
    if not math.isfinite(actual_val):
        fail(f"Non-finite actual value for {label}: {actual}")
    if label == "p":
        if not 0 <= actual_val <= 1 or not 0 <= expected <= 1:
            fail(f"Probability outside [0, 1]: expected {expected}, got {actual_val}")
        rel_tol, abs_tol = 2e-7, 1e-300
    if not math.isclose(actual_val, expected, rel_tol=rel_tol, abs_tol=abs_tol):
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def main():
    if len(sys.argv) < 6:
        fail(
            "Usage: check_assumptions_golden.py <log_path> <start_count> <golden_csv> "
            "<case_id> <analysis> [mode]"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    golden_path = Path(sys.argv[3])
    case_id = sys.argv[4]
    expected_analysis = normalize_arg(sys.argv[5])
    expected_mode = normalize_arg(sys.argv[6]) if len(sys.argv) > 6 else None

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    golden_rows = load_golden(golden_path)
    expected = golden_rows.get(case_id)
    if expected is None:
        fail(f"Golden values missing for case_id: {case_id}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = find_entry(entries, expected_analysis, expected_mode)
    if entry is None:
        fail("No assumptions log entry found for golden check")

    results = entry.get("results", {}) or {}
    checks_df = results.get("checks_df") or []
    if not checks_df:
        fail("checks_df is empty")

    row = find_row(checks_df, expected)
    if row is None:
        fail("Expected assumptions row not found")
    if row.get("status") != "available":
        fail(f"Golden diagnostic is not available: {row.get('status')!r}")

    numeric_keys = ["statistic", "df1", "df2", "p", "value", "n"]
    for key in numeric_keys:
        if key not in row or key not in expected:
            fail(f"Missing numeric field: {key}")
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)
    print(f"Golden {case_id}: all {len(numeric_keys)} numeric/null fields and available status verified")


if __name__ == "__main__":
    main()
