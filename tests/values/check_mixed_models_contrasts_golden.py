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
    exp_emmeans = normalize_text(expected.get("emmeans"))
    exp_contrasts = normalize_text(expected.get("contrasts"))
    if exp_formula and normalize_text(options.get("formula")) != exp_formula:
        return False
    if exp_emmeans and normalize_text(options.get("emmeans")) != exp_emmeans:
        return False
    if exp_contrasts and normalize_text(options.get("contrasts")) != exp_contrasts:
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


def find_row(contrasts_df, expected):
    term = normalize_text(expected.get("term"))
    contrast = normalize_text(expected.get("contrast"))
    for row in contrasts_df:
        if normalize_text(row.get("term")) != term:
            continue
        if normalize_text(row.get("contrast")) != contrast:
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
        fail("Usage: check_mixed_models_contrasts_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No mixed_models log entry found for contrasts check")

    results = entry.get("results", {}) or {}
    contrasts_df = results.get("contrasts_df") or []
    if not contrasts_df:
        fail("contrasts_df is empty")

    row = find_row(contrasts_df, expected)
    if row is None:
        fail("Expected contrasts row not found")

    numeric_keys = ["estimate", "se", "df", "t", "p", "p_adj", "ci_low", "ci_high"]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
