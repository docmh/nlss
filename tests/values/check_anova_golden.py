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


def parse_list(value):
    if value in (None, "", "-"):
        return []
    if isinstance(value, list):
        return [str(v) for v in value]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return [str(value)]


def parse_float(value):
    if value in (None, "", "-"):
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
            mode = row.get("mode")
            if mode:
                rows[mode] = row
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


def normalize_mode(mode: str) -> str:
    if not mode:
        return ""
    if mode.startswith("between"):
        return "between"
    if mode.startswith("within"):
        return "within"
    if mode.startswith("mixed"):
        return "mixed"
    return mode


def find_entry(entries, mode: str):
    expected_mode = normalize_mode(mode)
    for entry in reversed(entries):
        if entry.get("module") != "anova":
            continue
        options = entry.get("options", {}) or {}
        if options.get("mode") != expected_mode:
            continue
        return entry
    return None


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def find_row(summary_df, model: str, term: str):
    norm_model = normalize_text(model)
    norm_term = normalize_text(term)
    preferred = None
    for row in summary_df:
        row_model = normalize_text(row.get("model"))
        row_term = normalize_text(row.get("term"))
        if row_model != norm_model or row_term != norm_term:
            continue
        # Prefer rows with non-null F statistics when duplicates exist.
        f_val = row.get("f")
        if f_val is not None:
            try:
                if float(f_val) == float(f_val):
                    return row
            except (TypeError, ValueError):
                return row
        if preferred is None:
            preferred = row
    return preferred


def compare_numeric(actual, expected, label, rel_tol=1e-6, abs_tol=1e-6):
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
        fail("Usage: check_anova_golden.py <log_path> <start_count> <golden_csv> <mode>")

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    golden_path = Path(sys.argv[3])
    mode = sys.argv[4]

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    golden_rows = load_golden(golden_path)
    expected = golden_rows.get(mode)
    if expected is None:
        fail(f"Golden values missing for mode: {mode}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = find_entry(entries, mode)
    if entry is None:
        fail(f"No anova log entry found for mode: {mode}")

    options = entry.get("options", {}) or {}
    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []

    model = expected.get("model")
    term = expected.get("term")
    if not model or not term:
        fail("Golden values missing model/term")

    row = find_row(summary_df, model, term)
    if row is None:
        fail(f"Expected row not found for model={model} term={term}")

    # Optional option checks
    if expected.get("dv"):
        if options.get("dv") != expected.get("dv"):
            fail(f"Expected dv {expected.get('dv')}, got {options.get('dv')}")
    if expected.get("between"):
        expected_between = parse_list(expected.get("between"))
        actual_between = parse_list(options.get("between"))
        if actual_between != expected_between:
            fail(f"Expected between {expected_between}, got {actual_between}")
    if expected.get("within"):
        expected_within = parse_list(expected.get("within"))
        actual_within = parse_list(options.get("within"))
        if actual_within != expected_within:
            fail(f"Expected within {expected_within}, got {actual_within}")
    if expected.get("subject_id"):
        if options.get("subject_id") != expected.get("subject_id"):
            fail(f"Expected subject_id {expected.get('subject_id')}, got {options.get('subject_id')}")

    numeric_keys = [
        "df1",
        "df2",
        "ss",
        "ms",
        "f",
        "p",
        "eta_sq",
        "partial_eta_sq",
        "omega_sq",
        "partial_omega_sq",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        if expected_val is None:
            continue
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
