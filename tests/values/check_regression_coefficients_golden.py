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


def parse_bool(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return None
    val = str(value).strip().lower()
    if val in ("1", "true", "t", "yes", "y"):
        return True
    if val in ("0", "false", "f", "no", "n"):
        return False
    return None


def normalize_text(value):
    if value is None:
        return ""
    return str(value).strip()


def normalize_list(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return []
    if isinstance(value, list):
        return [str(item).strip() for item in value if str(item).strip()]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return [str(value)]


def normalize_blocks(value):
    if value in (None, "", "-", "NA", "NaN", "None"):
        return ""
    if isinstance(value, list):
        parts = []
        for block in value:
            if isinstance(block, list):
                items = [str(item).strip() for item in block if str(item).strip()]
            else:
                item = str(block).strip()
                items = [item] if item else []
            parts.append(",".join(items))
        return ";".join(parts)
    if isinstance(value, str):
        return value.strip()
    return str(value).strip()


def normalize_list_str(value):
    items = normalize_list(value)
    return ",".join(items)


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


def match_entry(entry, expected):
    if entry.get("module") != "regression":
        return False
    options = entry.get("options", {}) or {}

    exp_dv = normalize_text(expected.get("dv"))
    if exp_dv and normalize_text(options.get("dv")) != exp_dv:
        return False

    exp_family = normalize_text(expected.get("family"))
    if exp_family and normalize_text(options.get("family")) != exp_family:
        return False

    exp_link = normalize_text(expected.get("link"))
    if exp_link and normalize_text(options.get("link")) != exp_link:
        return False

    exp_center = normalize_text(expected.get("center"))
    if exp_center and normalize_text(options.get("center")) != exp_center:
        return False

    exp_standardize = normalize_text(expected.get("standardize"))
    if exp_standardize and normalize_text(options.get("standardize")) != exp_standardize:
        return False

    exp_blocks = normalize_text(expected.get("blocks"))
    if exp_blocks and normalize_blocks(options.get("blocks")) != exp_blocks:
        return False

    exp_ivs = normalize_text(expected.get("ivs"))
    if exp_ivs and normalize_list_str(options.get("ivs")) != exp_ivs:
        return False

    exp_interactions = normalize_text(expected.get("interactions"))
    if exp_interactions and normalize_list_str(options.get("interactions")) != exp_interactions:
        return False

    exp_group_var = normalize_text(expected.get("group_var"))
    if exp_group_var and normalize_text(options.get("group")) != exp_group_var:
        return False

    exp_bootstrap = parse_bool(expected.get("bootstrap"))
    if exp_bootstrap is not None:
        if parse_bool(options.get("bootstrap")) != exp_bootstrap:
            return False

    exp_boot_samples = parse_float(expected.get("bootstrap_samples"))
    if exp_boot_samples is not None:
        try:
            if float(options.get("bootstrap_samples")) != exp_boot_samples:
                return False
        except (TypeError, ValueError):
            return False

    return True


def find_entry(entries, expected):
    for entry in reversed(entries):
        if match_entry(entry, expected):
            return entry
    return None


def find_row(coefficients_df, expected):
    exp_model = normalize_text(expected.get("model"))
    exp_term = normalize_text(expected.get("term"))
    exp_group = normalize_text(expected.get("group"))
    for row in coefficients_df:
        if normalize_text(row.get("model")) != exp_model:
            continue
        if normalize_text(row.get("term")) != exp_term:
            continue
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


def main():
    if len(sys.argv) < 5:
        fail(
            "Usage: check_regression_coefficients_golden.py <log_path> <start_count> <golden_csv> <case_id>"
        )

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
        fail("No regression log entry found for coefficients")

    results = entry.get("results", {}) or {}
    coefficients_df = results.get("coefficients_df") or []
    if not coefficients_df:
        fail("coefficients_df is empty")

    row = find_row(coefficients_df, expected)
    if row is None:
        fail("Expected coefficients row not found")

    numeric_keys = [
        "estimate",
        "se",
        "stat",
        "p",
        "ci_low",
        "ci_high",
        "beta",
        "exp_b",
        "exp_ci_low",
        "exp_ci_high",
        "boot_ci_low",
        "boot_ci_high",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
