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
    if value in (None, "", "-", "NA", "NaN", "None", "null"):
        return None
    text = str(value).strip()
    if text == "":
        return None
    lowered = text.lower()
    if lowered in ("inf", "+inf", "infinity", "+infinity"):
        return math.inf
    if lowered in ("-inf", "-infinity"):
        return -math.inf
    try:
        return float(text)
    except (TypeError, ValueError):
        return None


def is_missing(value):
    if value is None:
        return True
    if isinstance(value, str):
        lowered = value.strip().lower()
        return lowered in ("", "-", "na", "nan", "none", "null")
    return False


def parse_bool(value):
    if value in (None, "", "-", "NA", "NaN", "None", "null"):
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
    if value in (None, "", "-", "NA", "NaN", "None", "null"):
        return []
    if isinstance(value, list):
        return [str(item).strip() for item in value if str(item).strip()]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return [str(value).strip()]


def normalize_list_str(value):
    return ",".join(normalize_list(value))


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
    if entry.get("module") != "t_test":
        return False
    options = entry.get("options", {}) or {}

    exp_mode = normalize_text(expected.get("mode"))
    if exp_mode and normalize_text(options.get("mode")) != exp_mode:
        return False

    exp_vars = normalize_text(expected.get("vars"))
    if exp_vars and normalize_list_str(options.get("vars")) != exp_vars:
        return False

    exp_x = normalize_text(expected.get("x"))
    if exp_x and normalize_list_str(options.get("x")) != exp_x:
        return False

    exp_y = normalize_text(expected.get("y"))
    if exp_y and normalize_list_str(options.get("y")) != exp_y:
        return False

    exp_group = normalize_text(expected.get("group"))
    if exp_group and normalize_text(options.get("group")) != exp_group:
        return False

    exp_alt = normalize_text(expected.get("alternative"))
    if exp_alt and normalize_text(options.get("alternative")) != exp_alt:
        return False

    exp_var_equal = parse_bool(expected.get("var_equal"))
    if exp_var_equal is not None:
        if parse_bool(options.get("var_equal")) != exp_var_equal:
            return False

    exp_conf = parse_float(expected.get("conf_level"))
    if exp_conf is not None:
        actual_conf = parse_float(options.get("conf_level"))
        if actual_conf is None or not math.isclose(actual_conf, exp_conf, rel_tol=1e-6, abs_tol=1e-6):
            return False

    exp_boot = parse_bool(expected.get("bootstrap"))
    if exp_boot is not None:
        if parse_bool(options.get("bootstrap")) != exp_boot:
            return False

    exp_boot_samples = parse_float(expected.get("bootstrap_samples"))
    if exp_boot_samples is not None:
        actual_boot_samples = parse_float(options.get("bootstrap_samples"))
        if actual_boot_samples is None or not math.isclose(actual_boot_samples, exp_boot_samples, rel_tol=1e-6, abs_tol=1e-6):
            return False

    exp_mu = parse_float(expected.get("mu"))
    if exp_mu is not None:
        actual_mu = parse_float(options.get("mu"))
        if actual_mu is None or not math.isclose(actual_mu, exp_mu, rel_tol=1e-6, abs_tol=1e-6):
            return False

    return True


def find_entry(entries, expected):
    for entry in reversed(entries):
        if match_entry(entry, expected):
            return entry
    return None


def find_row(summary_df, expected):
    exp_test_type = normalize_text(expected.get("test_type"))
    exp_variable = normalize_text(expected.get("variable"))
    exp_measure_1 = normalize_text(expected.get("measure_1"))
    exp_measure_2 = normalize_text(expected.get("measure_2"))
    exp_group_1 = normalize_text(expected.get("group_1"))
    exp_group_2 = normalize_text(expected.get("group_2"))
    for row in summary_df:
        if normalize_text(row.get("test_type")) != exp_test_type:
            continue
        if normalize_text(row.get("variable")) != exp_variable:
            continue
        if normalize_text(row.get("measure_1")) != exp_measure_1:
            continue
        if normalize_text(row.get("measure_2")) != exp_measure_2:
            continue
        if normalize_text(row.get("group_1")) != exp_group_1:
            continue
        if normalize_text(row.get("group_2")) != exp_group_2:
            continue
        return row
    return None


def compare_numeric(actual, expected, label, rel_tol=1e-6, abs_tol=1e-6):
    if expected is None:
        if not is_missing(actual):
            fail(f"Expected missing value for {label}, got {actual!r}")
        return
    actual_val = parse_float(actual)
    if actual_val is None:
        fail(f"Missing actual value for {label}")
    if math.isinf(expected):
        if not math.isinf(actual_val) or (expected < 0) != (actual_val < 0):
            fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")
        return
    if not math.isclose(actual_val, expected, rel_tol=rel_tol, abs_tol=abs_tol):
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_t_test_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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
        fail("No matching t_test log entry found.")

    results = entry.get("results", {}) or {}
    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    row = find_row(summary_df, expected)
    if row is None:
        fail("Expected summary row not found")

    numeric_keys = [
        "n_1",
        "n_2",
        "mean_1",
        "mean_2",
        "sd_1",
        "sd_2",
        "mean_diff",
        "t",
        "df",
        "p",
        "d",
        "ci_low",
        "ci_high",
        "boot_ci_low",
        "boot_ci_high",
        "boot_d_ci_low",
        "boot_d_ci_high",
        "mu",
    ]
    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
