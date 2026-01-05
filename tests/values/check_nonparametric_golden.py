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


def normalize_expected_text(value):
    if value is None:
        return ""
    text = str(value).strip()
    if text == "":
        return ""
    if text.lower() in ("na", "nan", "none", "null"):
        return ""
    return text


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
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        if value == 1:
            return True
        if value == 0:
            return False
    val = str(value).strip().lower()
    if val in ("1", "true", "t", "yes", "y"):
        return True
    if val in ("0", "false", "f", "no", "n"):
        return False
    return None


def parse_list(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(v).strip() for v in value if str(v).strip()]
    text = str(value).strip()
    if text == "" or text.lower() in ("none", "null", "na"):
        return []
    return [item.strip() for item in text.split(",") if item.strip()]


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


def compare_bool(actual, expected, label):
    if expected is None:
        return
    actual_val = parse_bool(actual)
    if actual_val is None:
        fail(f"Missing actual value for {label}")
    if actual_val != expected:
        fail(f"Mismatch for {label}: expected {expected}, got {actual_val}")


def compare_list(actual, expected, label):
    expected_list = parse_list(expected)
    if not expected_list:
        return
    actual_list = parse_list(actual)
    if actual_list != expected_list:
        fail(f"Mismatch for {label}: expected {expected_list}, got {actual_list}")


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


def match_entry(entries, expected):
    exp_test = normalize_expected_text(expected.get("opt_test"))
    exp_mode = normalize_expected_text(expected.get("opt_mode"))
    exp_group = normalize_expected_text(expected.get("opt_group"))
    exp_vars = expected.get("opt_vars")
    exp_x = expected.get("opt_x")
    exp_y = expected.get("opt_y")
    exp_within = expected.get("opt_within")
    exp_subject = normalize_expected_text(expected.get("opt_subject_id"))
    exp_mu = parse_float(expected.get("opt_mu"))
    exp_alt = normalize_expected_text(expected.get("opt_alternative"))
    exp_exact = parse_bool(expected.get("opt_exact"))
    exp_cont = parse_bool(expected.get("opt_continuity"))
    exp_conf = parse_float(expected.get("opt_conf_level"))
    exp_posthoc = normalize_expected_text(expected.get("opt_posthoc"))
    exp_p_adjust = normalize_expected_text(expected.get("opt_p_adjust"))
    exp_effect = normalize_expected_text(expected.get("opt_effect_size"))
    exp_digits = parse_float(expected.get("opt_digits"))

    for entry in reversed(entries):
        if entry.get("module") != "nonparametric":
            continue
        options = entry.get("options", {}) or {}
        if exp_test and normalize_text(options.get("test")) != exp_test:
            continue
        if exp_mode and normalize_text(options.get("mode")) != exp_mode:
            continue
        if exp_group and normalize_text(options.get("group")) != exp_group:
            continue
        if exp_subject and normalize_text(options.get("subject_id")) != exp_subject:
            continue
        if exp_mu is not None:
            actual_mu = parse_float(options.get("mu"))
            if actual_mu is None or not math.isclose(actual_mu, exp_mu, rel_tol=1e-6, abs_tol=1e-6):
                continue
        if exp_alt and normalize_text(options.get("alternative")) != exp_alt:
            continue
        if exp_exact is not None and parse_bool(options.get("exact")) != exp_exact:
            continue
        if exp_cont is not None and parse_bool(options.get("continuity")) != exp_cont:
            continue
        if exp_conf is not None:
            actual_conf = parse_float(options.get("conf_level"))
            if actual_conf is None or not math.isclose(actual_conf, exp_conf, rel_tol=1e-6, abs_tol=1e-6):
                continue
        if exp_posthoc and normalize_text(options.get("posthoc")) != exp_posthoc:
            continue
        if exp_p_adjust and normalize_text(options.get("p_adjust")) != exp_p_adjust:
            continue
        if exp_effect and normalize_text(options.get("effect_size")) != exp_effect:
            continue
        if exp_digits is not None:
            actual_digits = parse_float(options.get("digits"))
            if actual_digits is None or not math.isclose(actual_digits, exp_digits, rel_tol=1e-6, abs_tol=1e-6):
                continue
        if exp_vars:
            compare_list(options.get("vars"), exp_vars, "opt_vars")
        if exp_x:
            compare_list(options.get("x"), exp_x, "opt_x")
        if exp_y:
            compare_list(options.get("y"), exp_y, "opt_y")
        if exp_within:
            compare_list(options.get("within"), exp_within, "opt_within")
        return entry
    return None


def find_row(summary_df, expected):
    exp_test = normalize_expected_text(expected.get("test_type"))
    exp_var = normalize_expected_text(expected.get("variable"))
    exp_m1 = normalize_expected_text(expected.get("measure_1"))
    exp_m2 = normalize_expected_text(expected.get("measure_2"))
    exp_group = normalize_expected_text(expected.get("group"))
    exp_g1 = normalize_expected_text(expected.get("group_1"))
    exp_g2 = normalize_expected_text(expected.get("group_2"))

    for row in summary_df:
        if exp_test and normalize_text(row.get("test_type")) != exp_test:
            continue
        if exp_var and normalize_text(row.get("variable")) != exp_var:
            continue
        if exp_m1 and normalize_text(row.get("measure_1")) != exp_m1:
            continue
        if exp_m2 and normalize_text(row.get("measure_2")) != exp_m2:
            continue
        if exp_group and normalize_text(row.get("group")) != exp_group:
            continue
        if exp_g1 and normalize_text(row.get("group_1")) != exp_g1:
            continue
        if exp_g2 and normalize_text(row.get("group_2")) != exp_g2:
            continue
        return row
    return None


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_nonparametric_golden.py <log_path> <start_count> <golden_csv> <case_id>")

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

    entry = match_entry(entries, expected)
    if entry is None:
        fail("No nonparametric log entry found for expected options")

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
        "n_total",
        "median_1",
        "median_2",
        "iqr_1",
        "iqr_2",
        "median",
        "iqr",
        "median_diff",
        "iqr_diff",
        "statistic",
        "df",
        "p",
        "effect_size_value",
        "ci_low",
        "ci_high",
        "mu",
    ]

    for key in numeric_keys:
        expected_val = parse_float(expected.get(key))
        compare_numeric(row.get(key), expected_val, key)


if __name__ == "__main__":
    main()
