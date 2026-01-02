# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_list(value):
    if value in (None, "", "-"):
        return None
    items = [item.strip() for item in str(value).split(",")]
    items = [item for item in items if item]
    return items or None


def normalize_list(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(v) for v in value]
    if isinstance(value, str):
        return [item.strip() for item in value.split(",") if item.strip()]
    return [str(value)]


def parse_int(value):
    if value in (None, "", "-"):
        return None
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def parse_float(value):
    if value in (None, "", "-"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def parse_kv_args(args):
    extra = {}
    for arg in args:
        if "=" not in arg:
            continue
        key, value = arg.split("=", 1)
        key = key.strip()
        value = value.strip()
        if key:
            extra[key] = value
    return extra


def load_entries(path, start_count):
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


def fail(message):
    print(message)
    sys.exit(1)


def ensure_columns(rows, columns, label):
    if not columns:
        return
    missing = [col for col in columns if all(col not in row for row in rows)]
    if missing:
        fail(f"{label} missing columns: {', '.join(missing)}")


def ensure_non_null(rows, columns, label):
    if not columns:
        return
    for col in columns:
        if not any(row.get(col) is not None for row in rows):
            fail(f"{label} column has only null values: {col}")


def floats_close(actual, expected, tol=1e-6):
    if actual is None or expected is None:
        return False
    return abs(actual - expected) <= tol


def parse_expectation(value):
    parts = [part.strip() for part in value.split("|")]
    if len(parts) != 4:
        fail(f"Invalid expectation format: {value} (expected var|group|metric|value)")
    var, group, metric, expected = parts
    group = group if group not in ("", "-", "none", "None") else ""
    expected = expected.strip()
    return var, group, metric, expected


def normalize_row_value(row_value):
    if row_value is None:
        return None
    try:
        return float(row_value)
    except (TypeError, ValueError):
        return row_value


def match_group(row_group, expected_group):
    if expected_group == "":
        return row_group in (None, "", "None")
    return str(row_group) == expected_group


def main():
    if len(sys.argv) < 3:
        fail("Usage: check_descriptive_stats_log.py <log_path> <start_count> [key=value...]")

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    extra = parse_kv_args(sys.argv[3:])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "descriptive_stats":
            entry = candidate
            break
    if entry is None:
        fail("No descriptive_stats log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}

    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    columns = parse_list(extra.get("columns")) or [
        "variable",
        "group",
        "total_n",
        "n",
        "missing_n",
        "missing_pct",
        "mean",
        "sd",
        "median",
        "min",
        "max",
        "variance",
        "range",
        "q1",
        "q3",
        "iqr",
        "mad",
        "cv",
        "trimmed_mean",
        "p5",
        "p10",
        "p90",
        "p95",
        "outliers_tukey",
        "outliers_z",
        "mode",
        "n_unique",
        "se",
        "ci_low",
        "ci_high",
        "skewness",
        "kurtosis",
    ]
    ensure_columns(summary_df, columns, "summary_df")

    nonnull = parse_list(extra.get("nonnull")) or []
    ensure_non_null(summary_df, nonnull, "summary_df")

    min_rows = parse_int(extra.get("min_rows"))
    if min_rows is not None and len(summary_df) < min_rows:
        fail(f"Expected at least {min_rows} rows, got {len(summary_df)}")

    expected_vars = parse_list(extra.get("vars"))
    if expected_vars is not None:
        actual_vars = normalize_list(options.get("vars"))
        if actual_vars != expected_vars:
            fail(f"Expected vars {expected_vars}, got {actual_vars}")

    expected_group = extra.get("group")
    if expected_group is not None:
        actual_group = options.get("group")
        if expected_group in ("-", "", "none", "None"):
            if actual_group not in (None, "", "None"):
                fail(f"Expected group to be empty, got {actual_group}")
        else:
            if actual_group != expected_group:
                fail(f"Expected group {expected_group}, got {actual_group}")

    expected_prompt = extra.get("user_prompt")
    if expected_prompt is not None:
        actual_prompt = entry.get("user_prompt")
        if actual_prompt != expected_prompt:
            fail(f"Expected user_prompt {expected_prompt}, got {actual_prompt}")

    expected_trim = parse_float(extra.get("trim"))
    if expected_trim is not None:
        actual_trim = parse_float(options.get("trim"))
        if not floats_close(actual_trim, expected_trim):
            fail(f"Expected trim {expected_trim}, got {options.get('trim')}")

    expected_iqr = parse_float(extra.get("iqr_multiplier"))
    if expected_iqr is not None:
        actual_iqr = parse_float(options.get("iqr_multiplier"))
        if not floats_close(actual_iqr, expected_iqr):
            fail(f"Expected iqr_multiplier {expected_iqr}, got {options.get('iqr_multiplier')}")

    expected_z = parse_float(extra.get("outlier_z"))
    if expected_z is not None:
        actual_z = parse_float(options.get("outlier_z"))
        if not floats_close(actual_z, expected_z):
            fail(f"Expected outlier_z {expected_z}, got {options.get('outlier_z')}")

    require_vars = parse_list(extra.get("require_vars"))
    if require_vars:
        missing_vars = []
        for var in require_vars:
            if not any(str(row.get("variable")) == var for row in summary_df):
                missing_vars.append(var)
        if missing_vars:
            fail(f"Missing expected variables: {', '.join(missing_vars)}")

    expectations = []
    for key, value in extra.items():
        if key.startswith("expect_"):
            expectations.append(value)

    for expectation in expectations:
        var, group, metric, expected = parse_expectation(expectation)
        matches = [
            row
            for row in summary_df
            if str(row.get("variable")) == var and match_group(row.get("group"), group)
        ]
        if not matches:
            fail(f"Expected row not found for variable={var} group={group or 'None'}")
        if metric == "exists":
            continue
        row_value = matches[0].get(metric)
        if expected in ("NA", "null", "None"):
            if row_value is not None:
                fail(f"Expected {var}:{metric} to be NA, got {row_value}")
            continue
        expected_float = parse_float(expected)
        actual_value = normalize_row_value(row_value)
        if expected_float is not None:
            if actual_value is None or not floats_close(float(actual_value), expected_float):
                fail(f"Expected {var}:{metric} {expected_float}, got {row_value}")
        else:
            if str(actual_value) != expected:
                fail(f"Expected {var}:{metric} {expected}, got {row_value}")


if __name__ == "__main__":
    main()
