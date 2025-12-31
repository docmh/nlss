#!/usr/bin/env python3
import json
import math
import sys
from pathlib import Path


def parse_bool(value):
    if value in (None, "", "-"):
        return None
    val = str(value).strip().lower()
    if val in ("1", "true", "yes", "y", "t"):
        return True
    if val in ("0", "false", "no", "n", "f"):
        return False
    return None


def parse_int(value):
    if value in (None, "", "-"):
        return None
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def parse_float(value):
    if value in (None, "", "-"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


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


def has_non_null(rows, key):
    for row in rows:
        value = row.get(key)
        if value is not None:
            return True
    return False


def is_truthy(value):
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        return value != 0
    if isinstance(value, str):
        return value.strip().lower() in ("1", "true", "yes", "y", "t")
    return False


def any_truthy(rows, key):
    return any(is_truthy(row.get(key)) for row in rows)


def compare_float(actual, expected, label):
    if actual is None:
        fail(f"Expected {label} {expected}, got None")
    try:
        actual_val = float(actual)
    except (TypeError, ValueError):
        fail(f"Expected {label} {expected}, got {actual}")
    if not math.isclose(actual_val, expected, rel_tol=1e-6, abs_tol=1e-6):
        fail(f"Expected {label} {expected}, got {actual_val}")


def main():
    if len(sys.argv) < 3:
        fail("Usage: check_correlations_log.py <log_path> <start_count> [key=value...]")

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
        if candidate.get("module") == "correlations":
            entry = candidate
            break
    if entry is None:
        fail("No correlations log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}

    expected_status = extra.get("status")
    if expected_status is not None:
        status = results.get("status")
        if status != expected_status:
            fail(f"Expected status {expected_status}, got {status}")
        return

    summary_df = results.get("summary_df") or []
    diagnostics_df = results.get("diagnostics_df") or []
    comparison_df = results.get("comparison_df") or []

    if not summary_df:
        fail("summary_df is empty")
    if not diagnostics_df:
        fail("diagnostics_df is empty")

    summary_cols = parse_list(extra.get("cols")) or [
        "var1",
        "var2",
        "group",
        "method",
        "alternative",
        "controls",
        "partial",
        "n",
        "total_n",
        "missing_n",
        "missing_pct",
        "r",
        "p_value",
        "ci_low",
        "ci_high",
        "boot_ci_low",
        "boot_ci_high",
        "missing_method",
        "conf_level",
        "p_adjusted",
        "p_adjust_method",
        "r0",
        "z_r0",
        "p_r0",
    ]
    diag_cols = parse_list(extra.get("diagnostic_cols")) or [
        "variable",
        "group",
        "n",
        "total_n",
        "missing_n",
        "missing_pct",
        "skewness",
        "kurtosis",
        "shapiro_w",
        "shapiro_p",
    ]
    ensure_columns(summary_df, summary_cols, "summary_df")
    ensure_columns(diagnostics_df, diag_cols, "diagnostics_df")

    min_rows = parse_int(extra.get("min_rows"))
    if min_rows is not None and len(summary_df) < min_rows:
        fail(f"Expected at least {min_rows} summary rows, got {len(summary_df)}")

    min_groups = parse_int(extra.get("min_groups"))
    if min_groups is not None:
        groups = {str(row.get("group")) for row in summary_df if row.get("group") not in (None, "")}
        if len(groups) < min_groups:
            fail(f"Expected at least {min_groups} group(s), got {len(groups)}")

    expect_boot = parse_bool(extra.get("expect_boot"))
    if expect_boot is True and not has_non_null(summary_df, "boot_ci_low"):
        fail("Expected bootstrap confidence intervals to be present")

    expect_r0 = parse_bool(extra.get("expect_r0"))
    if expect_r0 is True:
        if not has_non_null(summary_df, "r0"):
            fail("Expected r0 values to be present")
        if not has_non_null(summary_df, "z_r0"):
            fail("Expected z_r0 values to be present")

    expect_partial = parse_bool(extra.get("expect_partial"))
    if expect_partial is True and not any_truthy(summary_df, "partial"):
        fail("Expected partial correlations")

    expect_compare = parse_bool(extra.get("expect_compare"))
    if expect_compare is True:
        if not comparison_df:
            fail("Expected comparison_df to be present")
        comparison_cols = parse_list(extra.get("comparison_cols")) or [
            "var1",
            "var2",
            "group1",
            "group2",
            "r1",
            "r2",
            "n1",
            "n2",
            "z",
            "p_value",
        ]
        ensure_columns(comparison_df, comparison_cols, "comparison_df")
        min_comparisons = parse_int(extra.get("min_comparisons"))
        if min_comparisons is not None and len(comparison_df) < min_comparisons:
            fail(f"Expected at least {min_comparisons} comparisons, got {len(comparison_df)}")
    elif expect_compare is False:
        if comparison_df:
            fail("Expected comparison_df to be empty")

    expected_vars = parse_list(extra.get("vars"))
    if expected_vars is not None:
        actual_vars = normalize_list(options.get("vars"))
        if actual_vars != expected_vars:
            fail(f"Expected vars {expected_vars}, got {actual_vars}")

    expected_x = parse_list(extra.get("x"))
    if expected_x is not None:
        actual_x = normalize_list(options.get("x"))
        if actual_x != expected_x:
            fail(f"Expected x {expected_x}, got {actual_x}")

    expected_y = parse_list(extra.get("y"))
    if expected_y is not None:
        actual_y = normalize_list(options.get("y"))
        if actual_y != expected_y:
            fail(f"Expected y {expected_y}, got {actual_y}")

    expected_controls = parse_list(extra.get("controls"))
    if expected_controls is not None:
        actual_controls = normalize_list(options.get("controls"))
        if actual_controls != expected_controls:
            fail(f"Expected controls {expected_controls}, got {actual_controls}")

    expected_group = extra.get("group")
    if expected_group is not None:
        actual_group = options.get("group")
        if expected_group in ("-", "", "none", "None"):
            if actual_group not in (None, "", "None"):
                fail(f"Expected group to be empty, got {actual_group}")
        else:
            if actual_group != expected_group:
                fail(f"Expected group {expected_group}, got {actual_group}")

    expected_method = extra.get("method")
    if expected_method is not None:
        actual_method = options.get("method")
        if actual_method != expected_method:
            fail(f"Expected method {expected_method}, got {actual_method}")

    expected_missing = extra.get("missing")
    if expected_missing is not None:
        actual_missing = options.get("missing")
        if actual_missing != expected_missing:
            fail(f"Expected missing {expected_missing}, got {actual_missing}")

    expected_alternative = extra.get("alternative")
    if expected_alternative is not None:
        actual_alternative = options.get("alternative")
        if actual_alternative != expected_alternative:
            fail(f"Expected alternative {expected_alternative}, got {actual_alternative}")

    expected_p_adjust = extra.get("p_adjust")
    if expected_p_adjust is not None:
        actual_p_adjust = options.get("p_adjust")
        if actual_p_adjust != expected_p_adjust:
            fail(f"Expected p_adjust {expected_p_adjust}, got {actual_p_adjust}")

    expected_conf_level = parse_float(extra.get("conf_level"))
    if expected_conf_level is not None:
        compare_float(options.get("conf_level"), expected_conf_level, "conf_level")

    expected_bootstrap = parse_bool(extra.get("bootstrap"))
    if expected_bootstrap is not None:
        actual_bootstrap = options.get("bootstrap")
        if actual_bootstrap is None or bool(actual_bootstrap) != expected_bootstrap:
            fail(f"Expected bootstrap {expected_bootstrap}, got {actual_bootstrap}")

    expected_bootstrap_samples = parse_int(extra.get("bootstrap_samples"))
    if expected_bootstrap_samples is not None:
        actual_bootstrap_samples = parse_int(options.get("bootstrap_samples"))
        if actual_bootstrap_samples != expected_bootstrap_samples:
            fail(f"Expected bootstrap_samples {expected_bootstrap_samples}, got {actual_bootstrap_samples}")

    expected_r0 = parse_float(extra.get("r0"))
    if expected_r0 is not None:
        compare_float(options.get("r0"), expected_r0, "r0")

    expected_compare_groups = parse_bool(extra.get("compare_groups"))
    if expected_compare_groups is not None:
        actual_compare_groups = options.get("compare_groups")
        if actual_compare_groups is None or bool(actual_compare_groups) != expected_compare_groups:
            fail(f"Expected compare_groups {expected_compare_groups}, got {actual_compare_groups}")

    expected_digits = parse_int(extra.get("digits"))
    if expected_digits is not None:
        actual_digits = parse_int(options.get("digits"))
        if actual_digits != expected_digits:
            fail(f"Expected digits {expected_digits}, got {actual_digits}")


if __name__ == "__main__":
    main()
