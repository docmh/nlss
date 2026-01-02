# SPDX-License-Identifier: Apache-2.0
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


def is_na(value):
    if value is None:
        return True
    if isinstance(value, str) and value.strip() == "":
        return True
    if isinstance(value, float) and math.isnan(value):
        return True
    return False


def sort_key(row):
    factor = row.get("factor")
    item = row.get("item")
    loading = row.get("loading")

    factor_na = is_na(factor)
    factor_key = "" if factor_na else str(factor)

    loading_na = is_na(loading)
    loading_val = 0.0
    if not loading_na:
        try:
            loading_val = float(loading)
        except (TypeError, ValueError):
            loading_na = True

    item_na = is_na(item)
    item_key = "" if item_na else str(item)

    return (factor_na, factor_key, loading_na, -abs(loading_val), item_na, item_key)


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


def approx_equal(a, b, tol=1e-6):
    if a is None or b is None:
        return False
    try:
        return abs(float(a) - float(b)) <= tol
    except (TypeError, ValueError):
        return False


def main():
    if len(sys.argv) < 3:
        fail("Usage: check_efa_log.py <log_path> <start_count> [key=value...]")

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
        if candidate.get("module") == "efa":
            entry = candidate
            break
    if entry is None:
        fail("No efa log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}

    loadings_df = results.get("loadings_df") or []
    summary_df = results.get("summary_df") or []
    eigen_df = results.get("eigen_df") or []

    if not loadings_df:
        fail("loadings_df is empty")
    if not summary_df:
        fail("summary_df is empty")
    if not eigen_df:
        fail("eigen_df is empty")

    load_cols = parse_list(extra.get("load_cols")) or [
        "item",
        "factor",
        "loading",
        "h2",
        "u2",
        "complexity",
        "cross_loading",
        "group",
    ]
    summary_cols = parse_list(extra.get("summary_cols")) or [
        "group",
        "n_obs",
        "n_items",
        "n_factors",
        "method",
        "rotation",
        "cor",
        "missing",
        "kmo",
        "bartlett_chi2",
        "bartlett_df",
        "bartlett_p",
        "variance_explained",
    ]
    eigen_cols = parse_list(extra.get("eigen_cols")) or [
        "group",
        "component",
        "eigenvalue",
        "proportion",
        "cumulative",
    ]

    ensure_columns(loadings_df, load_cols, "loadings_df")
    ensure_columns(summary_df, summary_cols, "summary_df")
    ensure_columns(eigen_df, eigen_cols, "eigen_df")

    min_load_rows = parse_int(extra.get("min_load_rows"))
    if min_load_rows is not None and len(loadings_df) < min_load_rows:
        fail(f"Expected at least {min_load_rows} loadings rows, got {len(loadings_df)}")

    min_summary_rows = parse_int(extra.get("min_summary_rows"))
    if min_summary_rows is not None and len(summary_df) < min_summary_rows:
        fail(f"Expected at least {min_summary_rows} summary rows, got {len(summary_df)}")

    min_groups = parse_int(extra.get("min_groups"))
    if min_groups is not None:
        groups = {str(row.get("group")) for row in summary_df if row.get("group") is not None}
        if len(groups) < min_groups:
            fail(f"Expected at least {min_groups} group(s), got {len(groups)}")

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

    expected_method = extra.get("method")
    if expected_method is not None:
        actual_method = options.get("method")
        if actual_method != expected_method:
            fail(f"Expected method {expected_method}, got {actual_method}")

    expected_rotation = extra.get("rotation")
    if expected_rotation is not None:
        actual_rotation = options.get("rotation")
        if actual_rotation != expected_rotation:
            fail(f"Expected rotation {expected_rotation}, got {actual_rotation}")

    expected_rule = extra.get("n_factors_rule")
    if expected_rule is not None:
        actual_rule = options.get("n_factors_rule")
        if actual_rule != expected_rule:
            fail(f"Expected n_factors_rule {expected_rule}, got {actual_rule}")

    expected_cor = extra.get("cor")
    if expected_cor is not None:
        actual_cor = options.get("cor")
        if actual_cor != expected_cor:
            fail(f"Expected cor {expected_cor}, got {actual_cor}")

    expected_missing = extra.get("missing")
    if expected_missing is not None:
        actual_missing = options.get("missing")
        if actual_missing != expected_missing:
            fail(f"Expected missing {expected_missing}, got {actual_missing}")

    expected_cutoff = parse_float(extra.get("loading_cutoff"))
    if expected_cutoff is not None:
        actual_cutoff = options.get("loading_cutoff")
        if not approx_equal(actual_cutoff, expected_cutoff, tol=1e-6):
            fail(f"Expected loading_cutoff {expected_cutoff}, got {actual_cutoff}")

    expected_eigen = parse_float(extra.get("eigen_threshold"))
    if expected_eigen is not None:
        actual_eigen = options.get("eigen_threshold")
        if not approx_equal(actual_eigen, expected_eigen, tol=1e-6):
            fail(f"Expected eigen_threshold {expected_eigen}, got {actual_eigen}")

    expected_digits = parse_int(extra.get("digits"))
    if expected_digits is not None:
        actual_digits = options.get("digits")
        if actual_digits != expected_digits:
            fail(f"Expected digits {expected_digits}, got {actual_digits}")

    expected_n_factors = parse_int(extra.get("n_factors"))
    if expected_n_factors is not None:
        actual_n_factors = options.get("n_factors")
        if actual_n_factors != expected_n_factors:
            fail(f"Expected n_factors {expected_n_factors}, got {actual_n_factors}")

    expected_sort = parse_bool(extra.get("sort_loadings"))
    if expected_sort is not None:
        actual_sort = options.get("sort_loadings")
        if actual_sort is None or bool(actual_sort) is not expected_sort:
            fail(f"Expected sort_loadings {expected_sort}, got {actual_sort}")

    expected_coerce = parse_bool(extra.get("coerce"))
    if expected_coerce is not None:
        actual_coerce = options.get("coerce")
        if actual_coerce is None or bool(actual_coerce) is not expected_coerce:
            fail(f"Expected coerce {expected_coerce}, got {actual_coerce}")

    check_sorted = parse_bool(extra.get("check_sorted")) or parse_bool(extra.get("sorted"))
    if check_sorted:
        indices = list(range(len(loadings_df)))
        sorted_indices = sorted(indices, key=lambda i: sort_key(loadings_df[i]))
        if sorted_indices != indices:
            fail("loadings_df is not sorted by factor and loading.")


if __name__ == "__main__":
    main()
