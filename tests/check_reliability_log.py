#!/usr/bin/env python3
import json
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


def parse_list(value):
    if value in (None, "", "-"):
        return None
    items = [item.strip() for item in str(value).split(",")]
    items = [item for item in items if item]
    return items or None


def parse_float(value):
    if value in (None, "", "-"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


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


def main():
    if len(sys.argv) < 3:
        fail("Usage: check_reliability_log.py <log_path> <start_count> [key=value...]")

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
        if candidate.get("module") == "reliability":
            entry = candidate
            break
    if entry is None:
        fail("No reliability log entry found.")

    results = entry.get("results", {}) or {}
    status = results.get("status")
    expected_status = extra.get("status")
    if expected_status is not None:
        if status != expected_status:
            fail(f"Expected status {expected_status}, got {status}")
        print("status ok")
        return

    summary_df = results.get("summary_df") or []
    if not summary_df:
        fail("summary_df is empty")

    expected_cols = parse_list(extra.get("cols")) or [
        "analysis",
        "analysis_label",
        "method_label",
        "estimate",
        "n",
        "missing_n",
        "missing_pct",
    ]
    ensure_columns(summary_df, expected_cols, "summary_df")

    min_rows = parse_int(extra.get("min_rows"))
    if min_rows is not None and len(summary_df) < min_rows:
        fail(f"Expected at least {min_rows} summary rows, got {len(summary_df)}")

    expected_analysis = extra.get("analysis")
    if expected_analysis is not None:
        actual = {str(row.get("analysis")) for row in summary_df if row.get("analysis") is not None}
        if expected_analysis not in actual:
            fail(f"Expected analysis {expected_analysis}, got {', '.join(sorted(actual))}")

    min_groups = parse_int(extra.get("min_groups"))
    if min_groups is not None:
        groups = {str(row.get("group")) for row in summary_df if row.get("group") is not None}
        if len(groups) < min_groups:
            fail(f"Expected at least {min_groups} group(s), got {len(groups)}")

    options = entry.get("options", {}) or {}

    expected_vars = parse_list(extra.get("vars"))
    if expected_vars is not None:
        actual_vars = normalize_list(options.get("vars"))
        if actual_vars != expected_vars:
            fail(f"Expected vars {expected_vars}, got {actual_vars}")

    expected_format = extra.get("format")
    if expected_format is not None:
        actual_format = options.get("format")
        if actual_format != expected_format:
            fail(f"Expected format {expected_format}, got {actual_format}")

    expected_missing = extra.get("missing")
    if expected_missing is not None:
        actual_missing = options.get("missing")
        if actual_missing != expected_missing:
            fail(f"Expected missing {expected_missing}, got {actual_missing}")

    expected_icc_model = extra.get("icc_model")
    if expected_icc_model is not None:
        actual_icc_model = options.get("icc_model")
        if actual_icc_model != expected_icc_model:
            fail(f"Expected icc_model {expected_icc_model}, got {actual_icc_model}")

    expected_icc_type = extra.get("icc_type")
    if expected_icc_type is not None:
        actual_icc_type = options.get("icc_type")
        if actual_icc_type != expected_icc_type:
            fail(f"Expected icc_type {expected_icc_type}, got {actual_icc_type}")

    expected_icc_unit = extra.get("icc_unit")
    if expected_icc_unit is not None:
        actual_icc_unit = options.get("icc_unit")
        if actual_icc_unit != expected_icc_unit:
            fail(f"Expected icc_unit {expected_icc_unit}, got {actual_icc_unit}")

    expected_kappa_weight = extra.get("kappa_weight")
    if expected_kappa_weight is not None:
        actual_kappa_weight = options.get("kappa_weight")
        if actual_kappa_weight != expected_kappa_weight:
            fail(f"Expected kappa_weight {expected_kappa_weight}, got {actual_kappa_weight}")

    expected_method = extra.get("method")
    if expected_method is not None:
        actual_method = options.get("method")
        if actual_method != expected_method:
            fail(f"Expected method {expected_method}, got {actual_method}")

    expected_conf_level = parse_float(extra.get("conf_level"))
    if expected_conf_level is not None:
        actual_conf_level = options.get("conf_level")
        try:
            actual_conf_level = float(actual_conf_level)
        except (TypeError, ValueError):
            actual_conf_level = None
        if actual_conf_level is None or abs(actual_conf_level - expected_conf_level) > 1e-6:
            fail(f"Expected conf_level {expected_conf_level}, got {actual_conf_level}")

    expected_coerce = parse_bool(extra.get("coerce"))
    if expected_coerce is not None:
        actual_coerce = options.get("coerce")
        if actual_coerce is not expected_coerce:
            fail(f"Expected coerce {expected_coerce}, got {actual_coerce}")

    expected_group = extra.get("group")
    if expected_group is not None:
        actual_group = options.get("group")
        if expected_group in ("-", "", "none", "None"):
            if actual_group not in (None, "", "None"):
                fail(f"Expected group to be empty, got {actual_group}")
        else:
            if actual_group != expected_group:
                fail(f"Expected group {expected_group}, got {actual_group}")

    expected_id = extra.get("id")
    if expected_id is not None:
        actual_id = options.get("id")
        if expected_id in ("-", "", "none", "None"):
            if actual_id not in (None, "", "None"):
                fail(f"Expected id to be empty, got {actual_id}")
        else:
            if actual_id != expected_id:
                fail(f"Expected id {expected_id}, got {actual_id}")

    expected_rater = extra.get("rater")
    if expected_rater is not None:
        actual_rater = options.get("rater")
        if expected_rater in ("-", "", "none", "None"):
            if actual_rater not in (None, "", "None"):
                fail(f"Expected rater to be empty, got {actual_rater}")
        else:
            if actual_rater != expected_rater:
                fail(f"Expected rater {expected_rater}, got {actual_rater}")

    expected_score = extra.get("score")
    if expected_score is not None:
        actual_score = options.get("score")
        if expected_score in ("-", "", "none", "None"):
            if actual_score not in (None, "", "None"):
                fail(f"Expected score to be empty, got {actual_score}")
        else:
            if actual_score != expected_score:
                fail(f"Expected score {expected_score}, got {actual_score}")


if __name__ == "__main__":
    main()
