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
        fail("Usage: check_scale_log.py <log_path> <start_count> [key=value...]")

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
        if candidate.get("module") == "scale":
            entry = candidate
            break
    if entry is None:
        fail("No scale log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}

    item_df = results.get("item_df") or []
    reliability_df = results.get("reliability_df") or []

    if not item_df:
        fail("item_df is empty")
    if not reliability_df:
        fail("reliability_df is empty")

    item_cols = parse_list(extra.get("item_cols")) or [
        "item",
        "group",
        "n",
        "missing_n",
        "missing_pct",
        "mean",
        "sd",
        "min",
        "max",
        "item_total_r",
        "item_rest_r",
        "alpha_if_deleted",
    ]
    rel_cols = parse_list(extra.get("rel_cols")) or [
        "group",
        "n_items",
        "n_total",
        "n_complete",
        "missing_n",
        "missing_pct",
        "alpha",
        "alpha_std",
        "omega_total",
        "omega_status",
        "r_bar",
        "r_min",
        "r_max",
        "score_method",
        "score_mean",
        "score_sd",
        "score_min",
        "score_max",
    ]
    ensure_columns(item_df, item_cols, "item_df")
    ensure_columns(reliability_df, rel_cols, "reliability_df")

    min_item_rows = parse_int(extra.get("min_item_rows"))
    if min_item_rows is not None and len(item_df) < min_item_rows:
        fail(f"Expected at least {min_item_rows} item_df rows, got {len(item_df)}")

    min_rel_rows = parse_int(extra.get("min_rel_rows"))
    if min_rel_rows is not None and len(reliability_df) < min_rel_rows:
        fail(f"Expected at least {min_rel_rows} reliability_df rows, got {len(reliability_df)}")

    min_groups = parse_int(extra.get("min_groups"))
    if min_groups is not None:
        groups = {str(row.get("group")) for row in reliability_df if row.get("group") is not None}
        if len(groups) < min_groups:
            fail(f"Expected at least {min_groups} group(s), got {len(groups)}")

    expected_vars = parse_list(extra.get("vars"))
    if expected_vars is not None:
        actual_vars = normalize_list(options.get("vars"))
        if actual_vars != expected_vars:
            fail(f"Expected vars {expected_vars}, got {actual_vars}")

    expected_reverse = parse_list(extra.get("reverse"))
    if expected_reverse is not None:
        actual_reverse = normalize_list(options.get("reverse"))
        if actual_reverse != expected_reverse:
            fail(f"Expected reverse {expected_reverse}, got {actual_reverse}")

    expected_group = extra.get("group")
    if expected_group is not None:
        actual_group = options.get("group")
        if expected_group in ("-", "", "none", "None"):
            if actual_group not in (None, "", "None"):
                fail(f"Expected group to be empty, got {actual_group}")
        else:
            if actual_group != expected_group:
                fail(f"Expected group {expected_group}, got {actual_group}")

    expected_missing = extra.get("missing")
    if expected_missing is not None:
        actual_missing = options.get("missing")
        if actual_missing != expected_missing:
            fail(f"Expected missing {expected_missing}, got {actual_missing}")

    expected_score = extra.get("score")
    if expected_score is not None:
        actual_score = options.get("score")
        if actual_score != expected_score:
            fail(f"Expected score {expected_score}, got {actual_score}")

    expected_score_method = extra.get("score_method")
    if expected_score_method is not None:
        if not any(row.get("score_method") == expected_score_method for row in reliability_df):
            fail(f"Expected score_method {expected_score_method} in reliability_df")

    expected_omega = parse_bool(extra.get("omega"))
    if expected_omega is not None:
        actual_omega = options.get("omega")
        if actual_omega is None or bool(actual_omega) != expected_omega:
            fail(f"Expected omega {expected_omega}, got {actual_omega}")

    expected_coerce = parse_bool(extra.get("coerce"))
    if expected_coerce is not None:
        actual_coerce = options.get("coerce")
        if actual_coerce is None or bool(actual_coerce) != expected_coerce:
            fail(f"Expected coerce {expected_coerce}, got {actual_coerce}")

    expected_digits = parse_int(extra.get("digits"))
    if expected_digits is not None:
        actual_digits = options.get("digits")
        if actual_digits != expected_digits:
            fail(f"Expected digits {expected_digits}, got {actual_digits}")

    expected_omega_status = extra.get("omega_status")
    if expected_omega_status is not None:
        if not any(row.get("omega_status") == expected_omega_status for row in reliability_df):
            fail(f"Expected omega_status {expected_omega_status} in reliability_df")

    expected_user_prompt = extra.get("user_prompt")
    if expected_user_prompt is not None:
        actual_prompt = entry.get("user_prompt")
        if actual_prompt != expected_user_prompt:
            fail(f"Expected user_prompt {expected_user_prompt}, got {actual_prompt}")


if __name__ == "__main__":
    main()
