# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_int(value):
    if value in (None, "", "-"):
        return None
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def parse_bool(value):
    if value in (None, "", "-"):
        return None
    val = str(value).strip().lower()
    if val in ("1", "true", "yes", "y", "t"):
        return True
    if val in ("0", "false", "no", "n", "f"):
        return False
    return None


def parse_list(value):
    if value in (None, "", "-"):
        return []
    items = [item.strip() for item in str(value).split(",")]
    return [item for item in items if item]


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


def main():
    if len(sys.argv) < 8:
        fail(
            "Usage: check_impute_log.py <log_path> <start_count> <min_rows> <expected_vars> "
            "<expected_engine> <expect_indicator> <expect_backup> [option_key=value ...]"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    min_rows = parse_int(sys.argv[3])
    expected_vars = parse_list(sys.argv[4])
    expected_engine = sys.argv[5]
    expect_indicator = parse_bool(sys.argv[6])
    expect_backup = parse_bool(sys.argv[7])
    extra = parse_kv_args(sys.argv[8:])
    method_expectations = {}
    user_prompt_expected = None
    for key in list(extra.keys()):
        if key.startswith("method."):
            method_expectations[key.split(".", 1)[1]] = extra.pop(key)
        elif key in ("user_prompt", "user-prompt"):
            user_prompt_expected = extra.pop(key)

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "impute":
            entry = candidate
            break
    if entry is None:
        fail("No impute log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}
    summary_rows = results.get("summary_df") or []
    if not isinstance(summary_rows, list):
        summary_rows = []

    if user_prompt_expected not in (None, "", "-"):
        actual_prompt = entry.get("user_prompt") or ""
        if actual_prompt != user_prompt_expected:
            fail(f"Expected user_prompt '{user_prompt_expected}', got '{actual_prompt}'")

    if min_rows is not None and len(summary_rows) < min_rows:
        fail(f"Expected at least {min_rows} rows, got {len(summary_rows)}")

    imputed_vars = results.get("imputed_vars") or []
    if not isinstance(imputed_vars, list):
        imputed_vars = []
    if not imputed_vars and summary_rows:
        imputed_vars = [
            row.get("target")
            for row in summary_rows
            if isinstance(row, dict) and row.get("target")
        ]

    if expected_vars:
        missing = [var for var in expected_vars if var not in imputed_vars]
        if missing:
            fail(f"Expected imputed vars {missing}, got {imputed_vars}")

    if expected_engine not in (None, "", "-"):
        engine = results.get("engine") or options.get("engine") or options.get("engine_used")
        if engine != expected_engine:
            fail(f"Expected engine '{expected_engine}', got '{engine}'")

    indicator_vars = results.get("indicator_vars") or []
    if not isinstance(indicator_vars, list):
        indicator_vars = []

    if expect_indicator is True and not indicator_vars:
        fail("Expected indicator_vars in results")
    if expect_indicator is False and indicator_vars:
        fail("Did not expect indicator_vars")

    if expect_backup is True:
        backup_path = results.get("backup_path")
        if not backup_path:
            fail("Expected backup_path in results")
        if not Path(backup_path).exists():
            fail(f"Missing backup file: {backup_path}")
    elif expect_backup is False:
        if results.get("backup_path"):
            fail("Did not expect backup_path")

    output_path = results.get("output_path")
    if output_path and not Path(output_path).exists():
        fail(f"Missing output file: {output_path}")

    if method_expectations:
        method_map = {}
        for row in summary_rows:
            if not isinstance(row, dict):
                continue
            var_name = row.get("variable")
            if var_name:
                method_map[var_name] = row.get("method")
        for var_name, expected in method_expectations.items():
            if expected in ("", "-", None):
                continue
            actual = method_map.get(var_name)
            if actual is None:
                fail(f"Missing method for variable '{var_name}'")
            if str(actual).lower() != str(expected).lower():
                fail(f"Expected method for '{var_name}' = '{expected}', got '{actual}'")

    for key, expected in extra.items():
        if expected in ("", "-", None):
            continue
        actual = options.get(key)
        expected_bool = parse_bool(expected)
        if isinstance(actual, bool) and expected_bool is not None:
            if actual != expected_bool:
                fail(f"Expected {key}={expected_bool}, got {actual}")
            continue
        actual_text = "" if actual is None else str(actual)
        if actual_text != expected:
            fail(f"Expected {key}='{expected}', got '{actual_text}'")


if __name__ == "__main__":
    main()
