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
    if len(sys.argv) < 6:
        fail(
            "Usage: check_data_transform_log.py <log_path> <start_count> <min_steps> "
            "<expected_actions> <expect_backup> [option_key=value ...]"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    min_steps = parse_int(sys.argv[3])
    expected_actions = parse_list(sys.argv[4])
    expect_backup = parse_bool(sys.argv[5])
    extra = parse_kv_args(sys.argv[6:])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "data_transform":
            entry = candidate
            break
    if entry is None:
        fail("No data_transform log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}
    log_rows = results.get("transform_log_df") or []
    if not isinstance(log_rows, list):
        log_rows = []

    if min_steps is not None and len(log_rows) < min_steps:
        fail(f"Expected at least {min_steps} log rows, got {len(log_rows)}")

    if expected_actions:
        actions = [
            row.get("action")
            for row in log_rows
            if isinstance(row, dict) and row.get("action")
        ]
        missing = [action for action in expected_actions if action not in actions]
        if missing:
            fail(f"Expected actions {missing}, got {actions}")

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
