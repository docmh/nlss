# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import math
import sys
from pathlib import Path


def parse_expected(raw):
    if raw is None:
        return None
    text = str(raw).strip()
    if text == "" or text == "-":
        return None
    lowered = text.lower()
    if lowered in ("none", "null"):
        return None
    if lowered in ("true", "false"):
        return lowered == "true"
    try:
        return float(text)
    except ValueError:
        return text


def parse_expectations(args):
    expectations = {}
    for arg in args:
        if "=" not in arg:
            continue
        key, value = arg.split("=", 1)
        key = key.strip()
        value = value.strip()
        if key:
            expectations[key] = value
    return expectations


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if not line.strip():
                continue
            try:
                entries.append(json.loads(line))
            except json.JSONDecodeError:
                continue
    return entries


def get_path(obj, parts):
    cur = obj
    for part in parts:
        if isinstance(cur, dict) and part in cur:
            cur = cur[part]
        else:
            return None
    return cur


def match_value(actual, expected):
    if expected is None:
        return actual is None
    if isinstance(expected, bool):
        if isinstance(actual, bool):
            return actual == expected
        if isinstance(actual, str):
            lowered = actual.strip().lower()
            if lowered in ("true", "false"):
                return (lowered == "true") == expected
        return bool(actual) == expected
    if isinstance(expected, float):
        if actual is None:
            return False
        try:
            actual_num = float(actual)
        except (TypeError, ValueError):
            return False
        return math.isclose(actual_num, expected, rel_tol=1e-6, abs_tol=1e-6)
    if isinstance(actual, list):
        expected_items = [item.strip() for item in str(expected).split(",") if item.strip()]
        actual_items = [str(item) for item in actual if item is not None]
        if not expected_items:
            return not actual_items
        return all(item in actual_items for item in expected_items)
    return actual == expected


def fail(message):
    print(message)
    sys.exit(1)


def main():
    if len(sys.argv) < 5:
        fail("Usage: check_nonparametric_log.py LOG START STATUS MODE [KEY=VALUE...]")

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    expected_status = sys.argv[3]
    expected_mode = sys.argv[4]
    expectations = parse_expectations(sys.argv[5:])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "nonparametric":
            entry = candidate
            break
    if entry is None:
        fail("No nonparametric log entry found.")

    results = entry.get("results") or {}
    options = entry.get("options") or {}

    status_key = (expected_status or "").strip().lower()
    ok_status = status_key in ("", "-", "ok", "none", "null")
    if not ok_status:
        if results.get("status") != expected_status:
            fail(f"Expected status {expected_status}, got {results.get('status')}")
    else:
        summary_df = results.get("summary_df")
        if not isinstance(summary_df, list) or len(summary_df) == 0:
            fail("Expected non-empty summary_df for ok run")

    mode_key = (expected_mode or "").strip().lower()
    if mode_key not in ("", "-", "none", "null"):
        if options.get("mode") != expected_mode:
            fail(f"Expected mode {expected_mode}, got {options.get('mode')}")

    for key, raw_value in expectations.items():
        expected = parse_expected(raw_value)
        if key == "user_prompt":
            actual = entry.get("user_prompt")
        elif "." in key:
            actual = get_path(entry, key.split("."))
        else:
            if key in options:
                actual = options.get(key)
            else:
                actual = entry.get(key)
        if not match_value(actual, expected):
            fail(f"Mismatch for {key}: expected {expected!r}, got {actual!r}")


if __name__ == "__main__":
    main()
