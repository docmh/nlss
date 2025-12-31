#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_arg(value):
    if value in (None, "", "-"):
        return None
    return value


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
                entries.append(json.loads(line))
    return entries


def fail(message):
    print(message)
    sys.exit(1)


def main():
    if len(sys.argv) < 8:
        fail(
            "Usage: check_power_log.py <log_path> <start_count> <expected_status> "
            "<expected_analysis> <expected_mode> <expected_metric> <min_rows> "
            "[effect_source=...] [power_min=...] [n_total_min=...] [user_prompt=...] "
            "[t_type=...] [alternative=...] [ratio=...] [n1=...] [n2=...] [n_total=...] "
            "[n_per_group=...] [groups=...] [u=...] [df=...] [rmsea0=...] [rmsea1=...] "
            "[effect_size=...]"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    expected_status = sys.argv[3]
    expected_analysis = parse_arg(sys.argv[4])
    expected_mode = parse_arg(sys.argv[5])
    expected_metric = parse_arg(sys.argv[6])
    min_rows = parse_int(sys.argv[7]) or 1
    extra = parse_kv_args(sys.argv[8:])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = entries[-1]
    module = entry.get("module")
    if module != "power":
        fail(f"Expected module power, got {module}")

    results = entry.get("results", {}) or {}

    if expected_status not in (None, "", "-"):
        status = results.get("status")
        if status != expected_status:
            fail(f"Expected status {expected_status}, got {status}")
        return

    summary = results.get("summary_df") or []
    if not summary:
        fail("summary_df is empty")
    if len(summary) < min_rows:
        fail(f"Expected at least {min_rows} row(s), got {len(summary)}")

    row = summary[-1]

    if expected_analysis and row.get("analysis") != expected_analysis:
        fail(f"Expected analysis {expected_analysis}, got {row.get('analysis')}")
    if expected_mode and row.get("mode") != expected_mode:
        fail(f"Expected mode {expected_mode}, got {row.get('mode')}")
    if expected_metric and row.get("effect_metric") != expected_metric:
        fail(f"Expected effect_metric {expected_metric}, got {row.get('effect_metric')}")

    expect_user_prompt = parse_arg(extra.get("user_prompt"))
    if expect_user_prompt is not None:
        actual_prompt = entry.get("user_prompt")
        if actual_prompt != expect_user_prompt:
            fail(f"Expected user_prompt {expect_user_prompt}, got {actual_prompt}")

    expect_source = parse_arg(extra.get("effect_source"))
    if expect_source and row.get("effect_source") != expect_source:
        fail(f"Expected effect_source {expect_source}, got {row.get('effect_source')}")

    expect_t_type = parse_arg(extra.get("t_type"))
    if expect_t_type and row.get("t_type") != expect_t_type:
        fail(f"Expected t_type {expect_t_type}, got {row.get('t_type')}")

    expect_alternative = parse_arg(extra.get("alternative"))
    if expect_alternative and row.get("alternative") != expect_alternative:
        fail(f"Expected alternative {expect_alternative}, got {row.get('alternative')}")

    numeric_fields = {
        "ratio",
        "n1",
        "n2",
        "n_total",
        "n_per_group",
        "groups",
        "u",
        "df",
        "rmsea0",
        "rmsea1",
        "effect_size",
    }
    for field in numeric_fields:
        expected = parse_float(parse_arg(extra.get(field)))
        if expected is None:
            continue
        actual = row.get(field)
        if actual is None:
            fail(f"Expected {field}={expected}, got None")
        try:
            actual_val = float(actual)
        except (TypeError, ValueError):
            fail(f"Expected {field}={expected}, got {actual}")
        if abs(actual_val - expected) > 1e-6:
            fail(f"Expected {field}={expected}, got {actual_val}")

    power_min = parse_float(parse_arg(extra.get("power_min")))
    if power_min is not None:
        power_val = row.get("power")
        if power_val is None or float(power_val) < power_min:
            fail(f"Expected power >= {power_min}, got {power_val}")

    n_total_min = parse_float(parse_arg(extra.get("n_total_min")))
    if n_total_min is not None:
        n_total = row.get("n_total")
        if n_total is None or float(n_total) < n_total_min:
            fail(f"Expected n_total >= {n_total_min}, got {n_total}")


if __name__ == "__main__":
    main()
