# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_arg(value):
    if value in ("", "-", "none", "None"):
        return None
    return value


def parse_expectation(value):
    if value is None:
        return None
    return str(value).strip().lower()


def match_numeric(value, expectation):
    exp = parse_expectation(expectation)
    if exp is None:
        return True
    if exp in ("present", "any"):
        return value is not None
    if exp in ("absent", "none"):
        return value is None
    try:
        num = float(value)
    except (TypeError, ValueError):
        return False
    if exp in ("gt0", "greater_than_zero"):
        return num > 0
    if exp in ("ge0", "gte0", "greater_equal_zero"):
        return num >= 0
    try:
        target = float(exp)
    except (TypeError, ValueError):
        return True
    return abs(num - target) <= 1e-6


def to_group(value):
    if value is None:
        return "NA"
    return str(value)


if len(sys.argv) < 9:
    sys.stderr.write(
        "Usage: check_crosstabs_log.py LOG START ROW COL GROUP PHI CRAMERS CONTINGENCY\n"
    )
    sys.exit(2)

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
expect_row = parse_arg(sys.argv[3])
expect_col = parse_arg(sys.argv[4])
expect_group = parse_arg(sys.argv[5])
expect_phi = parse_arg(sys.argv[6])
expect_cramers = parse_arg(sys.argv[7])
expect_contingency = parse_arg(sys.argv[8])

if not log_path.exists():
    sys.exit(1)

index = 0
found = False
with log_path.open("r", encoding="utf-8") as handle:
    for line in handle:
        if not line.strip():
            continue
        index += 1
        if index <= start_count:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if entry.get("module") != "crosstabs":
            continue
        results = entry.get("results", {})
        tests_df = results.get("tests_df")
        if not isinstance(tests_df, list) or len(tests_df) == 0:
            continue
        for row in tests_df:
            if not isinstance(row, dict):
                continue
            row_var = row.get("row_var")
            col_var = row.get("col_var")
            group = to_group(row.get("group"))
            if expect_row is not None and row_var != expect_row:
                continue
            if expect_col is not None and col_var != expect_col:
                continue
            if expect_group is not None and group != to_group(expect_group):
                continue
            if not match_numeric(row.get("phi"), expect_phi):
                continue
            if not match_numeric(row.get("cramers_v"), expect_cramers):
                continue
            if not match_numeric(row.get("contingency_c"), expect_contingency):
                continue
            found = True
            break
        if found:
            break

sys.exit(0 if found else 1)
