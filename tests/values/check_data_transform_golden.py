#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Compare complete transformed columns in a JSONL entry to base-R goldens."""
import csv
import json
import math
import sys
from pathlib import Path


def main():
    if len(sys.argv) != 5:
        raise SystemExit("Usage: check_data_transform_golden.py LOG START_COUNT GOLDEN_CSV CASE_ID")
    log, start, golden, case_id = sys.argv[1:]
    entries = [json.loads(line) for line in Path(log).read_text(encoding="utf-8").splitlines()[int(start):] if line.strip()]
    entries = [entry for entry in entries if entry.get("module") == "data_transform"]
    if not entries:
        raise AssertionError("No new data_transform entry")
    actual = entries[-1].get("results", {}).get("transformed_df")
    if not isinstance(actual, list) or not actual:
        raise AssertionError("transformed_df must retain full row-oriented numerical data")
    with Path(golden).open(encoding="utf-8", newline="") as handle:
        expected = [row for row in csv.DictReader(handle) if row["case_id"] == case_id]
    if not expected:
        raise AssertionError(f"No golden values for {case_id}")
    if len(actual) != max(int(row["row"]) for row in expected):
        raise AssertionError("Transformed row count differs")
    for row in expected:
        observed_row = actual[int(row["row"]) - 1]
        if row["variable"] not in observed_row:
            raise AssertionError(f"Missing transformed variable {row['variable']}")
        value = observed_row[row["variable"]]
        label = f"{case_id} row={row['row']} variable={row['variable']}"
        if row["value"] == "NA":
            if value is not None:
                raise AssertionError(f"Expected missing: {label}; got {value}")
        elif value is None or not math.isclose(float(value), float(row["value"]), rel_tol=1e-10, abs_tol=1e-10):
            raise AssertionError(f"Mismatch: {label}; expected {row['value']}, got {value}")
    print(f"[PASS] data_transform golden {case_id}: {len(expected)} independent values")


if __name__ == "__main__":
    main()
