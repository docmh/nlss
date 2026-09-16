#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Check legacy impute JSONL against independently generated base-R cells."""
import csv
import json
import math
import sys
from pathlib import Path


def main():
    if len(sys.argv) != 5:
        raise SystemExit("Usage: check_impute_values.py LOG START_COUNT GOLDEN_CSV CASE_ID")
    log, start, golden, case_id = sys.argv[1:]
    entries = [json.loads(line) for line in Path(log).read_text(encoding="utf-8").splitlines()[int(start):] if line.strip()]
    entries = [entry for entry in entries if entry.get("module") == "impute"]
    if not entries:
        raise AssertionError("No new impute JSONL entry")
    result = entries[-1]["results"]
    with Path(golden).open(encoding="utf-8", newline="") as handle:
        expected = [row for row in csv.DictReader(handle) if row["case_id"] == case_id]
    if not expected:
        raise AssertionError(f"No golden cells for {case_id}")
    for table in {row["table"] for row in expected} - {"metrics"}:
        value = result.get(table)
        if not isinstance(value, list) or len(value) != max(int(row["row"]) for row in expected if row["table"] == table):
            raise AssertionError(f"Wrong row count for {table}")
    for row in expected:
        owner = result if row["table"] == "metrics" else result[row["table"]][int(row["row"]) - 1]
        if row["key"] not in owner:
            raise AssertionError(f"Missing key {row['table']}.{row['key']}")
        actual = owner[row["key"]]
        label = f"{case_id} {row['table']} row={row['row']} key={row['key']}"
        if row["kind"] == "missing":
            if actual is not None:
                raise AssertionError(f"Expected null {label}, got {actual!r}")
        elif row["kind"] == "number":
            if actual is None or not math.isclose(float(actual), float(row["value"]), rel_tol=1e-10, abs_tol=1e-10):
                raise AssertionError(f"Mismatch {label}: expected {row['value']}, got {actual!r}")
        elif str(actual) != row["value"]:
            raise AssertionError(f"Mismatch {label}: expected {row['value']!r}, got {actual!r}")
    print(f"[PASS] impute golden {case_id}: {len(expected)} independent cells")


if __name__ == "__main__":
    main()
