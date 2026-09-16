#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Compare a public legacy JSONL entry with independent base-R frequency goldens."""
import csv
import json
import math
import sys
from pathlib import Path


def main():
    if len(sys.argv) != 4:
        raise ValueError("Usage: check_frequencies_golden.py LOG GOLDEN CASE_ID")
    log, golden, case = sys.argv[1:]
    entries = [json.loads(line) for line in Path(log).read_text().splitlines() if line.strip()]
    actual = next(entry for entry in reversed(entries) if entry.get("module") == "frequencies")["results"]["summary_df"]
    expected = [row for row in csv.DictReader(Path(golden).open()) if row["case_id"] == case]
    if len(actual) != len(expected):
        raise AssertionError(f"Frequency row count: {len(actual)} != {len(expected)}")
    key = lambda row: tuple(str(row.get(field) or "") for field in ("variable", "group", "level"))
    indexed = {key(row): row for row in actual}
    if len(indexed) != len(actual):
        raise AssertionError("Duplicate frequency row keys")
    checks = 0
    for wanted in expected:
        got = indexed[key(wanted)]
        for field in ("n", "pct_total", "pct_valid", "total_n", "missing_n", "missing_pct"):
            exp = None if wanted[field] == "" else float(wanted[field])
            value = got[field]
            if exp is None:
                assert value is None, (key(wanted), field, value, exp)
            else:
                assert value is not None and math.isclose(float(value), exp, rel_tol=1e-10, abs_tol=1e-8), (key(wanted), field, value, exp)
            checks += 1
    print(f"Frequency {case}: {checks} independent numeric comparisons passed")


if __name__ == "__main__":
    main()
