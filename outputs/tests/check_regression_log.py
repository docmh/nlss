#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_int(value):
    if value in (None, "", "-"):
        return None
    return int(value)


def parse_bool(value):
    if value in (None, "", "-"):
        return None
    return value.lower() in ("1", "true", "yes", "y")


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
    if len(sys.argv) != 14:
        fail(
            "Usage: check_regression_log.py <log_path> <start_count> <expected_status> "
            "<expected_family> <expected_stat_label> <min_models> <min_groups> "
            "<min_comparisons> <expect_beta> <expect_expb> <expect_boot> "
            "<expect_interaction> <expect_diagnostics>"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    expected_status = sys.argv[3]
    expected_family = sys.argv[4]
    expected_stat_label = sys.argv[5]
    min_models = parse_int(sys.argv[6])
    min_groups = parse_int(sys.argv[7])
    min_comparisons = parse_int(sys.argv[8])
    expect_beta = parse_bool(sys.argv[9])
    expect_expb = parse_bool(sys.argv[10])
    expect_boot = parse_bool(sys.argv[11])
    expect_interaction = parse_bool(sys.argv[12])
    expect_diagnostics = parse_bool(sys.argv[13])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = entries[-1]
    module = entry.get("module")
    if module != "regression":
        fail(f"Expected module regression, got {module}")

    results = entry.get("results", {}) or {}

    if expected_status not in (None, "", "-"):
        status = results.get("status")
        if status != expected_status:
            fail(f"Expected status {expected_status}, got {status}")
        return

    coefficients = results.get("coefficients_df") or []
    summary = results.get("summary_df") or []
    comparisons = results.get("comparisons_df") or []
    diagnostics = results.get("diagnostics_df") or []

    if not coefficients:
        fail("coefficients_df is empty")
    if not summary:
        fail("summary_df is empty")

    if expected_family not in (None, "", "-"):
        family = entry.get("options", {}).get("family")
        if family != expected_family:
            fail(f"Expected family {expected_family}, got {family}")

    if expected_stat_label not in (None, "", "-"):
        if not any(row.get("stat_label") == expected_stat_label for row in coefficients):
            fail(f"Expected stat_label {expected_stat_label} in coefficients_df")

    if min_models is not None:
        models = {row.get("model") for row in summary if row.get("model") is not None}
        if len(models) < min_models:
            fail(f"Expected at least {min_models} model(s), got {len(models)}")

    if min_groups is not None:
        groups = {row.get("group") for row in summary if row.get("group") not in (None, "")}
        if len(groups) < min_groups:
            fail(f"Expected at least {min_groups} group(s), got {len(groups)}")

    if min_comparisons is not None:
        if len(comparisons) < min_comparisons:
            fail(f"Expected at least {min_comparisons} comparison(s), got {len(comparisons)}")

    if expect_beta is True:
        if not any(row.get("beta") is not None for row in coefficients):
            fail("Expected non-null beta values")

    if expect_expb is True:
        if not any(row.get("exp_b") is not None for row in coefficients):
            fail("Expected non-null exp_b values")

    if expect_boot is True:
        if not any(row.get("boot_ci_low") is not None for row in coefficients):
            fail("Expected non-null bootstrap CI values")

    if expect_interaction is True:
        if not any(
            isinstance(row.get("term"), str) and (":" in row.get("term") or "*" in row.get("term"))
            for row in coefficients
        ):
            fail("Expected interaction term in coefficients_df")

    if expect_diagnostics is True:
        if not diagnostics:
            fail("Expected diagnostics_df entries")

    if expect_diagnostics is False:
        if diagnostics:
            fail("Expected diagnostics_df to be empty")


if __name__ == "__main__":
    main()
