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


def parse_list(value):
    if value in (None, "", "-"):
        return []
    return [item.strip() for item in value.split(",") if item.strip()]


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
    if len(sys.argv) != 12:
        fail(
            "Usage: check_sem_log.py <log_path> <start_count> <expected_status> "
            "<expected_analysis> <mode> <min_params> <fit_keys> <expect_modindices> "
            "<expect_residuals> <expect_bootstrap> <expect_group>"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    expected_status = sys.argv[3]
    expected_analysis = sys.argv[4]
    mode = sys.argv[5]
    min_params = parse_int(sys.argv[6])
    fit_keys = parse_list(sys.argv[7])
    expect_modindices = parse_bool(sys.argv[8])
    expect_residuals = parse_bool(sys.argv[9])
    expect_bootstrap = parse_bool(sys.argv[10])
    expect_group = parse_bool(sys.argv[11])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "sem":
            entry = candidate
            break
    if entry is None:
        fail("No sem log entry found.")

    results = entry.get("results", {}) or {}
    options = entry.get("options", {}) or {}
    status = results.get("status")

    if expected_status not in (None, "", "-"):
        if status != expected_status:
            fail(f"Expected status {expected_status}, got {status}")
        if expected_status != "ok":
            return
    else:
        if status != "ok":
            fail(f"Expected status ok, got {status}")

    analysis = results.get("analysis") or options.get("analysis")
    if expected_analysis not in (None, "", "-"):
        if analysis != expected_analysis:
            fail(f"Expected analysis {expected_analysis}, got {analysis}")

    if mode == "invariance":
        fit = results.get("fit")
        if not isinstance(fit, list) or len(fit) < 2:
            fail("Expected invariance fit table with at least 2 rows")
        if fit_keys:
            first = fit[0] or {}
            for key in fit_keys:
                if key not in first:
                    fail(f"Expected fit key '{key}' in invariance table")
    else:
        fit = results.get("fit")
        if fit_keys:
            if not isinstance(fit, dict):
                fail("Expected fit metrics dict")
            for key in fit_keys:
                if key not in fit:
                    fail(f"Expected fit key '{key}'")
        if min_params is not None:
            params = results.get("params", {}) or {}
            rows = params.get("rows")
            if rows is None or rows < min_params:
                fail(f"Expected at least {min_params} params rows, got {rows}")

    if expect_modindices is True:
        cutoff = options.get("modindices_cutoff")
        if not cutoff or cutoff <= 0:
            fail("Expected modindices_cutoff > 0")
        if results.get("modindices") is None:
            fail("Expected modindices count in results")

    if expect_residuals is True:
        if options.get("residuals_output") in (None, {}):
            fail("Expected residuals_output in options")
    elif expect_residuals is False:
        if options.get("residuals_output") not in (None, {}):
            fail("Did not expect residuals_output")

    if expect_bootstrap is True:
        if not options.get("bootstrap"):
            fail("Expected bootstrap TRUE")
        if options.get("se") != "bootstrap":
            fail("Expected se=bootstrap")
    elif expect_bootstrap is False:
        if options.get("bootstrap"):
            fail("Did not expect bootstrap")

    if expect_group is True:
        if not options.get("group"):
            fail("Expected group option")
    elif expect_group is False:
        if options.get("group"):
            fail("Did not expect group option")


if __name__ == "__main__":
    main()
