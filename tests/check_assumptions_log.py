# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def parse_arg(value):
    if value in (None, "", "-", "none", "None"):
        return None
    return value


def parse_int(value):
    if value in (None, "", "-", "none", "None"):
        return None
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def parse_float(value):
    if value in (None, "", "-", "none", "None"):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def parse_bool(value):
    if value in (None, "", "-", "none", "None"):
        return None
    val = str(value).strip().lower()
    if val in ("1", "true", "yes", "y", "t"):
        return True
    if val in ("0", "false", "no", "n", "f"):
        return False
    return None


def parse_list(value):
    if value in (None, "", "-", "none", "None"):
        return []
    return [item.strip() for item in str(value).split(",") if item.strip()]


def listify(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(item) for item in value if item is not None]
    return [str(value)]


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


def expect_list_contains(actual, expected, label):
    if not expected:
        return
    actual_lower = {item.lower() for item in actual}
    missing = [item for item in expected if item.lower() not in actual_lower]
    if missing:
        fail(f"Expected {label} to include {missing}, got {sorted(actual)}")


def expect_list_absent(actual, expected, label):
    if not expected:
        return
    actual_lower = {item.lower() for item in actual}
    present = [item for item in expected if item.lower() in actual_lower]
    if present:
        fail(f"Expected {label} to exclude {present}, got {sorted(actual)}")


def compare_float(actual, expected, label):
    if expected is None:
        return
    try:
        actual_val = float(actual)
    except (TypeError, ValueError):
        fail(f"Expected {label}={expected}, got {actual}")
    if abs(actual_val - expected) > 1e-6:
        fail(f"Expected {label}={expected}, got {actual_val}")


def compare_int(actual, expected, label):
    if expected is None:
        return
    try:
        actual_val = int(float(actual))
    except (TypeError, ValueError):
        fail(f"Expected {label}={expected}, got {actual}")
    if actual_val != expected:
        fail(f"Expected {label}={expected}, got {actual_val}")


def compare_str(actual, expected, label):
    if expected is None:
        return
    if actual is None or str(actual).lower() != str(expected).lower():
        fail(f"Expected {label}={expected}, got {actual}")


def compare_bool(actual, expected, label):
    if expected is None:
        return
    if actual is None:
        fail(f"Expected {label}={expected}, got {actual}")
    if bool(actual) is not expected:
        fail(f"Expected {label}={expected}, got {actual}")


def main():
    if len(sys.argv) < 6:
        fail(
            "Usage: check_assumptions_log.py <log_path> <start_count> <expected_analysis> "
            "<expected_mode> <min_rows> [KEY=VALUE...]"
        )

    log_path = Path(sys.argv[1])
    start_count = int(sys.argv[2])
    expected_analysis = parse_arg(sys.argv[3])
    expected_mode = parse_arg(sys.argv[4])
    min_rows_arg = sys.argv[5]
    extra = parse_kv_args(sys.argv[6:])

    if not log_path.exists():
        fail(f"Missing log: {log_path}")

    entries = load_entries(log_path, start_count)
    if not entries:
        fail("No new log entries found.")

    entry = None
    for candidate in reversed(entries):
        if candidate.get("module") == "assumptions":
            entry = candidate
            break
    if entry is None:
        fail("No assumptions log entry found.")

    results = entry.get("results") or {}
    options = entry.get("options") or {}
    checks = results.get("checks_df")
    if not isinstance(checks, list):
        fail("checks_df missing or not a list")

    analysis = options.get("analysis")
    if expected_analysis is not None and analysis != expected_analysis:
        fail(f"Expected analysis {expected_analysis}, got {analysis}")

    mode = options.get("mode")
    if expected_mode is not None and mode != expected_mode:
        fail(f"Expected mode {expected_mode}, got {mode}")

    if min_rows_arg not in (None, "", "-", "none", "None"):
        if min_rows_arg == "gt0":
            if len(checks) <= 0:
                fail("Expected checks_df to have >0 rows")
        else:
            min_rows = parse_int(min_rows_arg)
            if min_rows is not None and len(checks) < min_rows:
                fail(f"Expected checks_df >= {min_rows}, got {len(checks)}")

    assumptions = {row.get("assumption") for row in checks if isinstance(row, dict) and row.get("assumption")}
    tests = {row.get("test") for row in checks if isinstance(row, dict) and row.get("test")}
    models = {row.get("model") for row in checks if isinstance(row, dict) and row.get("model")}
    targets = {row.get("target") for row in checks if isinstance(row, dict) and row.get("target")}
    analysis_types = {row.get("analysis_type") for row in checks if isinstance(row, dict) and row.get("analysis_type")}

    if expected_analysis is not None and analysis_types:
        if any(item != expected_analysis for item in analysis_types):
            fail(f"Expected analysis_type {expected_analysis}, got {sorted(analysis_types)}")

    expect_list_contains(assumptions, parse_list(extra.get("assumptions")), "assumptions")
    expect_list_absent(assumptions, parse_list(extra.get("assumptions_absent")), "assumptions")
    expect_list_contains(tests, parse_list(extra.get("tests")), "tests")
    expect_list_absent(tests, parse_list(extra.get("tests_absent")), "tests")
    expect_list_contains(models, parse_list(extra.get("models")), "models")
    expect_list_absent(models, parse_list(extra.get("models_absent")), "models")
    expect_list_contains(targets, parse_list(extra.get("targets")), "targets")
    expect_list_absent(targets, parse_list(extra.get("targets_absent")), "targets")

    if "user_prompt" in extra:
        if entry.get("user_prompt") != extra["user_prompt"]:
            fail(f"Expected user_prompt {extra['user_prompt']}, got {entry.get('user_prompt')}")

    compare_str(options.get("normality"), parse_arg(extra.get("normality")), "normality")
    compare_str(options.get("sem_type"), parse_arg(extra.get("sem_type")), "sem_type")
    compare_str(options.get("estimator"), parse_arg(extra.get("estimator")), "estimator")
    compare_str(options.get("missing"), parse_arg(extra.get("missing")), "missing")
    compare_str(options.get("se"), parse_arg(extra.get("se")), "se")
    compare_str(options.get("ci"), parse_arg(extra.get("ci")), "ci")
    compare_str(options.get("std"), parse_arg(extra.get("std")), "std")
    compare_str(options.get("optimizer"), parse_arg(extra.get("optimizer")), "optimizer")

    expected_homogeneity = parse_arg(extra.get("homogeneity"))
    if expected_homogeneity is not None:
        actual_homogeneity = [item.lower() for item in listify(options.get("homogeneity")) if item]
        if expected_homogeneity.lower() in ("none", "null"):
            if actual_homogeneity:
                fail(f"Expected homogeneity to be empty, got {actual_homogeneity}")
        else:
            expect_list_contains(actual_homogeneity, parse_list(expected_homogeneity), "homogeneity")

    for key in (
        "linearity",
        "homoscedasticity",
        "vif",
        "durbin_watson",
        "outliers",
        "influence",
        "random_effects",
        "singular",
        "convergence",
        "dharma",
        "performance",
        "mardia",
        "mahalanobis",
        "collinearity",
        "heywood",
        "bootstrap",
        "reml",
    ):
        compare_bool(options.get(key), parse_bool(extra.get(key)), key)

    compare_float(options.get("alpha"), parse_float(extra.get("alpha")), "alpha")
    compare_float(options.get("vif_warn"), parse_float(extra.get("vif_warn")), "vif_warn")
    compare_float(options.get("vif_high"), parse_float(extra.get("vif_high")), "vif_high")
    compare_float(options.get("outlier_z"), parse_float(extra.get("outlier_z")), "outlier_z")
    compare_float(options.get("cook_multiplier"), parse_float(extra.get("cook_multiplier")), "cook_multiplier")
    compare_float(options.get("mahalanobis_alpha"), parse_float(extra.get("mahalanobis_alpha")), "mahalanobis_alpha")
    compare_float(options.get("max_cor"), parse_float(extra.get("max_cor")), "max_cor")
    compare_float(options.get("max_kappa"), parse_float(extra.get("max_kappa")), "max_kappa")

    compare_int(options.get("digits"), parse_int(extra.get("digits")), "digits")
    compare_int(options.get("max_shapiro_n"), parse_int(extra.get("max_shapiro_n")), "max_shapiro_n")
    compare_int(options.get("bootstrap_samples"), parse_int(extra.get("bootstrap_samples")), "bootstrap_samples")
    compare_int(options.get("maxfun"), parse_int(extra.get("maxfun")), "maxfun")


if __name__ == "__main__":
    main()
