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
        return None
    items = [item.strip() for item in str(value).split(",")]
    items = [item for item in items if item]
    return items or None


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


def has_non_null(rows, key):
    for row in rows:
        if row.get(key) is not None:
            return True
    return False


def has_interaction_term(rows):
    for row in rows:
        term = row.get("term")
        if isinstance(term, str) and (":" in term or "*" in term):
            return True
    return False


def extract_numeric(rows, key):
    values = []
    if not isinstance(rows, list):
        return values
    for row in rows:
        if not isinstance(row, dict):
            continue
        value = row.get(key)
        try:
            num = float(value)
        except (TypeError, ValueError):
            continue
        if num != num:
            continue
        values.append(num)
    return values


def match_numeric_expectation(values, expectation, label):
    if expectation is None:
        return
    exp = str(expectation).strip().lower()
    if exp in ("present", "any"):
        if not values:
            fail(f"Expected {label} values to be present")
        return
    if exp in ("absent", "none"):
        if values:
            fail(f"Expected {label} values to be absent")
        return
    if exp in ("gt0", "greater_than_zero"):
        if not any(val > 0 for val in values):
            fail(f"Expected {label} values > 0")
        return
    if exp in ("ge0", "gte0", "greater_equal_zero"):
        if not any(val >= 0 for val in values):
            fail(f"Expected {label} values >= 0")
        return
    try:
        target = float(expectation)
    except (TypeError, ValueError):
        return
    if not any(abs(val - target) <= 1e-6 for val in values):
        fail(f"Expected {label} value {target}")


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
    if len(sys.argv) < 14:
        fail(
            "Usage: check_regression_log.py <log_path> <start_count> <expected_status> "
            "<expected_family> <expected_stat_label> <min_models> <min_groups> "
            "<min_comparisons> <expect_beta> <expect_expb> <expect_boot> "
            "<expect_interaction> <expect_diagnostics> "
            "[link=...] [conf_level=...] [digits=...] [center=...] [standardize=...] "
            "[bootstrap_samples=...] [user_prompt=...] [terms=...] [no_terms=...] [group=...] "
            "[f2=...] [delta_f2=...]"
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
    extra = parse_kv_args(sys.argv[14:])
    expect_link = parse_arg(extra.get("link"))
    expect_conf_level = parse_float(parse_arg(extra.get("conf_level")))
    expect_digits = parse_int(parse_arg(extra.get("digits")))
    expect_center = parse_arg(extra.get("center"))
    expect_standardize = parse_arg(extra.get("standardize"))
    expect_boot_samples = parse_int(parse_arg(extra.get("bootstrap_samples")))
    expect_user_prompt = parse_arg(extra.get("user_prompt"))
    expect_terms = parse_list(parse_arg(extra.get("terms")))
    expect_no_terms = parse_list(parse_arg(extra.get("no_terms")))
    expect_group = parse_arg(extra.get("group"))
    expect_f2 = parse_arg(extra.get("f2"))
    expect_delta_f2 = parse_arg(extra.get("delta_f2"))

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

    options = entry.get("options", {}) or {}

    if expected_family not in (None, "", "-"):
        family = options.get("family")
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
        if not has_non_null(coefficients, "beta"):
            fail("Expected non-null beta values")
    if expect_beta is False:
        if has_non_null(coefficients, "beta"):
            fail("Expected beta values to be null")

    if expect_expb is True:
        if not has_non_null(coefficients, "exp_b"):
            fail("Expected non-null exp_b values")
    if expect_expb is False:
        if has_non_null(coefficients, "exp_b"):
            fail("Expected exp_b values to be null")

    if expect_boot is True:
        if not has_non_null(coefficients, "boot_ci_low"):
            fail("Expected non-null bootstrap CI values")
    if expect_boot is False:
        if has_non_null(coefficients, "boot_ci_low"):
            fail("Expected bootstrap CI values to be null")

    if expect_interaction is True:
        if not has_interaction_term(coefficients):
            fail("Expected interaction term in coefficients_df")
    if expect_interaction is False:
        if has_interaction_term(coefficients):
            fail("Expected no interaction terms in coefficients_df")

    if expect_diagnostics is True:
        if not diagnostics:
            fail("Expected diagnostics_df entries")

    if expect_diagnostics is False:
        if diagnostics:
            fail("Expected diagnostics_df to be empty")

    if expect_link is not None:
        if options.get("link") != expect_link:
            fail(f"Expected link {expect_link}, got {options.get('link')}")

    if expect_conf_level is not None:
        conf_val = options.get("conf_level")
        try:
            if abs(float(conf_val) - expect_conf_level) > 1e-6:
                fail(f"Expected conf_level {expect_conf_level}, got {conf_val}")
        except (TypeError, ValueError):
            fail(f"Expected conf_level {expect_conf_level}, got {conf_val}")

    if expect_digits is not None:
        digits_val = options.get("digits")
        try:
            if int(digits_val) != expect_digits:
                fail(f"Expected digits {expect_digits}, got {digits_val}")
        except (TypeError, ValueError):
            fail(f"Expected digits {expect_digits}, got {digits_val}")

    if expect_center is not None:
        if options.get("center") != expect_center:
            fail(f"Expected center {expect_center}, got {options.get('center')}")

    if expect_standardize is not None:
        if options.get("standardize") != expect_standardize:
            fail(f"Expected standardize {expect_standardize}, got {options.get('standardize')}")

    if expect_boot_samples is not None:
        sample_val = options.get("bootstrap_samples")
        try:
            if int(sample_val) != expect_boot_samples:
                fail(f"Expected bootstrap_samples {expect_boot_samples}, got {sample_val}")
        except (TypeError, ValueError):
            fail(f"Expected bootstrap_samples {expect_boot_samples}, got {sample_val}")

    if expect_user_prompt is not None:
        user_prompt = entry.get("user_prompt")
        if user_prompt != expect_user_prompt:
            fail(f"Expected user_prompt {expect_user_prompt}, got {user_prompt}")

    if expect_terms is not None:
        term_values = {row.get("term") for row in coefficients if row.get("term") is not None}
        for term in expect_terms:
            if term not in term_values:
                fail(f"Expected term {term} in coefficients_df")

    if expect_no_terms is not None:
        term_values = {row.get("term") for row in coefficients if row.get("term") is not None}
        for term in expect_no_terms:
            if term in term_values:
                fail(f"Expected term {term} to be absent in coefficients_df")

    if expect_group is not None:
        if options.get("group") != expect_group:
            fail(f"Expected group {expect_group}, got {options.get('group')}")

    if expect_f2 is not None:
        match_numeric_expectation(extract_numeric(summary, "f2"), expect_f2, "f2")
    if expect_delta_f2 is not None:
        match_numeric_expectation(extract_numeric(comparisons, "delta_f2"), expect_delta_f2, "delta_f2")


if __name__ == "__main__":
    main()
