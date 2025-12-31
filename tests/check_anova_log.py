# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env python3
import json
import sys
from pathlib import Path

def parse_arg(value):
    if value in ("", "-", "none", "None"):
        return None
    return value

def parse_list(value):
    if value is None:
        return []
    return [item.strip() for item in str(value).split(",") if item.strip()]

def listify(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(item) for item in value if item is not None]
    return [str(value)]

def to_float(value):
    try:
        return float(value)
    except (TypeError, ValueError):
        return None

def parse_expectations(args):
    expectations = {}
    for arg in args:
        if "=" not in arg:
            continue
        key, value = arg.split("=", 1)
        key = key.strip().lower()
        value = value.strip()
        if key:
            expectations[key] = value
    return expectations

def match_expectations(entry, options, results, expectations):
    if not expectations:
        return True

    def lower(value):
        if value is None:
            return None
        return str(value).lower()

    if "type" in expectations:
        if lower(options.get("type")) != lower(expectations["type"]):
            return False

    if "p_adjust" in expectations:
        if lower(options.get("p_adjust")) != lower(expectations["p_adjust"]):
            return False

    if "contrasts" in expectations:
        if lower(options.get("contrasts")) != lower(expectations["contrasts"]):
            return False

    if "conf_level" in expectations:
        expected = to_float(expectations["conf_level"])
        actual = to_float(options.get("conf_level"))
        if expected is None or actual is None or abs(actual - expected) > 1e-6:
            return False

    if "digits" in expectations:
        expected = to_float(expectations["digits"])
        actual = to_float(options.get("digits"))
        if expected is None or actual is None or int(round(actual)) != int(round(expected)):
            return False

    if "bootstrap_samples" in expectations:
        expected = to_float(expectations["bootstrap_samples"])
        actual = to_float(options.get("bootstrap_samples"))
        if expected is None or actual is None or int(round(actual)) != int(round(expected)):
            return False

    if "user_prompt" in expectations:
        if entry.get("user_prompt") != expectations["user_prompt"]:
            return False

    if "covariates" in expectations:
        expected = [item.lower() for item in parse_list(expectations["covariates"])]
        actual = [item.lower() for item in listify(options.get("covariates"))]
        if any(item not in actual for item in expected):
            return False

    assumptions_df = results.get("assumptions_df")
    if "assumptions" in expectations:
        expected = parse_list(expectations["assumptions"])
        if not isinstance(assumptions_df, list):
            return False
        names = {row.get("assumption") for row in assumptions_df if isinstance(row, dict)}
        if any(item not in names for item in expected):
            return False

    if "assumption_tests" in expectations:
        expected = parse_list(expectations["assumption_tests"])
        if not isinstance(assumptions_df, list):
            return False
        tests = {row.get("test") for row in assumptions_df if isinstance(row, dict)}
        if any(item not in tests for item in expected):
            return False

    if "posthoc_groups" in expectations:
        mode = lower(expectations["posthoc_groups"])
        posthoc_df = results.get("posthoc_df")
        if not isinstance(posthoc_df, list):
            return False
        groups = [row.get("group") for row in posthoc_df if isinstance(row, dict)]
        non_empty = [g for g in groups if g not in (None, "", "NA")]
        if mode == "present":
            if not non_empty:
                return False
        elif mode == "absent":
            if non_empty:
                return False
        elif mode == "grouped":
            if not any("=" in str(g) for g in non_empty):
                return False
        elif mode == "overall":
            if not groups:
                return False
            for g in groups:
                if g not in (None, "", "Overall"):
                    return False

    if "posthoc_p_adj" in expectations:
        mode = lower(expectations["posthoc_p_adj"])
        posthoc_df = results.get("posthoc_df")
        if not isinstance(posthoc_df, list):
            return False
        has_adj = any(
            row.get("p_adj") is not None
            for row in posthoc_df if isinstance(row, dict)
        )
        if mode == "present" and not has_adj:
            return False
        if mode == "absent" and has_adj:
            return False

    if "contrast_rows" in expectations:
        contrast_df = results.get("contrasts_df")
        contrast_len = len(contrast_df) if isinstance(contrast_df, list) else 0
        expect = expectations["contrast_rows"]
        if expect == "gt0" and contrast_len <= 0:
            return False
        if expect.isdigit() and contrast_len != int(expect):
            return False

    return True

if len(sys.argv) < 9:
    sys.stderr.write("Usage: check_anova_log.py LOG START MODE POSTHOC POSTHOC_ROWS EFFECT_SIZE SPHERICITY STATUS [BOOT_CI] [KEY=VALUE...]\n")
    sys.exit(2)

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
expect_mode = parse_arg(sys.argv[3])
expect_posthoc = parse_arg(sys.argv[4])
expect_posthoc_rows = parse_arg(sys.argv[5])
expect_effect_size = parse_arg(sys.argv[6])
expect_sphericity = parse_arg(sys.argv[7])
expect_status = parse_arg(sys.argv[8])
expect_boot_ci = None
extra_args = []
if len(sys.argv) > 9:
    if "=" in sys.argv[9]:
        extra_args = sys.argv[9:]
    else:
        expect_boot_ci = parse_arg(sys.argv[9])
        extra_args = sys.argv[10:]
expectations = parse_expectations(extra_args)

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
        if entry.get("module") != "anova":
            continue
        results = entry.get("results", {})
        options = entry.get("options", {})

        if expect_status is not None:
            if results.get("status") != expect_status:
                continue
            found = True
            break

        summary_df = results.get("summary_df")
        if not isinstance(summary_df, list) or len(summary_df) == 0:
            continue

        if expect_mode is not None and options.get("mode") != expect_mode:
            continue
        if expect_posthoc is not None and options.get("posthoc") != expect_posthoc:
            continue
        if expect_effect_size is not None and options.get("effect_size") != expect_effect_size:
            continue

        if expect_posthoc_rows is not None:
            posthoc_df = results.get("posthoc_df")
            posthoc_len = len(posthoc_df) if isinstance(posthoc_df, list) else 0
            if expect_posthoc_rows == "gt0" and posthoc_len <= 0:
                continue
            if expect_posthoc_rows.isdigit() and posthoc_len != int(expect_posthoc_rows):
                continue

        if expect_sphericity is not None:
            assumptions_df = results.get("assumptions_df")
            if not isinstance(assumptions_df, list):
                continue
            has_sphericity = any(row.get("assumption") == "Sphericity" for row in assumptions_df if isinstance(row, dict))
            if expect_sphericity == "present" and not has_sphericity:
                continue
            if expect_sphericity == "absent" and has_sphericity:
                continue

        if expect_boot_ci is not None:
            has_boot_ci = any(
                row.get("boot_ci_low") is not None and row.get("boot_ci_high") is not None
                for row in summary_df if isinstance(row, dict)
            )
            if expect_boot_ci == "present" and not has_boot_ci:
                continue
            if expect_boot_ci == "absent" and has_boot_ci:
                continue

        if not match_expectations(entry, options, results, expectations):
            continue

        found = True
        break

sys.exit(0 if found else 1)
