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


def parse_bool(value):
    if value is None:
        return None
    if isinstance(value, bool):
        return value
    val = str(value).strip().lower()
    if val in ("true", "t", "1", "yes", "y"):
        return True
    if val in ("false", "f", "0", "no", "n"):
        return False
    return None


def parse_int(value):
    if value is None:
        return None
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def parse_float(value):
    if value is None:
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def list_len(value):
    if isinstance(value, list):
        return len(value)
    return 0


def extract_numeric(rows, keys):
    values = []
    if not isinstance(rows, list):
        return values
    for row in rows:
        if not isinstance(row, dict):
            continue
        for key in keys:
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
        return True
    exp = str(expectation).strip().lower()
    if exp in ("present", "any"):
        return bool(values)
    if exp in ("absent", "none"):
        return not values
    if exp in ("gt0", "greater_than_zero"):
        return any(val > 0 for val in values)
    if exp in ("ge0", "gte0", "greater_equal_zero"):
        return any(val >= 0 for val in values)
    try:
        target = float(expectation)
    except (TypeError, ValueError):
        return True
    return any(abs(val - target) <= 1e-6 for val in values)


if len(sys.argv) < 22:
    sys.stderr.write(
        "Usage: check_mixed_models_log.py LOG START STATUS FORMULA DV RANDOM TYPE DF_METHOD STANDARDIZE DIAGNOSTICS "
        "EMMEANS CONTRASTS P_ADJUST CONF_LEVEL OPTIMIZER MAXFUN REML FIXED_ROWS EMMEANS_ROWS CONTRAST_ROWS "
        "DIAGNOSTICS_ROWS STD_BETA [DIGITS] [USER_PROMPT] [SHAPIRO] [r2=...] [icc=...]\n"
    )
    sys.exit(2)

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
expect_status = parse_arg(sys.argv[3])
expect_formula = parse_arg(sys.argv[4])
expect_dv = parse_arg(sys.argv[5])
expect_random = parse_arg(sys.argv[6])
expect_type = parse_arg(sys.argv[7])
expect_df_method = parse_arg(sys.argv[8])
expect_standardize = parse_arg(sys.argv[9])
expect_diagnostics = parse_bool(parse_arg(sys.argv[10]))
expect_emmeans = parse_arg(sys.argv[11])
expect_contrasts = parse_arg(sys.argv[12])
expect_p_adjust = parse_arg(sys.argv[13])
expect_conf_level = parse_float(parse_arg(sys.argv[14]))
expect_optimizer = parse_arg(sys.argv[15])
expect_maxfun = parse_int(parse_arg(sys.argv[16]))
expect_reml = parse_bool(parse_arg(sys.argv[17]))
expect_fixed_rows = parse_arg(sys.argv[18])
expect_emmeans_rows = parse_arg(sys.argv[19])
expect_contrast_rows = parse_arg(sys.argv[20])
expect_diagnostics_rows = parse_arg(sys.argv[21])
expect_std_beta = parse_arg(sys.argv[22]) if len(sys.argv) > 22 else None
expect_digits = parse_int(parse_arg(sys.argv[23])) if len(sys.argv) > 23 else None
expect_user_prompt = parse_arg(sys.argv[24]) if len(sys.argv) > 24 else None
expect_shapiro = parse_arg(sys.argv[25]) if len(sys.argv) > 25 else None
if expect_shapiro is not None:
    expect_shapiro = str(expect_shapiro).strip().lower()
extra = {}
if len(sys.argv) > 26:
    for arg in sys.argv[26:]:
        if "=" not in arg:
            continue
        key, value = arg.split("=", 1)
        key = key.strip()
        value = value.strip()
        if key:
            extra[key] = value
expect_r2 = parse_arg(extra.get("r2"))
expect_icc = parse_arg(extra.get("icc"))

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
        if entry.get("module") != "mixed_models":
            continue
        results = entry.get("results", {})
        options = entry.get("options", {})

        if expect_status is not None:
            if results.get("status") == expect_status:
                found = True
                break
            continue

        fixed_df = results.get("fixed_effects_df")
        if not isinstance(fixed_df, list) or len(fixed_df) == 0:
            continue

        if expect_user_prompt is not None and entry.get("user_prompt") != expect_user_prompt:
            continue

        if expect_formula is not None and options.get("formula") != expect_formula:
            continue
        if expect_dv is not None and options.get("dv") != expect_dv:
            continue
        if expect_random is not None:
            random_terms = options.get("random")
            if isinstance(random_terms, list):
                if expect_random not in random_terms:
                    continue
            elif isinstance(random_terms, str):
                if random_terms != expect_random:
                    continue
            else:
                continue
        if expect_type is not None and options.get("type") != expect_type:
            continue

        if expect_df_method is not None:
            df_method = options.get("df-method")
            if expect_df_method == "none":
                if df_method is not None and df_method != "none":
                    continue
            elif df_method != expect_df_method:
                continue

        if expect_standardize is not None:
            std_val = options.get("standardize")
            if expect_standardize == "none":
                if std_val is not None and std_val != "none":
                    continue
            elif std_val != expect_standardize:
                continue

        if expect_diagnostics is not None:
            if bool(options.get("diagnostics")) != expect_diagnostics:
                continue

        if expect_emmeans is not None:
            if expect_emmeans == "none":
                if options.get("emmeans"):
                    continue
            elif options.get("emmeans") != expect_emmeans:
                continue

        if expect_contrasts is not None:
            if expect_contrasts == "none":
                if options.get("contrasts"):
                    continue
            elif options.get("contrasts") != expect_contrasts:
                continue

        if expect_p_adjust is not None:
            if expect_p_adjust == "none":
                if options.get("p-adjust"):
                    continue
            elif options.get("p-adjust") != expect_p_adjust:
                continue

        if expect_conf_level is not None:
            try:
                if abs(float(options.get("conf-level")) - expect_conf_level) > 1e-6:
                    continue
            except (TypeError, ValueError):
                continue

        if expect_optimizer is not None and options.get("optimizer") != expect_optimizer:
            continue

        if expect_maxfun is not None:
            maxfun = options.get("maxfun")
            try:
                if int(maxfun) != expect_maxfun:
                    continue
            except (TypeError, ValueError):
                continue

        if expect_reml is not None:
            if bool(options.get("reml")) != expect_reml:
                continue

        if expect_digits is not None:
            digits_val = options.get("digits")
            try:
                if int(digits_val) != expect_digits:
                    continue
            except (TypeError, ValueError):
                continue

        if expect_fixed_rows is not None:
            fixed_len = list_len(fixed_df)
            if expect_fixed_rows == "gt0" and fixed_len <= 0:
                continue
            if expect_fixed_rows.isdigit() and fixed_len != int(expect_fixed_rows):
                continue

        if expect_emmeans_rows is not None:
            emmeans_len = list_len(results.get("emmeans_df"))
            if expect_emmeans_rows == "gt0" and emmeans_len <= 0:
                continue
            if expect_emmeans_rows.isdigit() and emmeans_len != int(expect_emmeans_rows):
                continue

        if expect_contrast_rows is not None:
            contrast_len = list_len(results.get("contrasts_df"))
            if expect_contrast_rows == "gt0" and contrast_len <= 0:
                continue
            if expect_contrast_rows.isdigit() and contrast_len != int(expect_contrast_rows):
                continue

        if expect_diagnostics_rows is not None:
            diag_len = list_len(results.get("diagnostics_df"))
            if expect_diagnostics_rows == "gt0" and diag_len <= 0:
                continue
            if expect_diagnostics_rows.isdigit() and diag_len != int(expect_diagnostics_rows):
                continue

        if expect_std_beta is not None:
            std_present = False
            if isinstance(fixed_df, list):
                for row in fixed_df:
                    if isinstance(row, dict) and row.get("std_beta") is not None:
                        std_present = True
                        break
            if expect_std_beta == "present" and not std_present:
                continue
            if expect_std_beta == "absent" and std_present:
                continue

        if expect_shapiro is not None:
            shapiro_present = False
            diagnostics_df = results.get("diagnostics_df")
            if isinstance(diagnostics_df, list):
                for row in diagnostics_df:
                    if isinstance(row, dict) and row.get("metric") == "shapiro_wilk":
                        shapiro_present = True
                        break
            if expect_shapiro == "present" and not shapiro_present:
                continue
            if expect_shapiro == "absent" and shapiro_present:
                continue

        if expect_r2 is not None:
            r2_values = extract_numeric(results.get("r2_df"), ("r2_marginal", "r2_conditional"))
            if not match_numeric_expectation(r2_values, expect_r2, "r2"):
                continue

        if expect_icc is not None:
            icc_values = extract_numeric(results.get("icc_df"), ("icc",))
            if not match_numeric_expectation(icc_values, expect_icc, "icc"):
                continue

        found = True
        break

sys.exit(0 if found else 1)
