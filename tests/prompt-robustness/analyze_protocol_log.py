#!/usr/bin/env python3
import argparse
import csv
import json
import shlex
from datetime import datetime
from pathlib import Path
from statistics import median


HARMLESS_KEYS = {
    "digits",
    "template",
    "log",
    "user_prompt",
    "seed",
    "config",
    "csv",
    "parquet",
    "sav",
    "rds",
    "rdata",
    "df",
    "interactive",
    "header",
    "sep",
}

UNORDERED_LIST_KEYS = {
    "vars",
    "ivs",
    "controls",
    "x",
    "y",
    "rows",
    "cols",
}

ORDERED_LIST_KEYS = {
    "within",
    "blocks",
}

ALIAS_KEYS = {
    "between": "group",
    "group": "group",
    "row": "rows",
    "col": "cols",
    "residuals": "include_residuals",
    "expected": "include_expected",
}


def parse_args():
    parser = argparse.ArgumentParser(description="Analyze prompt robustness protocol_log.jsonl")
    parser.add_argument("--protocol", help="Path to protocol_log.jsonl (defaults to latest run)")
    parser.add_argument("--prompts", help="Path to prompts.csv (defaults to tests/prompt-robustness/prompts.csv)")
    parser.add_argument("--out", help="Output directory for summary files (defaults to protocol log folder)")
    return parser.parse_args()


def parse_iso(ts):
    if not ts:
        return None
    if ts.endswith("Z"):
        ts = ts[:-1] + "+00:00"
    try:
        return datetime.fromisoformat(ts)
    except ValueError:
        return None


def normalize_key(key):
    key = key.strip().lstrip("-")
    return key.replace("-", "_")


def normalize_value(value, key):
    if isinstance(value, list):
        return [str(v).strip() for v in value]
    if isinstance(value, (int, float)):
        return value
    if value is None:
        return None
    text = str(value).strip()
    if key in UNORDERED_LIST_KEYS or key in ORDERED_LIST_KEYS:
        items = [v.strip() for v in text.split(",") if v.strip()]
        return items
    try:
        if "." in text or "e" in text.lower():
            return float(text)
        return int(text)
    except ValueError:
        return text


def normalize_options(opts):
    normalized = {}
    for key, value in opts.items():
        canon_key = normalize_key(key)
        canon_key = ALIAS_KEYS.get(canon_key, canon_key)
        normalized[canon_key] = normalize_value(value, canon_key)
    return normalized


def parse_rscript_call(call):
    if not call:
        return None, {}
    try:
        tokens = shlex.split(call)
    except ValueError:
        tokens = call.split()
    script = None
    if tokens:
        if tokens[0].lower() == "rscript" and len(tokens) > 1:
            script = tokens[1]
        else:
            script = tokens[0]
    opts = parse_tokens(tokens)
    return script, opts


def parse_tokens(tokens):
    opts = {}
    idx = 0
    while idx < len(tokens):
        token = tokens[idx]
        if token.startswith("--"):
            key = normalize_key(token[2:])
            value = True
            if idx + 1 < len(tokens) and not tokens[idx + 1].startswith("--"):
                value = tokens[idx + 1]
                idx += 1
            opts[key] = value
        idx += 1
    return opts


def merge_opts(primary, secondary):
    merged = dict(primary)
    for key, value in secondary.items():
        if key not in merged or merged[key] in (None, "", []):
            merged[key] = value
    return merged


def load_prompts(path):
    if not path:
        return {}, []
    with open(path, "r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        rows = list(reader)
    mapping = {}
    for row in rows:
        key = (row.get("subskill", ""), row.get("nl_prompt", ""))
        mapping[key] = row
    return mapping, reader.fieldnames or ["subskill", "intent", "Rscript_call", "nl_prompt"]


def parse_prompt_csv_line(line, fieldnames):
    if not line:
        return {}
    try:
        reader = csv.reader([line])
        row = next(reader)
    except Exception:
        return {}
    if not fieldnames or len(row) != len(fieldnames):
        return {}
    return dict(zip(fieldnames, row))


def extract_expected(header, prompts_map, fieldnames):
    subskill = header.get("target_subskill", "")
    prompt = header.get("target_prompt", "")
    expected = prompts_map.get((subskill, prompt))
    if expected:
        return expected
    parsed = parse_prompt_csv_line(header.get("prompt_csv_line", ""), fieldnames)
    return parsed


def listify(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [str(v).strip() for v in value if str(v).strip()]
    text = str(value).strip()
    if not text:
        return []
    return [v.strip() for v in text.split(",") if v.strip()]


def to_set(value):
    return set(listify(value))


def contains_all(expected, actual, unordered=True):
    if unordered:
        return to_set(expected).issubset(to_set(actual))
    return listify(expected) == listify(actual)

def truthy(value):
    if isinstance(value, bool):
        return value
    if value is None:
        return False
    text = str(value).strip().lower()
    if text in {"true", "t", "1", "yes", "y"}:
        return True
    if text in {"false", "f", "0", "no", "n"}:
        return False
    return False

def prompt_contains_term(prompt, term):
    import re
    if not prompt or not term:
        return False
    pattern = r"(?i)(?<![A-Za-z0-9_])" + re.escape(term) + r"(?![A-Za-z0-9_])"
    return re.search(pattern, prompt) is not None


def required_terms_from_expected(expected):
    import re
    terms = []
    for key in ("vars", "x", "y", "dv", "ivs", "rows", "cols", "within", "subject_id", "group", "controls"):
        if key in expected and expected.get(key):
            terms.extend(listify(expected.get(key)))
    if expected.get("formula"):
        vars_in_formula = set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", str(expected.get("formula"))))
        vars_in_formula = {v for v in vars_in_formula if v not in {"c", "I"}}
        terms.extend(sorted(vars_in_formula))
    # de-dup while preserving order
    seen = set()
    out = []
    for term in terms:
        if term and term not in seen:
            seen.add(term)
            out.append(term)
    return out


def prompt_well_defined(subskill, expected_opts, target_prompt):
    expected = expected_signature(subskill, expected_opts)
    required_terms = required_terms_from_expected(expected)
    if not required_terms:
        return True, []
    missing = [term for term in required_terms if not prompt_contains_term(target_prompt, term)]
    if missing:
        return False, missing
    return True, []

def normalize_actual_options(entry):
    opts = normalize_options(entry.get("options", {}))
    commands = entry.get("commands") or []
    if commands and isinstance(commands, list):
        parsed = parse_tokens([str(t) for t in commands])
        opts = merge_opts(opts, normalize_options(parsed))
    prompt = entry.get("prompt")
    if prompt:
        _, parsed = parse_rscript_call(prompt)
        opts = merge_opts(opts, normalize_options(parsed))
    return opts


def expected_signature(subskill, expected_opts):
    expected = {}
    for key, value in expected_opts.items():
        canon_key = normalize_key(key)
        canon_key = ALIAS_KEYS.get(canon_key, canon_key)
        if canon_key in HARMLESS_KEYS:
            continue
        expected[canon_key] = normalize_value(value, canon_key)
    return expected


def method_required_from_prompt(prompt):
    if not prompt:
        return False
    text = prompt.lower()
    return any(token in text for token in ("spearman", "pearson", "kendall", "rank"))


def reverse_transform_applied(entries, expected_reverse):
    if not expected_reverse:
        return False
    expected_reverse = [str(v) for v in expected_reverse if v]
    for entry in entries:
        if entry.get("module") != "data_transform":
            continue
        calc = ""
        opts = entry.get("options") or {}
        calc = str(opts.get("calc") or "")
        if not calc:
            continue
        for var in expected_reverse:
            if var in calc and "-" in calc:
                return True
    return False


def extract_transform_map(entries):
    import re
    mapping = {}
    for entry in entries:
        if entry.get("module") != "data_transform":
            continue
        opts = entry.get("options") or {}
        calc = str(opts.get("calc") or "")
        if not calc or "=" not in calc:
            continue
        left, _, right = calc.partition("=")
        new_var = left.strip()
        if not new_var:
            continue
        vars_in_expr = set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", right))
        for var in vars_in_expr:
            mapping.setdefault(var, set()).add(new_var)
    return mapping


def satisfies_intent(subskill, expected_opts, actual_module, actual_opts, target_prompt, entries):
    """
    Returns classification in {satisfied, partially_satisfied, unsatisfied} and notes list.
    """
    notes = []
    expected = expected_signature(subskill, expected_opts)
    module = (actual_module or "").replace("-", "_")

    actual_vars = actual_opts.get("vars")
    if not actual_vars:
        x_vals = listify(actual_opts.get("x"))
        y_vals = listify(actual_opts.get("y"))
        if x_vals or y_vals:
            actual_vars = x_vals + y_vals

    if subskill == "descriptive-stats":
        if module != "descriptive_stats":
            return "unsatisfied", ["module_mismatch"]
        if "vars" in expected and not contains_all(expected["vars"], actual_vars):
            return "unsatisfied", ["vars_mismatch"]
        if "group" in expected and expected.get("group") not in (None, "", "NA"):
            if str(actual_opts.get("group", "")) != str(expected.get("group")):
                return "unsatisfied", ["group_mismatch"]
        return "satisfied", notes

    if subskill == "frequencies":
        expected_vars = listify(expected.get("vars", []))
        if module == "frequencies":
            if expected_vars and not contains_all(expected_vars, actual_vars):
                return "unsatisfied", ["vars_mismatch"]
            if "group" in expected and expected.get("group") not in (None, "", "NA"):
                if str(actual_opts.get("group", "")) != str(expected.get("group")):
                    return "unsatisfied", ["group_mismatch"]
            return "satisfied", notes
        if module == "crosstabs" and len(expected_vars) == 2:
            rows = listify(actual_opts.get("rows"))
            cols = listify(actual_opts.get("cols"))
            if set(rows + cols) == set(expected_vars):
                notes.append("module_substitution:crosstabs")
                return "partially_satisfied", notes
        return "unsatisfied", ["module_mismatch"]

    if subskill == "crosstabs":
        if module != "crosstabs":
            return "unsatisfied", ["module_mismatch"]
        row_expected = listify(expected.get("rows") or expected.get("row"))
        col_expected = listify(expected.get("cols") or expected.get("col"))
        rows = listify(actual_opts.get("rows"))
        cols = listify(actual_opts.get("cols"))
        if row_expected and not contains_all(row_expected, rows):
            return "unsatisfied", ["row_mismatch"]
        if col_expected and not contains_all(col_expected, cols):
            return "unsatisfied", ["col_mismatch"]
        if truthy(expected.get("chisq")) and not truthy(actual_opts.get("chisq")):
            return "unsatisfied", ["chisq_missing"]
        if truthy(expected.get("include_residuals")) and not truthy(actual_opts.get("include_residuals")):
            return "unsatisfied", ["residuals_missing"]
        return "satisfied", notes

    if subskill == "correlations":
        if module != "correlations":
            return "unsatisfied", ["module_mismatch"]
        if "vars" in expected and not contains_all(expected["vars"], actual_vars):
            return "unsatisfied", ["vars_mismatch"]
        if "controls" in expected and expected["controls"]:
            if not contains_all(expected["controls"], actual_opts.get("controls")):
                return "unsatisfied", ["controls_mismatch"]
        return "satisfied", notes

    if subskill == "scale":
        if module != "scale":
            return "unsatisfied", ["module_mismatch"]
        if "vars" in expected:
            expected_vars = listify(expected["vars"])
            missing_vars = [v for v in expected_vars if v not in listify(actual_vars)]
            if missing_vars:
                transform_map = extract_transform_map(entries)
                for var in list(missing_vars):
                    new_vars = transform_map.get(var, set())
                    if new_vars and any(nv in listify(actual_vars) for nv in new_vars):
                        missing_vars.remove(var)
                if missing_vars:
                    return "unsatisfied", ["vars_mismatch"]
                notes.append("vars_transformed")
        if expected.get("reverse"):
            expected_reverse = listify(expected.get("reverse"))
            if not contains_all(expected_reverse, listify(actual_opts.get("reverse"))):
                if reverse_transform_applied(entries, expected_reverse):
                    notes.append("reverse_transformed")
                    return "satisfied", notes
                return "unsatisfied", ["reverse_mismatch"]
            exp_min = expected.get("reverse_min")
            exp_max = expected.get("reverse_max")
            if exp_min is not None or exp_max is not None:
                if actual_opts.get("reverse_min") in (None, "") or actual_opts.get("reverse_max") in (None, ""):
                    if reverse_transform_applied(entries, expected_reverse):
                        notes.append("reverse_transformed")
                        return "satisfied", notes
                    notes.append("reverse_bounds_defaulted")
                    return "satisfied", notes
                if str(actual_opts.get("reverse_min")) != str(exp_min) or str(actual_opts.get("reverse_max")) != str(exp_max):
                    return "unsatisfied", ["reverse_bounds_mismatch"]
        return "satisfied", notes

    if subskill == "efa":
        if module != "efa":
            return "unsatisfied", ["module_mismatch"]
        if "vars" in expected and not contains_all(expected["vars"], actual_vars):
            return "unsatisfied", ["vars_mismatch"]
        if "n_factors" in expected and str(actual_opts.get("n_factors")) != str(expected.get("n_factors")):
            return "unsatisfied", ["n_factors_mismatch"]
        if "rotation" in expected and str(actual_opts.get("rotation")) != str(expected.get("rotation")):
            return "unsatisfied", ["rotation_mismatch"]
        return "satisfied", notes

    if subskill == "reliability":
        expected_analysis = expected.get("analysis")
        expected_method = expected.get("method")
        method_required = method_required_from_prompt(target_prompt)
        expected_vars = listify(expected.get("vars"))
        if module == "reliability":
            if expected_analysis and str(actual_opts.get("analysis")) != str(expected_analysis):
                return "unsatisfied", ["analysis_mismatch"]
            if expected_vars and not contains_all(expected_vars, actual_vars):
                return "unsatisfied", ["vars_mismatch"]
            if expected_method and method_required:
                if str(actual_opts.get("method")) != str(expected_method):
                    return "unsatisfied", ["method_mismatch"]
            return "satisfied", notes
        if module == "correlations" and expected_analysis in (None, "test_retest"):
            if expected_vars and not contains_all(expected_vars, actual_vars):
                return "unsatisfied", ["vars_mismatch"]
            if expected_method and method_required:
                if str(actual_opts.get("method")) != str(expected_method):
                    return "unsatisfied", ["method_mismatch"]
            notes.append("module_substitution:correlations")
            return "partially_satisfied", notes
        return "unsatisfied", ["module_mismatch"]

    if subskill == "assumptions":
        if module != "assumptions":
            return "unsatisfied", ["module_mismatch"]
        expected_analysis = expected.get("analysis")
        if expected_analysis and str(actual_opts.get("analysis")) != str(expected_analysis):
            return "unsatisfied", ["analysis_mismatch"]
        if expected.get("dv") and str(actual_opts.get("dv")) != str(expected.get("dv")):
            return "unsatisfied", ["dv_mismatch"]
        if expected.get("ivs") and not contains_all(expected.get("ivs"), actual_opts.get("ivs")):
            return "unsatisfied", ["ivs_mismatch"]
        return "satisfied", notes

    if subskill == "regression":
        if module != "regression":
            return "unsatisfied", ["module_mismatch"]
        if expected.get("dv") and str(actual_opts.get("dv")) != str(expected.get("dv")):
            return "unsatisfied", ["dv_mismatch"]
        if expected.get("ivs") and not contains_all(expected.get("ivs"), actual_opts.get("ivs")):
            return "unsatisfied", ["ivs_mismatch"]
        if expected.get("family") and str(actual_opts.get("family")) != str(expected.get("family")):
            return "unsatisfied", ["family_mismatch"]
        return "satisfied", notes

    if subskill == "power":
        if module != "power":
            return "unsatisfied", ["module_mismatch"]
        for key in ("analysis", "mode", "groups", "effect_size", "power"):
            if key in expected and str(actual_opts.get(key)) != str(expected.get(key)):
                return "unsatisfied", [f"{key}_mismatch"]
        return "satisfied", notes

    if subskill == "mixed-models":
        if module not in {"mixed_models", "mixed-models"}:
            return "unsatisfied", ["module_mismatch"]
        formula = str(actual_opts.get("formula") or "")
        # Accept dv/fixed/random formulation as equivalent to formula
        dv = actual_opts.get("dv")
        fixed = actual_opts.get("fixed")
        random = actual_opts.get("random")
        if not formula:
            if dv and fixed and random:
                formula = "{} ~ {} + ({})".format(dv, fixed, random)
            else:
                return "unsatisfied", ["formula_missing"]
        expected_formula = expected.get("formula")
        if expected_formula:
            # Check expected variable coverage (order-insensitive)
            import re
            expected_vars = set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", str(expected_formula)))
            actual_vars = set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", formula))
            # Drop common function-like tokens if present
            drop_tokens = {"c", "I"}
            expected_vars = {v for v in expected_vars if v not in drop_tokens}
            actual_vars = {v for v in actual_vars if v not in drop_tokens}
            if not expected_vars.issubset(actual_vars):
                return "unsatisfied", ["formula_missing_terms"]
            # Check random effect grouping factors
            expected_groups = set(re.findall(r"\\|\\s*([A-Za-z_][A-Za-z0-9_]*)", str(expected_formula)))
            actual_groups = set(re.findall(r"\\|\\s*([A-Za-z_][A-Za-z0-9_]*)", formula))
            if expected_groups and not expected_groups.issubset(actual_groups):
                return "unsatisfied", ["random_effect_group_mismatch"]
            return "satisfied", notes
        # Fallback rule if no expected formula is available
        if "(1|" not in formula:
            return "unsatisfied", ["random_intercept_missing"]
        return "satisfied", notes

    if subskill == "sem":
        if module != "sem":
            return "unsatisfied", ["module_mismatch"]
        model = str(actual_opts.get("model") or "")
        for term in ("x1", "mediator", "outcome_reg"):
            if term not in model:
                return "unsatisfied", [f"model_missing_{term}"]
        return "satisfied", notes

    if subskill == "anova":
        if module != "anova":
            return "unsatisfied", ["module_mismatch"]
        if expected.get("within") and not contains_all(expected.get("within"), actual_opts.get("within")):
            return "unsatisfied", ["within_mismatch"]
        if expected.get("between") and str(actual_opts.get("between")) != str(expected.get("between")):
            return "unsatisfied", ["between_mismatch"]
        if expected.get("subject_id") and str(actual_opts.get("subject_id")) != str(expected.get("subject_id")):
            return "unsatisfied", ["subject_id_mismatch"]
        return "satisfied", notes

    if subskill == "t-test":
        if module not in {"t_test", "t-test"}:
            return "unsatisfied", ["module_mismatch"]
        if expected.get("x") and str(actual_opts.get("x")) != str(expected.get("x")):
            return "unsatisfied", ["x_mismatch"]
        if expected.get("y") and str(actual_opts.get("y")) != str(expected.get("y")):
            return "unsatisfied", ["y_mismatch"]
        return "satisfied", notes

    if subskill == "nonparametric":
        if module != "nonparametric":
            return "unsatisfied", ["module_mismatch"]
        if expected.get("test") and str(actual_opts.get("test")) != str(expected.get("test")):
            return "unsatisfied", ["test_mismatch"]
        if expected.get("within") and not contains_all(expected.get("within"), actual_opts.get("within")):
            return "unsatisfied", ["within_mismatch"]
        if expected.get("subject_id") and str(actual_opts.get("subject_id")) != str(expected.get("subject_id")):
            return "unsatisfied", ["subject_id_mismatch"]
        return "satisfied", notes

    expected_module = subskill.replace("-", "_")
    if module == expected_module:
        return "partially_satisfied", ["module_match_no_rules"]
    return "unsatisfied", ["module_mismatch"]


def compute_percentile(values, pct):
    if not values:
        return None
    values_sorted = sorted(values)
    if len(values_sorted) == 1:
        return values_sorted[0]
    idx = int(round((pct / 100) * (len(values_sorted) - 1)))
    return values_sorted[idx]


def latest_protocol_log():
    root = Path(__file__).resolve().parents[2] / "outputs" / "prompt-robustness-runs"
    if not root.exists():
        return None
    run_dirs = [p for p in root.iterdir() if p.is_dir()]
    run_dirs.sort(key=lambda p: p.name, reverse=True)
    for run_dir in run_dirs:
        candidate = run_dir / "protocol_log.jsonl"
        if candidate.exists():
            return candidate
    return None


def main():
    args = parse_args()
    protocol_path = Path(args.protocol) if args.protocol else latest_protocol_log()
    if not protocol_path:
        raise SystemExit("protocol log not found (pass --protocol or ensure outputs/prompt-robustness-runs has runs)")
    if not protocol_path.exists():
        raise SystemExit(f"protocol log not found: {protocol_path}")

    default_prompts = Path(__file__).resolve().parents[1] / "prompt-robustness" / "prompts.csv"
    prompts_path = args.prompts if args.prompts else str(default_prompts)
    prompts_map, fieldnames = load_prompts(prompts_path if Path(prompts_path).exists() else None)

    runs = []
    current = None
    with protocol_path.open("r", encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            if "prompt_csv_line" in obj:
                if current:
                    runs.append(current)
                current = {"header": obj, "entries": []}
            else:
                if current is None:
                    continue
                current["entries"].append(obj)
    if current:
        runs.append(current)

    per_prompt = []
    durations = []
    interprompt_durations = []
    satisfied_count = 0
    partial_count = 0
    unsatisfied_count = 0
    missing_count = 0
    defined_count = 0
    underdefined_count = 0

    for idx, run in enumerate(runs, start=1):
        header = run["header"]
        entries = run["entries"]
        expected_row = extract_expected(header, prompts_map, fieldnames)
        expected_call = expected_row.get("Rscript_call") if expected_row else ""
        expected_script, expected_opts = parse_rscript_call(expected_call)
        expected_opts = normalize_options(expected_opts)

        expected_subskill = header.get("target_subskill", "")
        expected_module = expected_subskill.replace("-", "_")
        target_prompt = header.get("target_prompt", "")
        is_defined, missing_terms = prompt_well_defined(expected_subskill, expected_opts, target_prompt)
        if is_defined:
            defined_count += 1
        else:
            underdefined_count += 1

        entry_for_compare = entries[0] if entries else None
        for entry in entries:
            if entry.get("module") == expected_module:
                entry_for_compare = entry
                break
        if not entries:
            missing_count += 1
            per_prompt.append({
                "prompt_index": idx,
                "target_subskill": expected_subskill,
                "target_prompt": header.get("target_prompt", ""),
                "entry_count": 0,
                "classification": "missing",
            })
            continue

        actual_opts = normalize_actual_options(entry_for_compare)
        classification, notes = satisfies_intent(
            expected_subskill,
            expected_opts,
            entry_for_compare.get("module", ""),
            actual_opts,
            header.get("target_prompt", ""),
            entries,
        )

        # If prompt is underdefined, allow partial credit when module matches but vars are underspecified
        if not is_defined and classification == "unsatisfied" and "vars_mismatch" in notes:
            if entry_for_compare.get("module") == expected_subskill.replace("-", "_"):
                classification = "partially_satisfied"
                notes = [n for n in notes if n != "vars_mismatch"]
                notes.append("underdefined_prompt")

        if classification == "satisfied":
            satisfied_count += 1
        elif classification == "partially_satisfied":
            partial_count += 1
        else:
            unsatisfied_count += 1

        timestamps = [parse_iso(entry.get("timestamp_utc")) for entry in entries]
        timestamps = [t for t in timestamps if t is not None]
        duration_sec = None
        time_first = None
        time_last = None
        if timestamps:
            time_first = min(timestamps)
            time_last = max(timestamps)
            duration_sec = (time_last - time_first).total_seconds()
            durations.append(duration_sec)

        per_prompt.append({
            "prompt_index": idx,
            "target_subskill": expected_subskill,
            "target_prompt": header.get("target_prompt", ""),
            "expected_script": Path(expected_script).name if expected_script else "",
            "actual_module": entry_for_compare.get("module", ""),
            "classification": classification,
            "notes": ",".join(notes),
            "prompt_well_defined": is_defined,
            "prompt_missing_terms": ",".join(missing_terms),
            "entry_count": len(entries),
            "time_first": time_first.isoformat() if time_first else "",
            "time_last": time_last.isoformat() if time_last else "",
            "duration_seconds": duration_sec if duration_sec is not None else "",
        })

    # Interprompt durations using time_first deltas between consecutive prompts
    prev_time = None
    for row in per_prompt:
        tf = parse_iso(row.get("time_first"))
        if tf is None:
            prev_time = None
            continue
        if prev_time is not None:
            interprompt_durations.append((tf - prev_time).total_seconds())
        prev_time = tf

    total = len(per_prompt)
    summary = {
        "total_prompts": total,
        "satisfied_rate": satisfied_count / total if total else 0.0,
        "partial_rate": partial_count / total if total else 0.0,
        "unsatisfied_rate": unsatisfied_count / total if total else 0.0,
        "missing_rate": missing_count / total if total else 0.0,
        "prompt_defined_rate": defined_count / total if total else 0.0,
        "prompt_underdefined_rate": underdefined_count / total if total else 0.0,
        "median_time_sec": median(durations) if durations else None,
        "p95_time_sec": compute_percentile(durations, 95),
        "median_interprompt_sec": median(interprompt_durations) if interprompt_durations else None,
        "p95_interprompt_sec": compute_percentile(interprompt_durations, 95),
    }

    out_dir = Path(args.out) if args.out else protocol_path.parent
    out_dir.mkdir(parents=True, exist_ok=True)

    summary_path = out_dir / "protocol_summary.json"
    with summary_path.open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    per_prompt_path = out_dir / "protocol_per_prompt.csv"
    with per_prompt_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(per_prompt[0].keys()) if per_prompt else [])
        if per_prompt:
            writer.writeheader()
            writer.writerows(per_prompt)

    classification_counts = {}
    subskill_mismatch = {}
    for row in per_prompt:
        cls = row.get("classification", "")
        classification_counts[cls] = classification_counts.get(cls, 0) + 1
        sub = row.get("target_subskill", "")
        if cls == "unsatisfied":
            subskill_mismatch[sub] = subskill_mismatch.get(sub, 0) + 1

    def pct(count):
        return (count / total * 100.0) if total else 0.0

    worst_subskills = sorted(subskill_mismatch.items(), key=lambda x: (-x[1], x[0]))[:5]
    mismatch_examples = [r for r in per_prompt if r.get("classification") == "unsatisfied"][:10]

    lines = []
    lines.append("# Prompt Robustness Report")
    lines.append("")
    lines.append("## Summary")
    lines.append(f"- Total prompts: {total}")
    lines.append(f"- Satisfied rate: {summary['satisfied_rate']:.2%}")
    lines.append(f"- Partial rate: {summary['partial_rate']:.2%}")
    lines.append(f"- Unsatisfied rate: {summary['unsatisfied_rate']:.2%}")
    lines.append(f"- Missing rate: {summary['missing_rate']:.2%}")
    lines.append(f"- Prompt well-defined rate: {summary['prompt_defined_rate']:.2%}")
    lines.append(f"- Prompt underdefined rate: {summary['prompt_underdefined_rate']:.2%}")
    if summary["median_time_sec"] is not None:
        lines.append(f"- Median within-prompt span: {summary['median_time_sec']:.2f}s")
    if summary["p95_time_sec"] is not None:
        lines.append(f"- P95 within-prompt span: {summary['p95_time_sec']:.2f}s")
    if summary["median_interprompt_sec"] is not None:
        lines.append(f"- Median interprompt time: {summary['median_interprompt_sec']:.2f}s")
    if summary["p95_interprompt_sec"] is not None:
        lines.append(f"- P95 interprompt time: {summary['p95_interprompt_sec']:.2f}s")
    lines.append("")
    lines.append("## Classification Breakdown")
    for cls, count in sorted(classification_counts.items(), key=lambda x: (-x[1], x[0])):
        lines.append(f"- {cls}: {count} ({pct(count):.1f}%)")
    lines.append("")
    if worst_subskills:
        lines.append("## Subskills with Most Unsatisfied Runs")
        for sub, count in worst_subskills:
            lines.append(f"- {sub}: {count} unsatisfied")
        lines.append("")
    if mismatch_examples:
        lines.append("## Sample Unsatisfied Runs")
        for row in mismatch_examples:
            lines.append(f"- index {row.get('prompt_index')}: {row.get('target_subskill')} ({row.get('classification')})")
            notes = row.get("notes")
            if notes:
                lines.append(f"  - {notes}")
        lines.append("")

    def render_full_list(title, items):
        if not items:
            return
        lines.append(title)
        for row in items:
            lines.append(f"- index {row.get('prompt_index')}: {row.get('target_subskill')}")
            lines.append(f"  - prompt: {row.get('target_prompt')}")
            notes = row.get("notes")
            if notes:
                lines.append(f"  - why: {notes}")
            else:
                lines.append("  - why: (no detail)")
        lines.append("")

    unsatisfied_all = [r for r in per_prompt if r.get("classification") == "unsatisfied"]
    render_full_list("## All Unsatisfied Runs", unsatisfied_all)

    partial_all = [r for r in per_prompt if r.get("classification") == "partially_satisfied"]
    render_full_list("## All Partially Satisfied Runs", partial_all)

    missing_all = [r for r in per_prompt if r.get("classification") == "missing"]
    render_full_list("## All Missing Runs", missing_all)

    underdefined_all = [r for r in per_prompt if str(r.get("prompt_well_defined")) == "False"]
    if underdefined_all:
        lines.append("## All Underdefined Prompts")
        for row in underdefined_all:
            lines.append(f"- index {row.get('prompt_index')}: {row.get('target_subskill')}")
            lines.append(f"  - prompt: {row.get('target_prompt')}")
            missing_terms = row.get("prompt_missing_terms")
            if missing_terms:
                lines.append("  - missing terms: {}".format(missing_terms))
            else:
                lines.append("  - missing terms: (unspecified)")
        lines.append("")

    report_path = out_dir / "protocol_report.md"
    with report_path.open("w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")

    print(f"Wrote {summary_path}")
    print(f"Wrote {per_prompt_path}")
    print(f"Wrote {report_path}")


if __name__ == "__main__":
    main()
