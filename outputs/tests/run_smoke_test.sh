#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
DATA_PATH="${ROOT_DIR}/outputs/tests/golden_dataset.csv"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
RUN_RSCRIPT_PS="${ROOT_DIR}/core-stats/scripts/run_rscript.ps1"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/outputs/tests/check_r_package.R"
MIXED_MODELS_PREP_SCRIPT="${ROOT_DIR}/outputs/tests/mixed_models_prep.R"

get_config_value() {
  python3 - "$CONFIG_PATH" "$1" <<'PY'
import sys
path, key = sys.argv[1], sys.argv[2]
parts = key.split(".")
lines = open(path, "r", encoding="utf-8").read().splitlines()
stack = []
for line in lines:
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    key_name, _, rest = stripped.partition(":")
    while stack and stack[-1][1] >= indent:
        stack.pop()
    stack.append((key_name, indent))
    if [k for k, _ in stack] == parts:
        value = rest.strip()
        if value.startswith(("'", '"')) and value.endswith(("'", '"')) and len(value) >= 2:
            value = value[1:-1]
        print(value)
        sys.exit(0)
sys.exit(0)
PY
}

set_config_value() {
  python3 - "$CONFIG_PATH" "$1" "$2" <<'PY'
import sys
path, key, value = sys.argv[1], sys.argv[2], sys.argv[3]
parts = key.split(".")
lines = open(path, "r", encoding="utf-8").read().splitlines()
stack = []
for idx, line in enumerate(lines):
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    key_name, _, _ = stripped.partition(":")
    while stack and stack[-1][1] >= indent:
        stack.pop()
    stack.append((key_name, indent))
    if [k for k, _ in stack] == parts:
        lines[idx] = (" " * indent) + f"{key_name}: \"{value}\""
        break
else:
    sys.exit(1)
with open(path, "w", encoding="utf-8", newline="") as handle:
    handle.write("\n".join(lines) + "\n")
PY
}

RUNS_BASE_CFG="$(get_config_value tests.output_dir)"
TEMPLATE_OVERRIDE_DIR_CFG="$(get_config_value tests.template_dir)"
TEMPLATE_MARKER="$(get_config_value tests.template_marker)"

if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi
if [ -z "${TEMPLATE_OVERRIDE_DIR_CFG}" ]; then
  TEMPLATE_OVERRIDE_DIR_CFG="templates"
fi
if [ -z "${TEMPLATE_MARKER}" ]; then
  TEMPLATE_MARKER="TEMPLATE_SMOKE_TEST"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUN_MARKER="${TEMPLATE_MARKER}_${RUN_ID}"

to_abs_path() {
  local path="$1"
  if [[ "${path}" == "~"* ]]; then
    path="${HOME}${path:1}"
  fi
  if [[ "${path}" == /* || "${path}" =~ ^[A-Za-z]: ]]; then
    echo "${path}"
    return 0
  fi
  echo "${ROOT_DIR}/${path#./}"
}

resolve_run_path() {
  local path="$1"
  if [[ "${path}" == "~"* ]]; then
    path="${HOME}${path:1}"
  fi
  if [[ "${path}" == /* || "${path}" =~ ^[A-Za-z]: ]]; then
    echo "${path}"
    return 0
  fi
  echo "${RUN_ROOT}/${path#./}"
}

to_win_path() {
  local path="$1"
  if command -v wslpath >/dev/null 2>&1; then
    wslpath -w "$path" | tr '\\\\' '/'
    return 0
  fi
  echo "$path"
}

to_wsl_path() {
  local path="$1"
  if command -v wslpath >/dev/null 2>&1; then
    wslpath -u "$path"
    return 0
  fi
  echo "$path"
}

resolve_powershell() {
  if command -v powershell.exe >/dev/null 2>&1; then
    echo "powershell.exe"
    return 0
  fi
  if command -v pwsh.exe >/dev/null 2>&1; then
    echo "pwsh.exe"
    return 0
  fi
  if command -v pwsh >/dev/null 2>&1; then
    echo "pwsh"
    return 0
  fi
  return 1
}

POWERSHELL_BIN="$(resolve_powershell || true)"

RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${CORE_STATS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${CORE_STATS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
fi

WORKSPACE_DIR="${RUN_ROOT}/workspace"
TEMPLATE_OVERRIDE_DIR="$(resolve_run_path "${TEMPLATE_OVERRIDE_DIR_CFG}")"
TMP_BASE="${RUN_ROOT}/tmp"
LOG_PATH="${RUN_ROOT}/smoke_test.log"
DATASET_LABEL="$(basename "${DATA_PATH}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
APA_REPORT_PATH="${DATASET_DIR}/apa_report.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
WORKSPACE_DIR_WIN="$(to_win_path "${WORKSPACE_DIR}")"
DATA_PATH_WIN="$(to_win_path "${DATA_PATH}")"
PARQUET_PATH_WIN="$(to_win_path "${PARQUET_PATH}")"
RUN_RSCRIPT_PS_WIN="$(to_win_path "$(to_abs_path "${RUN_RSCRIPT_PS}")")"
TEMPLATE_OVERRIDE_DIR_WIN="$(to_win_path "${TEMPLATE_OVERRIDE_DIR}")"
MIXED_DATASET_LABEL="mixed_models_long"
MIXED_DATA_PATH="${TMP_BASE}/${MIXED_DATASET_LABEL}.csv"
MIXED_DATASET_DIR="${WORKSPACE_DIR}/${MIXED_DATASET_LABEL}"
MIXED_PARQUET_PATH="${MIXED_DATASET_DIR}/${MIXED_DATASET_LABEL}.parquet"
MIXED_APA_REPORT_PATH="${MIXED_DATASET_DIR}/apa_report.md"
MIXED_ANALYSIS_LOG_PATH="${MIXED_DATASET_DIR}/analysis_log.jsonl"
MIXED_DATA_PATH_WIN="$(to_win_path "${MIXED_DATA_PATH}")"
MIXED_PARQUET_PATH_WIN="$(to_win_path "${MIXED_PARQUET_PATH}")"

USE_POWERSHELL=1
if [ -z "${POWERSHELL_BIN}" ]; then
  USE_POWERSHELL=0
else
  if ! "${POWERSHELL_BIN}" -NoProfile -Command "exit 0" >/dev/null 2>&1; then
    USE_POWERSHELL=0
  fi
fi

IS_WSL=0
if [ -n "${WSL_INTEROP:-}" ]; then
  IS_WSL=1
elif [ -r /proc/version ] && rg -qi "microsoft|wsl" /proc/version; then
  IS_WSL=1
fi
if [ "${IS_WSL}" -eq 1 ]; then
  USE_POWERSHELL=0
fi

if [ "${USE_POWERSHELL}" -eq 0 ]; then
  if ! command -v Rscript >/dev/null 2>&1; then
    echo "[FAIL] PowerShell unavailable and Rscript not found." | tee -a "${LOG_PATH}"
    exit 1
  fi
  echo "[INFO] Using Rscript directly." | tee -a "${LOG_PATH}"
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TEMPLATE_OVERRIDE_DIR}" "${TMP_BASE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

CONFIG_BAK="$(mktemp)"
CONFIG_BASE="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

restore_config() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}" "${CONFIG_BASE}"
}

trap restore_config EXIT

CONFIG_OUTPUT_DIR="${WORKSPACE_DIR_WIN}"
if [ "${USE_POWERSHELL}" -eq 0 ]; then
  CONFIG_OUTPUT_DIR="${WORKSPACE_DIR}"
fi
set_config_value defaults.output_dir "${CONFIG_OUTPUT_DIR}"
cp "${CONFIG_PATH}" "${CONFIG_BASE}"

reset_to_base() {
  cp "${CONFIG_BASE}" "${CONFIG_PATH}"
}

HAS_RG=0
if command -v rg >/dev/null 2>&1; then
  HAS_RG=1
fi

assert_marker() {
  local marker="$1"
  local file="$2"
  local allow_missing="${3:-0}"
  local attempts=10
  local found=0
  for ((i = 1; i <= attempts; i++)); do
    if [ -f "${file}" ]; then
      if [ "${HAS_RG}" -eq 1 ]; then
        if rg -n "${marker}" "${file}" >/dev/null 2>&1; then
          found=1
          break
        fi
      else
        if grep -n "${marker}" "${file}" >/dev/null 2>&1; then
          found=1
          break
        fi
      fi
    fi
    sleep 0.3
  done
  if [ "${found}" -ne 1 ]; then
    if [ "${allow_missing}" -eq 1 ]; then
      if [ ! -f "${file}" ]; then
        echo "[WARN] missing output file (optional): ${file}" | tee -a "${LOG_PATH}"
      else
        echo "[WARN] marker not found (optional): ${marker}" | tee -a "${LOG_PATH}"
      fi
      return 0
    else
      if [ ! -f "${file}" ]; then
        echo "[FAIL] missing output file: ${file}" | tee -a "${LOG_PATH}"
      else
        echo "[FAIL] marker not found: ${marker}" | tee -a "${LOG_PATH}"
      fi
      exit 1
    fi
  fi
  return 0
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_PATH}"
  "$@" >>"${LOG_PATH}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
}

run_rscript() {
  local script_path="$1"; shift
  if [ "${USE_POWERSHELL}" -eq 1 ]; then
    local win_script
    win_script="$(to_win_path "$(to_abs_path "$script_path")")"
    "${POWERSHELL_BIN}" -NoProfile -ExecutionPolicy Bypass -File "${RUN_RSCRIPT_PS_WIN}" "${win_script}" "$@"
    return
  fi

  local script_arg="${script_path}"
  if [[ "${script_arg}" =~ ^[A-Za-z]: || "${script_arg}" =~ ^\\\\ ]]; then
    script_arg="$(to_wsl_path "${script_arg}")"
  fi
  local args=()
  local arg
  for arg in "$@"; do
    if [[ "${arg}" =~ ^[A-Za-z]: || "${arg}" =~ ^\\\\ ]]; then
      arg="$(to_wsl_path "${arg}")"
    fi
    args+=("${arg}")
  done
  Rscript "${script_arg}" "${args[@]}"
}

reset_parquet() {
  if [ -f "${PARQUET_PATH}" ]; then
    rm -f "${PARQUET_PATH}"
  fi
}

run_init_workspace() {
  local label="$1"
  reset_parquet
  run_ok "${label}" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"
}

run_init_workspace_custom() {
  local label="$1"; shift
  local data_path_win="$1"
  run_ok "${label}" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${data_path_win}"
}

has_r_package() {
  local pkg="$1"
  set +e
  run_rscript "${CHECK_R_PACKAGE_SCRIPT}" "${pkg}" >/dev/null 2>&1
  local status=$?
  set -e
  return "${status}"
}

prepare_mixed_models_data() {
  run_ok "mixed_models prep" run_rscript "${MIXED_MODELS_PREP_SCRIPT}" "${DATA_PATH_WIN}" "${MIXED_DATA_PATH_WIN}"
}

run_fail() {
  local label="$1"; shift
  echo "[RUN-FAIL] ${label}" | tee -a "${LOG_PATH}"
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} (expected failure)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
}

run_expect() {
  local label="$1"; shift
  local marker="$1"; shift
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_PATH}"
  local start_line
  start_line=$(wc -l < "${LOG_PATH}")
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -ne 0 ]; then
    echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
    return 0
  fi
  local tail_start=$((start_line + 1))
  if [ "${HAS_RG}" -eq 1 ]; then
    if tail -n +"${tail_start}" "${LOG_PATH}" | rg -n "${marker}" >/dev/null 2>&1; then
      echo "[PASS] ${label} (informational)" | tee -a "${LOG_PATH}"
      return 0
    fi
  else
    if tail -n +"${tail_start}" "${LOG_PATH}" | grep -n "${marker}" >/dev/null 2>&1; then
      echo "[PASS] ${label} (informational)" | tee -a "${LOG_PATH}"
      return 0
    fi
  fi
  echo "[FAIL] ${label} (expected failure or info marker: ${marker})" | tee -a "${LOG_PATH}"
  exit 1
}

log_count() {
  local log_file="$1"
  python3 - "$log_file" <<'PY'
import sys
from pathlib import Path
path = Path(sys.argv[1])
if not path.exists():
    print(0)
    sys.exit(0)
count = 0
with path.open("r", encoding="utf-8") as handle:
    for line in handle:
        if line.strip():
            count += 1
print(count)
PY
}

log_has_expected() {
  local log_file="$1"
  local start_count="$2"
  local module="$3"
  local status="$4"
  python3 - "$log_file" "$start_count" "$module" "$status" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
start = int(sys.argv[2])
module = sys.argv[3]
status = sys.argv[4]

if not path.exists():
    sys.exit(1)

index = 0
with path.open("r", encoding="utf-8") as handle:
    for line in handle:
        if not line.strip():
            continue
        index += 1
        if index <= start:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if entry.get("module") != module:
            continue
        results = entry.get("results", {})
        if results.get("status") != status:
            continue
        sys.exit(0)
sys.exit(1)
PY
}

run_expect_log() {
  local label="$1"; shift
  local log_file="$1"; shift
  local module="$1"; shift
  local status="$1"; shift
  local fallback_log="${ROOT_DIR}/core-stats-workspace/${DATASET_LABEL}/analysis_log.jsonl"
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_PATH}"
  local start_count
  local fallback_start
  start_count="$(log_count "${log_file}")"
  if [ "${fallback_log}" != "${log_file}" ]; then
    fallback_start="$(log_count "${fallback_log}")"
  else
    fallback_start="${start_count}"
  fi
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
    return 0
  fi
  local attempts=10
  for ((i = 1; i <= attempts; i++)); do
    if log_has_expected "${log_file}" "${start_count}" "${module}" "${status}"; then
      echo "[PASS] ${label} (informational log)" | tee -a "${LOG_PATH}"
      return 0
    fi
    if [ -f "${fallback_log}" ] && log_has_expected "${fallback_log}" "${fallback_start}" "${module}" "${status}"; then
      echo "[PASS] ${label} (informational log fallback)" | tee -a "${LOG_PATH}"
      return 0
    fi
    sleep 0.3
  done
  echo "[FAIL] ${label} (expected failure or ${status} log)" | tee -a "${LOG_PATH}"
  exit 1
}

resolve_log_path() {
  local out_dir
  out_dir="$(get_config_value defaults.output_dir)"
  if [ -z "${out_dir}" ]; then
    out_dir="${WORKSPACE_DIR}"
  fi
  if [[ "${out_dir}" =~ ^[A-Za-z]:[\\\\/] || "${out_dir}" =~ ^\\\\ ]]; then
    out_dir="$(to_wsl_path "${out_dir}")"
  else
    out_dir="$(to_abs_path "${out_dir}")"
  fi
  echo "${out_dir}/${DATASET_LABEL}/analysis_log.jsonl"
}

resolve_template_source() {
  local key="$1"
  local value
  value="$(get_config_value "templates.${key}")"
  if [ -z "${value}" ]; then
    echo "Missing templates.${key} in config." >&2
    return 1
  fi
  if [[ "${value}" == "~"* ]]; then
    value="${HOME}${value:1}"
  fi
  if [[ "${value}" =~ ^[A-Za-z]:[\\\\/] || "${value}" =~ ^\\\\ ]]; then
    echo "$(to_wsl_path "${value}")"
    return 0
  fi
  if [[ "${value}" == /* ]]; then
    echo "${value}"
    return 0
  fi
  echo "${ROOT_DIR}/core-stats/assets/${value}"
}

template_test_base() {
  local allow_missing="$1"; shift
  local label="$1"; shift
  local key="$1"; shift
  local output_file="$1"; shift
  local marker="${RUN_MARKER}_${key//./_}"
  local source
  local target

  reset_to_base
  if [[ "${key}" == init_workspace.* ]]; then
    reset_parquet
  fi
  source="$(resolve_template_source "${key}")"
  if [ ! -f "${source}" ]; then
    echo "[FAIL] missing template: ${source}" | tee -a "${LOG_PATH}"
    exit 1
  fi

  target="${TEMPLATE_OVERRIDE_DIR}/${key//./_}.md"
  target_win="${TEMPLATE_OVERRIDE_DIR_WIN}/${key//./_}.md"
  cp "${source}" "${target}"
  printf "\n\n%s\n" "${marker}" >> "${target}"
  if [ "${USE_POWERSHELL}" -eq 1 ]; then
    set_config_value "templates.${key}" "${target_win}"
  else
    set_config_value "templates.${key}" "${target}"
  fi
  run_ok "${label}" "$@"
  assert_marker "${marker}" "${output_file}" "${allow_missing}"
  reset_to_base
}

template_test() {
  template_test_base 0 "$@"
}

template_test_optional() {
  template_test_base 1 "$@"
}

cleanup_runs() {
  local keep="${CORE_STATS_KEEP_RUNS:-10}"
  local base="${RUNS_BASE}"
  if ! [[ "${keep}" =~ ^[0-9]+$ ]]; then
    keep=10
  fi
  if [ ! -d "${base}" ]; then
    return 0
  fi
  local runs=()
  while IFS= read -r name; do
    if [[ "${name}" =~ ^[0-9]{14}$ ]]; then
      runs+=("${name}")
    fi
  done < <(find "${base}" -maxdepth 1 -mindepth 1 -type d -printf "%f\n" | sort)
  local count="${#runs[@]}"
  if [ "${count}" -le "${keep}" ]; then
    return 0
  fi
  local remove_count=$((count - keep))
  for ((i = 0; i < remove_count; i++)); do
    rm -rf "${base}/${runs[$i]}"
  done
}

: > "${LOG_PATH}"

if [ ! -f "${DATA_PATH}" ]; then
  echo "[FAIL] missing dataset: ${DATA_PATH}" | tee -a "${LOG_PATH}"
  exit 1
fi

cd "${ROOT_DIR}"

rm -f "${APA_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${ANALYSIS_LOG_PATH}"
run_init_workspace "init workspace"

HAS_LAVAAN=0
if has_r_package "lavaan"; then
  HAS_LAVAAN=1
fi

run_ok "data_explorer clean" run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars id,site,group3,gender,education,cat_var2,ordinal_var
run_ok "descriptive_stats clean" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --group group3 --digits 3
run_ok "frequencies clean" run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender,education,cat_var2,ordinal_var --group group3
run_ok "crosstabs clean" run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3 --percent row --chisq TRUE --expected TRUE --residuals TRUE
run_ok "correlations clean" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --method pearson --missing pairwise
run_ok "scale clean" run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean --omega TRUE
run_ok "assumptions clean" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis regression --dv outcome_anova --ivs x1,x2,x3,mediator
run_ok "regression clean" run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --blocks "x1,x2;x3,mediator" --interactions x1:mediator --center mean --standardize predictors
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  run_ok "sem clean cfa" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
  run_ok "sem clean mediation" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis mediation --x x1 --m mediator --y outcome_reg --bootstrap FALSE
  run_expect_log "sem missing vars" "$(resolve_log_path)" "sem" "invalid_input" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=missing_item1,missing_item2"
else
  run_expect_log "sem missing lavaan" "$(resolve_log_path)" "sem" "missing_dependency" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=f1_1,f1_2"
fi
run_ok "anova clean between" run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
run_ok "t_test clean one-sample" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --mu 0
run_ok "t_test clean independent" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group2
run_ok "t_test clean paired" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x f1_1 --y f1_2

prepare_mixed_models_data
run_init_workspace_custom "init workspace (mixed models)" "${MIXED_DATA_PATH_WIN}"

HAS_LME4=0
if has_r_package "lme4"; then
  HAS_LME4=1
fi
HAS_EMMEANS=0
if has_r_package "emmeans"; then
  HAS_EMMEANS=1
fi

if [ "${HAS_LME4}" -eq 1 ]; then
  run_ok "mixed_models clean" run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + group3 + x1 + (1|id)" --reml TRUE --df-method satterthwaite
  run_ok "mixed_models emmeans" run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise --p-adjust holm
  run_expect_log "mixed_models missing random" "${MIXED_ANALYSIS_LOG_PATH}" "mixed_models" "invalid_input" \
    run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --dv score --fixed time,group3
else
  run_expect_log "mixed_models missing lme4" "${MIXED_ANALYSIS_LOG_PATH}" "mixed_models" "missing_dependency" \
    run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + (1|id)"
fi

run_init_workspace "init workspace (edge)"
run_ok "data_explorer edge" run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,cat_var,high_missing_var,all_missing_var,zero_var,near_zero_var
run_ok "descriptive_stats edge" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars skewed_var,outlier_var,zero_var,near_zero_var,high_missing_var --group group2 --digits 3
run_ok "frequencies edge" run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars cat_var --group group2
run_ok "crosstabs edge" run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row cat_var --col group2 --fisher TRUE --fisher-simulate TRUE --fisher-b 200
run_ok "correlations edge" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --x skewed_var,outlier_var --y age,income --method spearman --missing complete --p-adjust BH
run_ok "scale edge" run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f2_1,f2_2,f2_3,f2_4_rev --reverse f2_4_rev --reverse-min 1 --reverse-max 5 --missing complete --omega FALSE
run_ok "assumptions edge" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis anova --dv outcome_anova --between group3 --within pre_score,mid_score,post_score --subject-id id
run_ok "regression edge bootstrap" run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --ivs x1,x2,x3 --bootstrap TRUE --bootstrap-samples 200 --seed 42
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  run_ok "sem edge path" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis path --dv outcome_reg --ivs skewed_var,outlier_var
fi
run_ok "anova edge mixed" run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --within pre_score,mid_score,post_score --between group3 --subject-id id --posthoc pairwise
run_ok "t_test edge independent" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars pre_score --group group2 --var-equal TRUE
run_ok "t_test edge bootstrap paired" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x pre_score --y post_score --bootstrap TRUE --bootstrap-samples 200 --seed 42

run_init_workspace "init workspace (missings clean)"
run_ok "missings clean" run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --method listwise

run_init_workspace "init workspace (missings edge)"
run_ok "missings edge" run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,satisfaction,outcome_reg,high_missing_var,all_missing_var --method indicator --indicator-threshold 0.2 --drop-threshold 0.5

run_init_workspace "init workspace (transform clean)"
run_ok "data_transform clean" run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --calc "score_avg=(f1_1+f1_2+f1_4)/3" --standardize x1

run_init_workspace "init workspace (transform edge)"
run_ok "data_transform edge" run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --transform "skewed_var=log" --percentile-bins "outlier_var=4" --recode "group3=A:1,B:2,C:3" --drop zero_var --confirm-drop TRUE

template_test "template init_workspace apa" "init_workspace.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"
template_test "template init_workspace scratchpad" "init_workspace.scratchpad" "${SCRATCHPAD_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"

template_test "template descriptive_stats" "descriptive_stats.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2
template_test "template frequencies default" "frequencies.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender,education
template_test "template frequencies grouped" "frequencies.grouped" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender --group group3
template_test "template crosstabs default" "crosstabs.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3
template_test "template crosstabs grouped" "crosstabs.grouped" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3 --group site
template_test "template correlations default" "correlations.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3
template_test "template correlations cross" "correlations.cross" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --x x1,x2 --y x3,mediator
template_test "template scale" "scale.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5
template_test "template data_explorer" "data_explorer.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars id,site,group3
template_test_optional "template data_transform" "data_transform.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --calc "score_avg=(f1_1+f1_2+f1_4)/3"
run_init_workspace "init workspace (templates after transform)"
template_test_optional "template missings" "missings.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,satisfaction --method listwise
run_init_workspace "init workspace (templates after missings)"
template_test "template assumptions ttest" "assumptions.ttest" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --vars x1 --group group2
template_test "template assumptions anova" "assumptions.anova" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis anova --dv outcome_anova --between group3
template_test "template assumptions regression" "assumptions.regression" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis regression --dv outcome_anova --ivs x1,x2,x3
template_test "template regression" "regression.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --ivs x1,x2,x3
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  template_test "template sem default" "sem.default" "${APA_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis path --dv outcome_reg --ivs x1,x2
  template_test "template sem mediation" "sem.mediation" "${APA_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis mediation --x x1 --m mediator --y outcome_reg
else
  echo "[WARN] skipping sem templates (lavaan not installed)" | tee -a "${LOG_PATH}"
fi
if [ "${HAS_LME4}" -eq 1 ]; then
  template_test "template mixed_models default" "mixed_models.default" "${MIXED_APA_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + group3 + x1 + (1|id)"
  if [ "${HAS_EMMEANS}" -eq 1 ]; then
    template_test "template mixed_models emmeans" "mixed_models.emmeans" "${MIXED_APA_REPORT_PATH}" \
      run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise
  else
    echo "[WARN] skipping mixed_models emmeans template (emmeans not installed)" | tee -a "${LOG_PATH}"
  fi
else
  echo "[WARN] skipping mixed_models templates (lme4 not installed)" | tee -a "${LOG_PATH}"
fi
template_test "template anova default" "anova.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
template_test "template anova posthoc" "anova.posthoc" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
template_test "template t_test" "t_test.default" "${APA_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --mu 0

run_expect_log "t_test invalid group levels" "$(resolve_log_path)" "t_test" "expected_invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group3 --expect-two-groups TRUE
run_expect_log "t_test paired with group" "$(resolve_log_path)" "t_test" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x pre_score --y post_score --group group3
run_expect_log "anova missing subject-id" "$(resolve_log_path)" "anova" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --within pre_score,post_score
run_expect_log "regression missing dv" "$(resolve_log_path)" "regression" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --ivs x1,x2

echo "[DONE] smoke tests finished" | tee -a "${LOG_PATH}"
cleanup_runs
