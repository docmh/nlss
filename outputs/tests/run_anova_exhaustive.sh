#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/outputs/tests/check_anova_log.py"
DATA_GOLDEN="${ROOT_DIR}/outputs/tests/golden_dataset.csv"
DATA_EDGE="${ROOT_DIR}/outputs/tests/anova_edge_dataset.csv"
DATA_MISSING="${ROOT_DIR}/outputs/tests/anova_all_missing.csv"

DATASET_GOLDEN_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_GOLDEN_LABEL="${DATASET_GOLDEN_LABEL%.*}"
DATASET_EDGE_LABEL="$(basename "${DATA_EDGE}")"
DATASET_EDGE_LABEL="${DATASET_EDGE_LABEL%.*}"
DATASET_MISSING_LABEL="$(basename "${DATA_MISSING}")"
DATASET_MISSING_LABEL="${DATASET_MISSING_LABEL%.*}"

DATASET_GOLDEN_DIR="${WORKSPACE_DIR}/${DATASET_GOLDEN_LABEL}"
DATASET_EDGE_DIR="${WORKSPACE_DIR}/${DATASET_EDGE_LABEL}"
DATASET_MISSING_DIR="${WORKSPACE_DIR}/${DATASET_MISSING_LABEL}"

APA_REPORT_PATH="${DATASET_GOLDEN_DIR}/apa_report.md"
SCRATCHPAD_PATH="${DATASET_GOLDEN_DIR}/scratchpad.md"
LOG_PATH="${DATASET_GOLDEN_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_GOLDEN_DIR}/${DATASET_GOLDEN_LABEL}.parquet"
PARQUET_EDGE="${DATASET_EDGE_DIR}/${DATASET_EDGE_LABEL}.parquet"
PARQUET_MISSING="${DATASET_MISSING_DIR}/${DATASET_MISSING_LABEL}.parquet"

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

RUNS_BASE_CFG="$(get_config_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${CORE_STATS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${CORE_STATS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
fi

WORKSPACE_DIR="${RUN_ROOT}/anova_workspace"
TMP_BASE="${RUN_ROOT}/tmp/anova"
LOG_FILE="${RUN_ROOT}/anova_exhaustive.log"

mkdir -p "${RUN_ROOT}"

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

restore_config() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}"
}
trap restore_config EXIT

: > "${LOG_FILE}"

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_EDGE}" ]; then
  echo "[FAIL] missing dataset: ${DATA_EDGE}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_MISSING}" ]; then
  echo "[FAIL] missing dataset: ${DATA_MISSING}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_GOLDEN_DIR}" "${DATASET_EDGE_DIR}" "${DATASET_MISSING_DIR}"
mkdir -p "${TMP_BASE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
rm -f "${APA_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${LOG_PATH}"

set_config_value defaults.output_dir "${WORKSPACE_DIR}"

log_count() {
  python3 - "$1" <<'PY'
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

check_anova_log() {
  local start_count="$1"; shift
  local mode="$1"; shift
  local posthoc="$1"; shift
  local posthoc_rows="$1"; shift
  local effect_size="$1"; shift
  local sphericity="$1"; shift
  local status="$1"; shift
  local boot_ci="${1:--}"; shift || true
  python3 "${CHECK_SCRIPT}" "${LOG_PATH}" "${start_count}" "${mode}" "${posthoc}" "${posthoc_rows}" "${effect_size}" "${sphericity}" "${status}" "${boot_ci}"
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

run_expect_fail() {
  local label="$1"; shift
  echo "[RUN-EXPECT-FAIL] ${label}" | tee -a "${LOG_FILE}"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_FILE}"
}

run_expect_invalid() {
  local label="$1"; shift
  local status="$1"; shift
  echo "[RUN-EXPECT-INVALID] ${label}" | tee -a "${LOG_FILE}"
  local start_count
  start_count="$(log_count "${LOG_PATH}")"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  if check_anova_log "${start_count}" "-" "-" "-" "-" "-" "${status}"; then
    echo "[PASS] ${label} (status ${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

assert_log_unchanged() {
  local label="$1"; shift
  local start_count="$1"; shift
  local end_count
  end_count="$(log_count "${LOG_PATH}")"
  if [ "${start_count}" -ne "${end_count}" ]; then
    echo "[FAIL] ${label} (log changed unexpectedly)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "[PASS] ${label} (log unchanged)" | tee -a "${LOG_FILE}"
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

run_ok "init workspace (golden dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
run_ok "init workspace (edge dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_EDGE}"
run_ok "init workspace (all missing dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_MISSING}"

# Positive tests
start=$(log_count "${LOG_PATH}")
run_ok "anova between default" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between factorial covariate type III" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3,gender --covariates age --type III --effect-size eta_sq --posthoc pairwise --p-adjust bonferroni --conf-level 0.9 --user-prompt "anova factorial test"
check_anova_log "${start}" "between" "pairwise" "gt0" "eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between posthoc none" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --posthoc none
check_anova_log "${start}" "between" "none" "0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within sphericity auto" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --posthoc pairwise
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "present" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within sphericity none" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --sphericity none --posthoc pairwise
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "absent" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova mixed with covariate" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --between group3 --subject-id id --covariates age --posthoc pairwise --p-adjust BH --sphericity none
check_anova_log "${start}" "mixed" "pairwise" "gt0" "partial_eta_sq" "absent" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within posthoc tukey requested" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --posthoc tukey
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova CSV input (edge dataset)" Rscript "${R_SCRIPT_DIR}/anova.R" --csv "${DATA_EDGE}" --dv dv_num --between group --posthoc tukey
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between bootstrap" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --bootstrap TRUE --bootstrap-samples 200
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" "present" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova log false" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --log FALSE
assert_log_unchanged "anova log false" "${start}"

# Negative tests (invalid input with logged status)
run_expect_invalid "anova missing subject-id" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score
run_expect_invalid "anova within length <2" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score --subject-id id
run_expect_invalid "anova missing dv" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --between group3
run_expect_invalid "anova missing between/within" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova
run_expect_invalid "anova overlap within and dv" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score --subject-id id --dv pre_score

# Negative tests (expected failures without logged status)
run_expect_fail "anova nonnumeric dv" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv dv_char --between group
run_expect_fail "anova nonnumeric within" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --within pre_text,post --subject-id id
run_expect_fail "anova nonnumeric covariate" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv dv_num --between group --covariates covar_char
run_expect_fail "anova unknown dv" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv not_a_var --between group
run_expect_fail "anova no complete cases" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_MISSING}" --dv dv_num --between group

echo "[DONE] ANOVA exhaustive tests finished" | tee -a "${LOG_FILE}"
cleanup_runs
