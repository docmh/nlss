#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/outputs/tests/check_regression_log.py"
TEMPLATE_PATH="${ROOT_DIR}/core-stats/assets/regression/default-template.md"

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

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="core-stats-workspace.yml"
fi

DATA_DIR_CFG="$(get_config_value tests.data_dir)"
if [ -z "${DATA_DIR_CFG}" ]; then
  DATA_DIR_CFG="./outputs/tests"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_GOLDEN_CFG="$(get_config_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"

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

WORKSPACE_DIR="${RUN_ROOT}/regression_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/regression"
LOG_FILE="${RUN_ROOT}/regression_test.log"

mkdir -p "${RUN_ROOT}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
APA_REPORT_PATH="${DATASET_DIR}/apa_report.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"
TEMPLATE_BAK="${TMP_BASE}/regression-template-backup.md"

cleanup() {
  rm -f "${WORKSPACE_MANIFEST_PATH}"
  if [ -f "${TEMPLATE_BAK}" ]; then
    mv -f "${TEMPLATE_BAK}" "${TEMPLATE_PATH}"
  fi
}
trap cleanup EXIT

: > "${LOG_FILE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R or use scripts/run_rscript.ps1 on Windows." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
rm -f "${APA_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${LOG_PATH}"
cd "${WORKSPACE_DIR}"

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

check_log() {
  local start_count="$1"; shift
  python3 "${CHECK_SCRIPT}" "${LOG_PATH}" "${start_count}" "$@"
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
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
  if check_log "${start_count}" "${status}" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" ; then
    echo "[PASS] ${label} (status ${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

assert_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if ! rg -q --fixed-strings "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  else
    if ! grep -qF "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  fi
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

if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

start=$(log_count "${LOG_PATH}")
run_ok "ols basic (csv input)" Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_GOLDEN}" --dv outcome_reg --ivs x1,x2,x3
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols hierarchical blocks" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --blocks "x1,x2;x3"
check_log "${start}" "-" "gaussian" "t" "2" "-" "1" "-" "-" "-" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols interaction with centering" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --interactions x1:mediator --center mean
check_log "${start}" "-" "gaussian" "t" "2" "-" "1" "-" "-" "-" "true" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols grouped by site" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --group site
check_log "${start}" "-" "gaussian" "t" "1" "2" "-" "-" "-" "-" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols standardized betas" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2,x3 --standardize predictors
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "true" "-" "-" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols bootstrap CI" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --bootstrap TRUE --bootstrap-samples 50 --seed 123
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "true" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "logistic regression (numeric DV)" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1,x2 --family binomial
check_log "${start}" "-" "binomial" "z" "1" "-" "-" "-" "true" "-" "-" "-"

start=$(log_count "${LOG_PATH}")
run_ok "logistic regression (factor DV)" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv group2 --ivs x1,x2 --family binomial
check_log "${start}" "-" "binomial" "z" "1" "-" "-" "-" "true" "-" "-" "-"

start=$(log_count "${LOG_PATH}")
run_ok "poisson regression" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv count_outcome --ivs x1,x2 --family poisson
check_log "${start}" "-" "poisson" "z" "1" "-" "-" "-" "true" "-" "-" "-"

if [ -f "${TEMPLATE_PATH}" ]; then
  cp "${TEMPLATE_PATH}" "${TEMPLATE_BAK}"
  sed -i 's/title: "Regression"/title: "Regression TEMPLATE TEST"/' "${TEMPLATE_PATH}"
  sed -i 's/table_title: "Regression coefficients\."/table_title: "Regression coefficients TEMPLATE TEST\."/' "${TEMPLATE_PATH}"
  sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_PATH}"

  start=$(log_count "${LOG_PATH}")
  run_ok "template override" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2
  check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true"

  if [ ! -f "${APA_REPORT_PATH}" ]; then
    echo "[FAIL] missing report: ${APA_REPORT_PATH}" | tee -a "${LOG_FILE}"
    exit 1
  fi
  assert_contains "${APA_REPORT_PATH}" "Regression TEMPLATE TEST"
  assert_contains "${APA_REPORT_PATH}" "Regression coefficients TEMPLATE TEST."
  assert_contains "${APA_REPORT_PATH}" "*Note-TEST.*"

  mv -f "${TEMPLATE_BAK}" "${TEMPLATE_PATH}"
fi

run_expect_invalid "missing dv" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --ivs x1
run_expect_invalid "unknown predictor" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs not_a_var
run_expect_invalid "unknown interaction" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --interactions x1:not_a_var
run_expect_invalid "non-numeric dv gaussian" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv cat_var --ivs x1
run_expect_invalid "ivs identical to dv" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs outcome_reg
run_expect_invalid "no complete cases" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs all_missing_var
run_expect_invalid "unknown group" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --group not_a_group

echo "Regression tests: OK" | tee -a "${LOG_FILE}"
cleanup_runs
