# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env bash
set -euo pipefail

PYTHON_BIN="${PYTHON_BIN:-}"
if [ -z "${PYTHON_BIN}" ]; then
  if command -v python3 >/dev/null 2>&1; then
    PYTHON_BIN="python3"
  elif command -v python >/dev/null 2>&1; then
    PYTHON_BIN="python"
  else
    echo "Python not found. Install Python 3 or set PYTHON_BIN." >&2
    exit 2
  fi
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/scripts/config.yml"
TESTS_CONFIG_PATH="${NLSS_TESTS_CONFIG:-${ROOT_DIR}/tests/tests.yml}"
R_SCRIPT_DIR="${ROOT_DIR}/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/tests/check_regression_log.py"

get_config_value() {
  local path="${CONFIG_PATH}"
  local key="$1"
  if [ $# -gt 1 ]; then
    path="$1"
    key="$2"
  fi
  "${PYTHON_BIN}" - "$path" "$key" <<'PY'
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

get_tests_value() {
  local path="${TESTS_CONFIG_PATH}"
  if [ ! -f "${path}" ]; then
    path="${CONFIG_PATH}"
  fi
  get_config_value "${path}" "$1"
}

set_config_value() {
  "${PYTHON_BIN}" - "$CONFIG_PATH" "$1" "$2" <<'PY'
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
  if [[ "${path}" != /* && ! "${path}" =~ ^[A-Za-z]: ]]; then
    path="${ROOT_DIR}/${path#./}"
  fi
  if command -v cygpath >/dev/null 2>&1; then
    case "$(uname -s)" in
      MINGW*|MSYS*|CYGWIN*) path="$(cygpath -m "${path}")" ;;
    esac
  fi
  echo "${path}"
}

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="nlss-workspace.yml"
fi

DATA_DIR_CFG="$(get_tests_value tests.data_dir)"
if [ -z "${DATA_DIR_CFG}" ]; then
  DATA_DIR_CFG="./tests/data"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_GOLDEN_CFG="$(get_tests_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"
DATA_REGRESSION_SMALL="${DATA_DIR}/regression_data.csv"
DATA_SEMICOLON="${DATA_DIR}/regression_semicolon.csv"
DATA_NO_HEADER="${DATA_DIR}/regression_no_header.csv"
DATA_GROUP_EDGE="${DATA_DIR}/regression_group_edge.csv"

RUNS_BASE_CFG="$(get_tests_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${NLSS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${NLSS_TEST_ROOT}")"
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
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"
DATASET_SMALL_LABEL="$(basename "${DATA_REGRESSION_SMALL}")"
DATASET_SMALL_LABEL="${DATASET_SMALL_LABEL%.*}"
DATASET_SMALL_DIR="${WORKSPACE_DIR}/${DATASET_SMALL_LABEL}"
LOG_PATH_SMALL="${DATASET_SMALL_DIR}/analysis_log.jsonl"
DATASET_SEMI_LABEL="$(basename "${DATA_SEMICOLON}")"
DATASET_SEMI_LABEL="${DATASET_SEMI_LABEL%.*}"
DATASET_SEMI_DIR="${WORKSPACE_DIR}/${DATASET_SEMI_LABEL}"
LOG_PATH_SEMI="${DATASET_SEMI_DIR}/analysis_log.jsonl"
DATASET_NO_HEADER_LABEL="$(basename "${DATA_NO_HEADER}")"
DATASET_NO_HEADER_LABEL="${DATASET_NO_HEADER_LABEL%.*}"
DATASET_NO_HEADER_DIR="${WORKSPACE_DIR}/${DATASET_NO_HEADER_LABEL}"
LOG_PATH_NO_HEADER="${DATASET_NO_HEADER_DIR}/analysis_log.jsonl"
DATASET_GROUP_EDGE_LABEL="$(basename "${DATA_GROUP_EDGE}")"
DATASET_GROUP_EDGE_LABEL="${DATASET_GROUP_EDGE_LABEL%.*}"
DATASET_GROUP_EDGE_DIR="${WORKSPACE_DIR}/${DATASET_GROUP_EDGE_LABEL}"
LOG_PATH_GROUP_EDGE="${DATASET_GROUP_EDGE_DIR}/analysis_log.jsonl"
RDS_LABEL="regression_rds"
RDS_PATH="${TMP_BASE}/${RDS_LABEL}.rds"
RDATA_DF="regression_rdata_df"
RDATA_PATH="${TMP_BASE}/regression_rdata.RData"
DATASET_RDS_DIR="${WORKSPACE_DIR}/${RDS_LABEL}"
LOG_PATH_RDS="${DATASET_RDS_DIR}/analysis_log.jsonl"
DATASET_RDATA_DIR="${WORKSPACE_DIR}/${RDATA_DF}"
LOG_PATH_RDATA="${DATASET_RDATA_DIR}/analysis_log.jsonl"

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

cleanup() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}"
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

: > "${LOG_FILE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_REGRESSION_SMALL}" ]; then
  echo "[FAIL] missing dataset: ${DATA_REGRESSION_SMALL}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_SEMICOLON}" ]; then
  echo "[FAIL] missing dataset: ${DATA_SEMICOLON}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_NO_HEADER}" ]; then
  echo "[FAIL] missing dataset: ${DATA_NO_HEADER}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_GROUP_EDGE}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GROUP_EDGE}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
rm -f "${NLSS_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${LOG_PATH}"
cd "${WORKSPACE_DIR}"

Rscript - "${DATA_REGRESSION_SMALL}" "${RDS_PATH}" "${RDATA_PATH}" "${RDATA_DF}" >>"${LOG_FILE}" 2>&1 <<'RS'
args <- commandArgs(trailingOnly = TRUE)
csv_path <- args[1]
rds_path <- args[2]
rdata_path <- args[3]
df_name <- args[4]
df <- read.csv(csv_path, stringsAsFactors = FALSE)
saveRDS(df, rds_path)
assign(df_name, df)
save(list = df_name, file = rdata_path)
RS

log_count() {
  "${PYTHON_BIN}" - "$1" <<'PY'
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
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${LOG_PATH}" "${start_count}" "$@"
}

check_log_path() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" "$@"
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

run_expect_invalid_path() {
  local label="$1"; shift
  local log_path="$1"; shift
  local status="$1"; shift
  echo "[RUN-EXPECT-INVALID] ${label}" | tee -a "${LOG_FILE}"
  local start_count
  start_count="$(log_count "${log_path}")"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  if check_log_path "${log_path}" "${start_count}" "${status}" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" ; then
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

assert_log_unchanged() {
  local before="$1"
  local after="$2"
  local label="$3"
  if [ "${before}" -ne "${after}" ]; then
    echo "[FAIL] ${label} (expected log count unchanged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

resolve_template_source() {
  local value="$1"
  if [[ "${value}" == "~"* ]]; then
    value="${HOME}${value:1}"
  fi
  if [[ "${value}" == /* || "${value}" =~ ^[A-Za-z]: ]]; then
    echo "${value}"
    return 0
  fi
  echo "${ROOT_DIR}/assets/${value}"
}

cleanup_runs() {
  local keep="${NLSS_KEEP_RUNS:-10}"
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
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "f2=present"

start=$(log_count "${LOG_PATH}")
run_ok "ols workspace active dataset" Rscript "${R_SCRIPT_DIR}/regression.R" --dv outcome_reg --ivs x1,x2
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true"

start=$(log_count "${LOG_PATH}")
run_ok "ols hierarchical blocks" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --blocks "x1,x2;x3"
check_log "${start}" "-" "gaussian" "t" "2" "-" "1" "-" "-" "-" "-" "true" "f2=present" "delta_f2=present"

start=$(log_count "${LOG_PATH}")
run_ok "ols interaction with centering" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --interactions x1:mediator --center mean
check_log "${start}" "-" "gaussian" "t" "2" "-" "1" "-" "-" "-" "true" "true" "center=mean"

start=$(log_count "${LOG_PATH}")
run_ok "ols grouped by site" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --group site
check_log "${start}" "-" "gaussian" "t" "1" "2" "-" "-" "-" "-" "-" "true" "group=site"

start=$(log_count "${LOG_PATH}")
run_ok "ols standardized betas" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2,x3 --standardize predictors
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "true" "-" "-" "-" "true" "standardize=predictors"

start=$(log_count "${LOG_PATH}")
run_ok "ols bootstrap CI" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --bootstrap TRUE --bootstrap-samples 50 --seed 123
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "true" "-" "true" "bootstrap_samples=50"

start=$(log_count "${LOG_PATH}")
run_ok "ols bootstrap zero samples" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --bootstrap TRUE --bootstrap-samples 0 --seed 123
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "false" "-" "true" "bootstrap_samples=0"

start=$(log_count "${LOG_PATH_SMALL}")
run_ok "ols auto ivs (numeric default)" Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_REGRESSION_SMALL}" --dv y
check_log_path "${LOG_PATH_SMALL}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "terms=x1,x2"

start=$(log_count "${LOG_PATH_SMALL}")
run_ok "ols custom conf + digits + user prompt" Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_REGRESSION_SMALL}" --dv y --ivs x1,x2 --conf-level 0.9 --digits 4 --user-prompt regression_test_prompt
check_log_path "${LOG_PATH_SMALL}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "conf_level=0.9" "digits=4" "user_prompt=regression_test_prompt"

start=$(log_count "${LOG_PATH_NO_HEADER}")
run_ok "ols csv header false" Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_NO_HEADER}" --header FALSE --dv V1 --ivs V2,V3
check_log_path "${LOG_PATH_NO_HEADER}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "terms=V2,V3"

start=$(log_count "${LOG_PATH_SEMI}")
run_ok "ols csv custom sep" Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_SEMICOLON}" --sep ";" --dv dv --ivs x1,x2
check_log_path "${LOG_PATH_SEMI}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "terms=x1,x2"

start=$(log_count "${LOG_PATH_RDS}")
run_ok "ols rds input (factor predictor)" Rscript "${R_SCRIPT_DIR}/regression.R" --rds "${RDS_PATH}" --dv y --ivs x1,f1
check_log_path "${LOG_PATH_RDS}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "terms=x1"

start=$(log_count "${LOG_PATH_RDATA}")
run_ok "ols rdata input (df name)" Rscript "${R_SCRIPT_DIR}/regression.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --dv y --ivs x1,x2
check_log_path "${LOG_PATH_RDATA}" "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true" "terms=x1,x2"

start=$(log_count "${LOG_PATH}")
run_ok "logistic regression (numeric DV)" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1,x2 --family binomial
check_log "${start}" "-" "binomial" "z" "1" "-" "-" "-" "true" "-" "-" "false"

start=$(log_count "${LOG_PATH}")
run_ok "logistic regression (factor DV)" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv group2 --ivs x1,x2 --family binomial
check_log "${start}" "-" "binomial" "z" "1" "-" "-" "-" "true" "-" "-" "false"

start=$(log_count "${LOG_PATH}")
run_ok "logistic blocks + interactions" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1 --interactions x1:mediator --family binomial
check_log "${start}" "-" "binomial" "z" "2" "-" "1" "-" "true" "-" "true" "false"

start=$(log_count "${LOG_PATH}")
run_ok "logistic grouped by site" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1,x2 --family binomial --group site
check_log "${start}" "-" "binomial" "z" "1" "2" "-" "-" "true" "-" "-" "false" "group=site"

start=$(log_count "${LOG_PATH}")
run_ok "logistic probit bootstrap" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1,x2 --family binomial --link probit --bootstrap TRUE --bootstrap-samples 20 --seed 123
check_log "${start}" "-" "binomial" "z" "1" "-" "-" "-" "true" "true" "-" "false" "link=probit" "bootstrap_samples=20"

start=$(log_count "${LOG_PATH}")
run_ok "poisson regression (blocks)" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv count_outcome --blocks "x1;x2" --family poisson
check_log "${start}" "-" "poisson" "z" "2" "-" "1" "-" "true" "-" "-" "false"

TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.regression.default)"
if [ -z "${TEMPLATE_DEFAULT_ORIG}" ]; then
  TEMPLATE_DEFAULT_ORIG="regression/default-template.md"
fi
TEMPLATE_DEFAULT_TMP="${TMP_BASE}/regression_default_template.md"

cp "$(resolve_template_source "${TEMPLATE_DEFAULT_ORIG}")" "${TEMPLATE_DEFAULT_TMP}"
printf "\n\nREGRESSION_TEMPLATE_DEFAULT\n" >>"${TEMPLATE_DEFAULT_TMP}"

set_config_value templates.regression.default "${TEMPLATE_DEFAULT_TMP}"

start=$(log_count "${LOG_PATH}")
run_ok "template override" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2
check_log "${start}" "-" "gaussian" "t" "1" "-" "-" "-" "-" "-" "-" "true"

assert_contains "${NLSS_REPORT_PATH}" "REGRESSION_TEMPLATE_DEFAULT"

set_config_value templates.regression.default "${TEMPLATE_DEFAULT_ORIG}"

start=$(log_count "${LOG_PATH}")
run_ok "ols log disabled" Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1,x2 --log FALSE
end=$(log_count "${LOG_PATH}")
assert_log_unchanged "${start}" "${end}" "ols log disabled"

run_expect_invalid "missing dv" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --ivs x1
run_expect_invalid "dv not found" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv not_a_var --ivs x1
run_expect_invalid "unknown predictor" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs not_a_var
run_expect_invalid "unknown interaction" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --interactions x1:not_a_var
run_expect_invalid "invalid binomial link" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv binary_outcome --ivs x1 --family binomial --link badlink
run_expect_invalid "non-numeric dv gaussian" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv cat_var --ivs x1
run_expect_invalid "non-numeric dv poisson" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv cat_var --ivs x1 --family poisson
run_expect_invalid "ivs identical to dv" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs outcome_reg
run_expect_invalid "no complete cases" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs all_missing_var
run_expect_invalid "unknown group" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_reg --ivs x1 --group not_a_group
run_expect_invalid_path "group with <3 complete cases" "${LOG_PATH_GROUP_EDGE}" invalid_input Rscript "${R_SCRIPT_DIR}/regression.R" --csv "${DATA_GROUP_EDGE}" --dv dv --ivs x1 --group group

echo "Regression tests: OK" | tee -a "${LOG_FILE}"
cleanup_runs
