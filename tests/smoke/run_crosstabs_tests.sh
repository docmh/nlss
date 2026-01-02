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
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/scripts/config.yml"
TESTS_CONFIG_PATH="${NLSS_TESTS_CONFIG:-${ROOT_DIR}/tests/tests.yml}"
R_SCRIPT_DIR="${ROOT_DIR}/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_crosstabs_log.py"

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

WORKSPACE_DIR="${RUN_ROOT}/crosstabs_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
LOG_FILE="${RUN_ROOT}/crosstabs_test.log"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}"
: > "${LOG_FILE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"

rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}"
mkdir -p "${DATASET_DIR}"
: > "${WORKSPACE_MANIFEST_PATH}"
cd "${WORKSPACE_DIR}"

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

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

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

start=$(log_count "${LOG_PATH}")
run_ok "crosstabs cramers_v (3x3)" Rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_GOLDEN}" --row gender --col group3 --chisq TRUE --expected TRUE --residuals TRUE
check_log "${start}" "gender" "group3" "-" "absent" "present" "present"
assert_contains "${NLSS_REPORT_PATH}" "Cramer's V"

start=$(log_count "${LOG_PATH}")
run_ok "crosstabs phi (2x2)" Rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_GOLDEN}" --row binary_outcome --col group2 --chisq TRUE
check_log "${start}" "binary_outcome" "group2" "-" "present" "present" "present"
assert_contains "${NLSS_REPORT_PATH}" "phi"

echo "Crosstabs tests: OK" | tee -a "${LOG_FILE}"
