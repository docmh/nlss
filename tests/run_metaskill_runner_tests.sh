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

RUNS_BASE_CFG="$(get_tests_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"

RUN_ID="$(date +%Y%m%d%H%M%S)"
if [ -n "${NLSS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${NLSS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
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

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="nlss-workspace.yml"
fi

WORKSPACE_DIR="${RUN_ROOT}/metaskill_runner_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
LOG_FILE="${RUN_ROOT}/metaskill_runner_test.log"
TMP_BASE="${RUN_ROOT}/tmp/metaskill_runner"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

log() {
  echo "$@" | tee -a "${LOG_FILE}"
}

run_ok() {
  local label="$1"; shift
  if "$@" >>"${LOG_FILE}" 2>&1; then
    log "[PASS] ${label}"
  else
    log "[FAIL] ${label}"
    exit 1
  fi
}

run_expect_fail() {
  local label="$1"; shift
  if "$@" >>"${LOG_FILE}" 2>&1; then
    log "[FAIL] ${label} (unexpected success)"
    exit 1
  else
    log "[PASS] ${label}"
  fi
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

assert_contains() {
  local label="$1"
  local file="$2"
  local needle="$3"
  if "${PYTHON_BIN}" - "$file" "$needle" <<'PY'
import sys
from pathlib import Path
path = Path(sys.argv[1])
needle = sys.argv[2]
if not path.exists():
    sys.exit(1)
text = path.read_text(encoding="utf-8")
if needle not in text:
    sys.exit(1)
PY
  then
    log "[PASS] ${label}"
  else
    log "[FAIL] ${label}"
    exit 1
  fi
}

if ! command -v Rscript >/dev/null 2>&1; then
  log "[FAIL] Rscript not found. Install R and ensure Rscript is on PATH."
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  log "[FAIL] missing dataset: ${DATA_GOLDEN}"
  exit 1
fi

rm -f "${NLSS_REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
cd "${WORKSPACE_DIR}"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [ ! -f "${PARQUET_PATH}" ]; then
  log "[FAIL] missing parquet copy: ${PARQUET_PATH}"
  exit 1
fi

start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "metaskill_runner basic" Rscript "${R_SCRIPT_DIR}/metaskill_runner.R" \
  --parquet "${PARQUET_PATH}" --meta sample-description --intent "describe the sample" --notes "auto selection"

assert_contains "nlss_report includes metaskill name" "${NLSS_REPORT_PATH}" "sample-description"
assert_contains "analysis_log includes module" "${ANALYSIS_LOG_PATH}" "\"module\":\"metaskill_runner\""

after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
if [ "${after_count}" -le "${start_count}" ]; then
  log "[FAIL] analysis_log did not append"
  exit 1
else
  log "[PASS] analysis_log appended"
fi

start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "metaskill_runner no log" Rscript "${R_SCRIPT_DIR}/metaskill_runner.R" \
  --parquet "${PARQUET_PATH}" --meta sample-description --log FALSE
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
if [ "${after_count}" -ne "${start_count}" ]; then
  log "[FAIL] log should not append when --log FALSE"
  exit 1
else
  log "[PASS] log unchanged when --log FALSE"
fi

run_expect_fail "metaskill_runner missing meta" Rscript "${R_SCRIPT_DIR}/metaskill_runner.R" --parquet "${PARQUET_PATH}"

log "Metaskill runner tests complete."
