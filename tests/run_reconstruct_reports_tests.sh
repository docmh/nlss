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
RECONSTRUCT_SCRIPT="${R_SCRIPT_DIR}/reconstruct_reports.R"

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

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
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

WORKSPACE_DIR="${RUN_ROOT}/reconstruct_reports_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
LOG_FILE="${RUN_ROOT}/reconstruct_reports_test.log"
TMP_BASE="${RUN_ROOT}/tmp/reconstruct_reports"
OUT_DIR="${TMP_BASE}/out"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"

BROKEN_LOG_PATH="${TMP_BASE}/analysis_log_broken.jsonl"
OLD_LOG_PATH="${TMP_BASE}/analysis_log_old.jsonl"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}" "${OUT_DIR}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_RG=0
if command -v rg >/dev/null 2>&1; then
  HAS_RG=1
fi

assert_marker() {
  local marker="$1"
  local file="$2"
  if [ ! -f "${file}" ]; then
    echo "[FAIL] missing output file: ${file}" | tee -a "${LOG_FILE}"
    exit 1
  fi
  if [ "${HAS_RG}" -eq 1 ]; then
    if rg -n -F -- "${marker}" "${file}" >/dev/null 2>&1; then
      return 0
    fi
  else
    if grep -n -F -- "${marker}" "${file}" >/dev/null 2>&1; then
      return 0
    fi
  fi
  echo "[FAIL] marker not found: ${marker}" | tee -a "${LOG_FILE}"
  exit 1
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

run_fail() {
  local label="$1"; shift
  echo "[RUN-FAIL] ${label}" | tee -a "${LOG_FILE}"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} (expected failure)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_FILE}"
}

cd "${WORKSPACE_DIR}"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
run_ok "descriptive_stats" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH}" --vars x1,x2

DATE_UTC="$(date -u +%Y%m%d 2>/dev/null || date +%Y%m%d)"
ORIGINAL_METASKILL_REPORT_PATH="${DATASET_DIR}/report_${DATE_UTC}_sample-description_describe_the_sample.md"
DEFAULT_METASKILL_REPORT_PATH="${DATASET_DIR}/report_${DATE_UTC}_sample-description_describe_the_sample_reconstructed.md"

cat > "${ORIGINAL_METASKILL_REPORT_PATH}" <<'EOF'
# Dummy Metaskill Report

This is a placeholder for a metaskill report generated outside the runner.
EOF
assert_marker "# Dummy Metaskill Report" "${ORIGINAL_METASKILL_REPORT_PATH}"

SYNOPSIS_TEXT="Synopsis from runner"
run_ok "metaskill finalization" Rscript "${R_SCRIPT_DIR}/metaskill_runner.R" --parquet "${PARQUET_PATH}" --meta sample-description --intent "describe the sample" --phase finalization --synopsis "${SYNOPSIS_TEXT}"

DEFAULT_REPORT_PATH="${DATASET_DIR}/report_canonical_reconstructed.md"

run_ok "reconstruct default" Rscript "${RECONSTRUCT_SCRIPT}" "${ANALYSIS_LOG_PATH}"
assert_marker "# Descriptive Statistics" "${DEFAULT_REPORT_PATH}"
assert_marker "${SYNOPSIS_TEXT}" "${DEFAULT_REPORT_PATH}"
assert_marker "# Dummy Metaskill Report" "${DEFAULT_METASKILL_REPORT_PATH}"

cp "${ANALYSIS_LOG_PATH}" "${BROKEN_LOG_PATH}"
printf '%s\n' '{bad json' >> "${BROKEN_LOG_PATH}"
printf '%s\n' '{"report_block_b64":"not-base64","report_block_encoding":"gzip+base64"}' >> "${BROKEN_LOG_PATH}"

BROKEN_REPORT_PATH="${OUT_DIR}/report_canonical_reconstructed.md"
run_ok "reconstruct broken entries" Rscript "${RECONSTRUCT_SCRIPT}" --out-dir "${OUT_DIR}" "${BROKEN_LOG_PATH}"
assert_marker "# Descriptive Statistics" "${BROKEN_REPORT_PATH}"
assert_marker "invalid JSON" "${LOG_FILE}"
assert_marker "base64 decode failed" "${LOG_FILE}"

printf '%s\n' '{"module":"descriptive_stats","prompt":"legacy"}' > "${OLD_LOG_PATH}"
run_fail "reconstruct legacy log" Rscript "${RECONSTRUCT_SCRIPT}" "${OLD_LOG_PATH}"
assert_marker "older logs are not supported" "${LOG_FILE}"

echo "[DONE] reconstruct_reports tests finished" | tee -a "${LOG_FILE}"
