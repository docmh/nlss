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

TMP_BASE="${RUN_ROOT}/tmp/calc"
LOG_FILE="${RUN_ROOT}/calc_test.log"
PROMPT_FILE="${TMP_BASE}/calc_prompt.txt"
OUT_DIR_CFG="$(get_config_value defaults.output_dir)"

mkdir -p "${RUN_ROOT}" "${TMP_BASE}"
: > "${LOG_FILE}"

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

cat > "${PROMPT_FILE}" <<'EOF'
alpha_adj=0.05/3

4
plain
FALSE
EOF

if [ -z "${OUT_DIR_CFG}" ]; then
  OUT_DIR_CFG="./outputs/tmp"
fi
if [[ "${OUT_DIR_CFG}" == /* || "${OUT_DIR_CFG}" =~ ^[A-Za-z]: ]]; then
  OUT_DIR="${OUT_DIR_CFG}"
else
  OUT_DIR="${RUN_ROOT}/${OUT_DIR_CFG#./}"
fi
REPORT_PATH="${OUT_DIR}/report_canonical.md"
ANALYSIS_LOG_PATH="${OUT_DIR}/analysis_log.jsonl"

cd "${RUN_ROOT}"

run_ok "calc basic" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "alpha_adj=0.05/3" --digits 4
assert_marker "alpha_adj = 0.0167" "${LOG_FILE}"

run_ok "calc json" Rscript "${R_SCRIPT_DIR}/calc.R" --set "r=0.3" --expr "d=2*r/sqrt(1-r^2)" --digits 3 --format json
assert_marker '"d":0.629' "${LOG_FILE}"
assert_marker "\"module\":\"calc\"" "${ANALYSIS_LOG_PATH}"
assert_marker "Calculator Output" "${REPORT_PATH}"

run_ok "calc csv" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "val=1/3" --digits 3 --format csv
assert_marker "name,value" "${LOG_FILE}"
assert_marker "val,0.333" "${LOG_FILE}"

run_ok "calc multi expr" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "sqrt(0.2)|log(10)" --digits 3
assert_marker "expr_2 = 2.303" "${LOG_FILE}"

run_ok "calc unsafe vector" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "pnorm(c(-1,0,1))" --unsafe TRUE --digits 3
assert_marker "0.159, 0.500, 0.841" "${LOG_FILE}"

run_ok "calc interactive" env NLSS_PROMPT_FILE="${PROMPT_FILE}" \
  Rscript "${R_SCRIPT_DIR}/calc.R" --interactive
assert_marker "alpha_adj = 0.0167" "${LOG_FILE}"

run_fail "calc invalid expr" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "1+"
run_fail "calc invalid set" Rscript "${R_SCRIPT_DIR}/calc.R" --set "1a=3" --expr "1"
run_fail "calc restricted function" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "c(1,2)"
run_fail "calc bad format" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "1" --format xml
run_fail "calc bad digits" Rscript "${R_SCRIPT_DIR}/calc.R" --expr "1" --digits -1

echo "[DONE] calc tests finished" | tee -a "${LOG_FILE}"
