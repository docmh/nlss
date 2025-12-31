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
COMPREHENSIVE_TEMPLATE_PATH="${ROOT_DIR}/assets/research-academia/comprehensive-template.md"

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

TMP_BASE="${RUN_ROOT}/tmp/research_academia"
LOG_FILE="${RUN_ROOT}/research_academia_test.log"
TEMPLATE_PATH="${TMP_BASE}/research_template.md"

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

assert_regex() {
  local pattern="$1"
  local file="$2"
  if [ ! -f "${file}" ]; then
    echo "[FAIL] missing output file: ${file}" | tee -a "${LOG_FILE}"
    exit 1
  fi
  if [ "${HAS_RG}" -eq 1 ]; then
    if rg -n -i --regexp "${pattern}" "${file}" >/dev/null 2>&1; then
      return 0
    fi
  else
    if grep -n -i -E -- "${pattern}" "${file}" >/dev/null 2>&1; then
      return 0
    fi
  fi
  echo "[FAIL] regex not found: ${pattern}" | tee -a "${LOG_FILE}"
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

OUT_DIR_CFG="$(get_config_value defaults.output_dir)"
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

run_fail "research web disabled" Rscript "${R_SCRIPT_DIR}/research_academia.R" --query "effect size" --web FALSE
run_fail "research missing query" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --sources openalex
run_fail "research invalid sources" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --query "effect size" --sources invalid_source
run_fail "research invalid max-per-source" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --query "effect size" --sources openalex --max-per-source 0
run_fail "research invalid max-total" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --query "effect size" --sources openalex --max-total 0
run_fail "research invalid top-n" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --query "effect size" --sources openalex --top-n 0
run_fail "research invalid timeout" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" --web TRUE --query "effect size" --sources openalex --timeout 0

ALLOW_WEB="${NLSS_TEST_ALLOW_WEB:-}"
if [ -z "${ALLOW_WEB}" ]; then
  if [ "${NLSS_WEB_SEARCH:-}" = "1" ] || [ "${NLSS_WEB_SEARCH:-}" = "TRUE" ] || [ "${NLSS_WEB_SEARCH:-}" = "true" ]; then
    ALLOW_WEB="1"
  else
    ALLOW_WEB="0"
  fi
fi

if [ "${ALLOW_WEB}" = "1" ]; then
  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research basic" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex,crossref --top-n 5 --max-per-source 5 --max-total 10 --timeout 20 --web TRUE
  assert_marker "\"module\":\"research-academia\"" "${ANALYSIS_LOG_PATH}"
  assert_marker "Research (Academia)" "${REPORT_PATH}"
  assert_marker "Most Relevant" "${REPORT_PATH}"
  assert_regex "Sources:.*openalex.*crossref" "${REPORT_PATH}"
  assert_marker "References" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research openalex only" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
  assert_regex "Sources:.*openalex" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research crossref only" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources crossref --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
  assert_regex "Sources:.*crossref" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research comprehensive template" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --template "${COMPREHENSIVE_TEMPLATE_PATH}" --web TRUE
  assert_marker "Comprehensive Results" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research sources equals syntax" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources=openalex,crossref --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
  assert_regex "Sources:.*openalex.*crossref" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research sources whitespace" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex crossref --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
  assert_regex "Sources:.*openalex.*crossref" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research year bounds" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex --year-from 2018 --year-to 2020 --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
  assert_marker "Year from: 2018" "${REPORT_PATH}"
  assert_marker "Year to: 2020" "${REPORT_PATH}"

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  if [ -n "${NLSS_SEMANTIC_SCHOLAR_API_KEY:-}" ] || [ -n "${SEMANTIC_SCHOLAR_API_KEY:-}" ]; then
    run_ok "research semantic scholar only" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
      --query "stress experience" --sources semantic_scholar --top-n 3 --max-per-source 3 --max-total 6 --timeout 20 --web TRUE
    assert_regex "Sources:.*semantic_scholar" "${REPORT_PATH}"
    assert_marker "Research (Academia)" "${REPORT_PATH}"
  else
    echo "[SKIP] research semantic scholar only (set NLSS_SEMANTIC_SCHOLAR_API_KEY to enable)" | tee -a "${LOG_FILE}"
  fi

  cat > "${TEMPLATE_PATH}" <<'EOF'
---
narrative:
  template: "{{narrative}}"
---
# RESEARCH_TEMPLATE_MARKER

{{analysis_flags}}

{{table_body}}

{{note_body}}

{{references}}
EOF

  rm -f "${REPORT_PATH}" "${ANALYSIS_LOG_PATH}"
  run_ok "research template override" env NLSS_WEB_SEARCH=1 Rscript "${R_SCRIPT_DIR}/research_academia.R" \
    --query "effect size" --sources openalex --top-n 3 --max-per-source 3 --max-total 5 --template "${TEMPLATE_PATH}" --web TRUE
  assert_marker "RESEARCH_TEMPLATE_MARKER" "${REPORT_PATH}"
else
  echo "[SKIP] web-enabled research tests (set NLSS_TEST_ALLOW_WEB=1 to enable)" | tee -a "${LOG_FILE}"
fi

echo "[DONE] research-academia tests finished" | tee -a "${LOG_FILE}"
