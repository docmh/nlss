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

usage() {
  cat <<'EOF'
Usage: run_deliberate_tests.sh [--root <path>] [--repo-root <path>]
  --root         Override run root (defaults to tests.output_dir/<timestamp>).
  --repo-root    Override repo root (defaults to script location).
  --help         Show this help message.
EOF
}

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

RUN_ROOT=""
REPO_ROOT_OVERRIDE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --root)
      RUN_ROOT="$2"
      shift 2
      ;;
    --repo-root)
      REPO_ROOT_OVERRIDE="$2"
      shift 2
      ;;
    --golden-dataset|--template-dir|--template-marker)
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

if [ -n "${REPO_ROOT_OVERRIDE}" ]; then
  ROOT_DIR="$(cd "${REPO_ROOT_OVERRIDE}" && pwd)"
  CONFIG_PATH="${ROOT_DIR}/scripts/config.yml"
  TESTS_CONFIG_PATH="${NLSS_TESTS_CONFIG:-${ROOT_DIR}/tests/tests.yml}"
fi

RUNS_BASE_CFG="$(get_tests_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi

RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"

if [ -z "${RUN_ROOT}" ]; then
  if [ -n "${NLSS_TEST_ROOT:-}" ]; then
    RUN_ROOT="$(to_abs_path "${NLSS_TEST_ROOT}")"
  else
    RUN_ID="$(date +%Y%m%d%H%M%S)"
    RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
  fi
else
  RUN_ROOT="$(to_abs_path "${RUN_ROOT}")"
fi

mkdir -p "${RUN_ROOT}"

export NLSS_TEST_ROOT="${RUN_ROOT}"
export NLSS_TEST_REPO_ROOT="${ROOT_DIR}"
if [ -z "${NLSS_TESTS_CONFIG:-}" ]; then
  export NLSS_TESTS_CONFIG="${TESTS_CONFIG_PATH}"
fi

LOG_FILE="${RUN_ROOT}/deliberate_test.log"
: > "${LOG_FILE}"

log() {
  echo "$@" | tee -a "${LOG_FILE}"
}

SUITES=(
  "calc:tests/run_calc_tests.sh"
  "research_academia:tests/run_research_academia_tests.sh"
  "descriptive_stats:tests/run_descriptive_stats_tests.sh"
  "data_explorer:tests/run_data_explorer_tests.sh"
  "data_transform:tests/run_data_transform_tests.sh"
  "impute:tests/run_impute_tests.sh"
  "plot:tests/run_plot_tests.sh"
  "crosstabs:tests/run_crosstabs_tests.sh"
  "correlations:tests/run_correlations_tests.sh"
  "scale:tests/run_scale_tests.sh"
  "efa:tests/run_efa_tests.sh"
  "reliability:tests/run_reliability_tests.sh"
  "regression:tests/run_regression_tests.sh"
  "power:tests/run_power_tests.sh"
  "sem:tests/run_sem_tests.sh"
  "mixed_models:tests/run_mixed_models_tests.sh"
  "assumptions:tests/run_assumptions_tests.sh"
  "anova:tests/run_anova_tests.sh"
  "t_test:tests/run_t_test_tests.sh"
  "nonparametric:tests/run_nonparametric_tests.sh"
  "metaskill_runner:tests/run_metaskill_runner_tests.sh"
  "check_integrity:tests/run_check_integrity_tests.sh"
  "reconstruct_reports:tests/run_reconstruct_reports_tests.sh"
)

FAILURES=0

for entry in "${SUITES[@]}"; do
  label="${entry%%:*}"
  script="${entry#*:}"
  if [ ! -f "${ROOT_DIR}/${script}" ]; then
    log "[FAIL] missing script: ${ROOT_DIR}/${script}"
    FAILURES=$((FAILURES + 1))
    continue
  fi
  log "[RUN] ${label}"
  set +e
  bash "${ROOT_DIR}/${script}" >>"${LOG_FILE}" 2>&1
  status=$?
  set -e
  if [ "${status}" -ne 0 ]; then
    log "[FAIL] ${label} (exit ${status})"
    FAILURES=$((FAILURES + 1))
  else
    log "[PASS] ${label}"
  fi
done

if [ "${FAILURES}" -gt 0 ]; then
  log "Deliberate suite completed with ${FAILURES} failure(s)."
  exit 1
fi

log "Deliberate suite completed successfully."
