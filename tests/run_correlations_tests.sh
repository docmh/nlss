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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_correlations_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/check_r_package.R"

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

WORKSPACE_DIR="${RUN_ROOT}/correlations_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/correlations"
LOG_FILE="${RUN_ROOT}/correlations_test.log"
HELP_PATH="${TMP_BASE}/correlations_help.txt"
RDS_PATH="${TMP_BASE}/correlations_input.rds"
RDATA_PATH="${TMP_BASE}/correlations_input.RData"
RDATA_DF="corr_df"
SAV_PATH="${TMP_BASE}/correlations_input.sav"
CSV_SEMI_PATH="${TMP_BASE}/correlations_semicolon.csv"
CSV_NOHEADER_PATH="${TMP_BASE}/correlations_noheader.csv"
CSV_NONUMERIC_PATH="${TMP_BASE}/correlations_non_numeric.csv"

mkdir -p "${RUN_ROOT}"
: > "${LOG_FILE}"

cleanup() {
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
cd "${WORKSPACE_DIR}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"

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

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
HAS_FOREIGN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" foreign >/dev/null 2>&1; then
  HAS_FOREIGN=1
fi

cat >"${CSV_SEMI_PATH}" <<'EOF_DATA'
x1;x2;x3
1;2;3
2;3;4
3;4;5
EOF_DATA

cat >"${CSV_NOHEADER_PATH}" <<'EOF_DATA'
1,2,3
2,3,4
3,4,5
EOF_DATA

cat >"${CSV_NONUMERIC_PATH}" <<'EOF_DATA'
cat1,cat2
red,blue
blue,red
EOF_DATA

run_ok "help output" bash -c "Rscript \"${R_SCRIPT_DIR}/correlations.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Usage:"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

rm -f "${NLSS_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${LOG_PATH}"

VARS_BASE="x1,x2,x3"

start_count="$(log_count "${LOG_PATH}")"
run_ok "baseline csv" Rscript "${R_SCRIPT_DIR}/correlations.R" --csv "${DATA_GOLDEN}" --vars "${VARS_BASE}"
check_log "${LOG_PATH}" "${start_count}" vars="${VARS_BASE}" method=pearson missing=pairwise

start_count="$(log_count "${LOG_PATH}")"
run_ok "workspace active dataset" bash -c "cd \"${DATASET_DIR}\" && Rscript \"${R_SCRIPT_DIR}/correlations.R\" --vars \"${VARS_BASE}\""
check_log "${LOG_PATH}" "${start_count}" vars="${VARS_BASE}"

start_count="$(log_count "${LOG_PATH}")"
run_ok "cross spearman adjust" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" \
  --x x1,x2 --y mediator,outcome_anova --method spearman --missing complete --p-adjust holm
check_log "${LOG_PATH}" "${start_count}" method=spearman missing=complete p_adjust=holm x="x1,x2" y="mediator,outcome_anova"

start_count="$(log_count "${LOG_PATH}")"
run_ok "grouped correlations" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --group group2
check_log "${LOG_PATH}" "${start_count}" group=group2 min_groups=2

start_count="$(log_count "${LOG_PATH}")"
run_ok "partial correlations" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --controls age,income
check_log "${LOG_PATH}" "${start_count}" controls="age,income" expect_partial=true

start_count="$(log_count "${LOG_PATH}")"
run_ok "bootstrap correlations" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" \
  --bootstrap TRUE --bootstrap-samples 200 --seed 42
check_log "${LOG_PATH}" "${start_count}" bootstrap=true bootstrap_samples=200 expect_boot=true

start_count="$(log_count "${LOG_PATH}")"
run_ok "r0 correlations" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --r0 0.2
check_log "${LOG_PATH}" "${start_count}" r0=0.2 expect_r0=true

start_count="$(log_count "${LOG_PATH}")"
run_ok "compare groups" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --group group2 --compare-groups TRUE
check_log "${LOG_PATH}" "${start_count}" compare_groups=true group=group2 expect_compare=true min_groups=2
assert_contains "${NLSS_REPORT_PATH}" "Fisher r-to-z comparisons between groups."

start_count="$(log_count "${LOG_PATH}")"
run_ok "matrix template" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --template matrix
check_log "${LOG_PATH}" "${start_count}" vars="${VARS_BASE}"
assert_contains "${NLSS_REPORT_PATH}" "Correlation Matrix"
assert_contains "${NLSS_REPORT_PATH}" "Values below the diagonal are correlations; values above are p-values."

SEMI_LABEL="$(basename "${CSV_SEMI_PATH}")"
SEMI_LABEL="${SEMI_LABEL%.*}"
SEMI_LOG_PATH="${WORKSPACE_DIR}/${SEMI_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${SEMI_LOG_PATH}")"
run_ok "csv sep/header" Rscript "${R_SCRIPT_DIR}/correlations.R" --csv "${CSV_SEMI_PATH}" --sep ";" --vars x1,x2,x3
check_log "${SEMI_LOG_PATH}" "${start_count}" vars="x1,x2,x3"

NOHEADER_LABEL="$(basename "${CSV_NOHEADER_PATH}")"
NOHEADER_LABEL="${NOHEADER_LABEL%.*}"
NOHEADER_LOG_PATH="${WORKSPACE_DIR}/${NOHEADER_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${NOHEADER_LOG_PATH}")"
run_ok "csv no header" Rscript "${R_SCRIPT_DIR}/correlations.R" --csv "${CSV_NOHEADER_PATH}" --header FALSE --vars V1,V2,V3
check_log "${NOHEADER_LOG_PATH}" "${start_count}" vars="V1,V2,V3"

run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); ${RDATA_DF} <- df; save(${RDATA_DF}, file = \"${RDATA_PATH}\")"

RDS_LABEL="$(basename "${RDS_PATH}")"
RDS_LABEL="${RDS_LABEL%.*}"
RDS_LOG_PATH="${WORKSPACE_DIR}/${RDS_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${RDS_LOG_PATH}")"
run_ok "rds input" Rscript "${R_SCRIPT_DIR}/correlations.R" --rds "${RDS_PATH}" --vars "${VARS_BASE}"
check_log "${RDS_LOG_PATH}" "${start_count}" vars="${VARS_BASE}"

RDATA_LOG_PATH="${WORKSPACE_DIR}/${RDATA_DF}/analysis_log.jsonl"
start_count="$(log_count "${RDATA_LOG_PATH}")"
run_ok "rdata input" Rscript "${R_SCRIPT_DIR}/correlations.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --vars "${VARS_BASE}"
check_log "${RDATA_LOG_PATH}" "${start_count}" vars="${VARS_BASE}"

if [ "${HAS_HAVEN}" -eq 1 ] || [ "${HAS_FOREIGN}" -eq 1 ]; then
  if [ "${HAS_HAVEN}" -eq 1 ]; then
    run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
  else
    echo "[WARN] skipping sav write (haven not installed)" | tee -a "${LOG_FILE}"
  fi
  if [ -f "${SAV_PATH}" ]; then
    SAV_LABEL="$(basename "${SAV_PATH}")"
    SAV_LABEL="${SAV_LABEL%.*}"
    SAV_LOG_PATH="${WORKSPACE_DIR}/${SAV_LABEL}/analysis_log.jsonl"
    start_count="$(log_count "${SAV_LOG_PATH}")"
    run_ok "sav input" Rscript "${R_SCRIPT_DIR}/correlations.R" --sav "${SAV_PATH}" --vars "${VARS_BASE}"
    check_log "${SAV_LOG_PATH}" "${start_count}" vars="${VARS_BASE}"
  fi
else
  echo "[WARN] skipping sav input (haven/foreign not installed)" | tee -a "${LOG_FILE}"
fi

before_count="$(log_count "${LOG_PATH}")"
run_ok "log off" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --log FALSE
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "log off"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "compare groups without group" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --compare-groups TRUE
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "compare groups without group"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "compare groups invalid levels" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --group group3 --compare-groups TRUE
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "compare groups invalid levels"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "r0 missing value" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --r0
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "r0 missing value"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "r0 out of range" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --r0 1
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "r0 out of range"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "kendall with controls" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --controls age --method kendall
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "kendall with controls"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "kendall with r0" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --method kendall --r0 0.2
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "kendall with r0"

before_count="$(log_count "${LOG_PATH}")"
run_expect_fail "kendall compare groups" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --group group2 --compare-groups TRUE --method kendall
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "kendall compare groups"

NONUMERIC_LABEL="$(basename "${CSV_NONUMERIC_PATH}")"
NONUMERIC_LABEL="${NONUMERIC_LABEL%.*}"
NONUMERIC_LOG_PATH="${WORKSPACE_DIR}/${NONUMERIC_LABEL}/analysis_log.jsonl"
before_count="$(log_count "${NONUMERIC_LOG_PATH}")"
run_expect_fail "non-numeric vars" Rscript "${R_SCRIPT_DIR}/correlations.R" --csv "${CSV_NONUMERIC_PATH}" --vars cat1,cat2
after_count="$(log_count "${NONUMERIC_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "non-numeric vars"

before_count="$(log_count "${NONUMERIC_LOG_PATH}")"
run_expect_fail "no numeric vars" Rscript "${R_SCRIPT_DIR}/correlations.R" --csv "${CSV_NONUMERIC_PATH}"
after_count="$(log_count "${NONUMERIC_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "no numeric vars"
