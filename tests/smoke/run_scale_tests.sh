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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_scale_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"
TEMPLATE_PATH="${ROOT_DIR}/assets/scale/default-template.md"

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
  DATA_DIR_CFG="./tests"
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

WORKSPACE_DIR="${RUN_ROOT}/scale_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/scale"
LOG_FILE="${RUN_ROOT}/scale_test.log"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"

export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [[ ! -f "${CHECK_SCRIPT}" ]]; then
  echo "Missing check script: ${CHECK_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi

if [[ ! -f "${DATA_GOLDEN}" ]]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" jsonlite >/dev/null 2>&1; then
  echo "[FAIL] jsonlite not installed." | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" arrow >/dev/null 2>&1; then
  echo "[FAIL] arrow not installed; scale tests require parquet support." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_SAV=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_SAV=1
else
  echo "[WARN] haven not installed; sav tests will be skipped." | tee -a "${LOG_FILE}"
fi

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"
TEMPLATE_BAK="${TMP_BASE}/scale-template-backup.md"
HELP_PATH="${TMP_BASE}/scale-help.txt"

cleanup() {
  if [[ -f "${TEMPLATE_BAK}" ]]; then
    mv -f "${TEMPLATE_BAK}" "${TEMPLATE_PATH}"
  fi
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

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

check_scale_log() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" "$@"
}

assert_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if ! rg -q --fixed-strings -- "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  else
    if ! grep -qF -- "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  fi
}

assert_not_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if rg -q --fixed-strings -- "$expected" "$file"; then
      echo "Did not expect to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  else
    if grep -qF -- "$expected" "$file"; then
      echo "Did not expect to find: $expected" | tee -a "${LOG_FILE}"
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

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

run_expect_fail() {
  local label="$1"; shift
  local log_path="$1"; shift
  local expected_msg="$1"; shift
  local start_count
  start_count="$(log_count "${log_path}")"
  local run_log="${TMP_BASE}/fail_${label//[^A-Za-z0-9_-]/_}.log"
  echo "[RUN-EXPECT-FAIL] ${label}" | tee -a "${LOG_FILE}"
  set +e
  "$@" >"${run_log}" 2>&1
  local status=$?
  set -e
  cat "${run_log}" >>"${LOG_FILE}"
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  local end_count
  end_count="$(log_count "${log_path}")"
  assert_log_unchanged "${start_count}" "${end_count}" "${label}"
  if [ -n "${expected_msg}" ]; then
    assert_contains "${run_log}" "${expected_msg}"
  fi
  echo "[PASS] ${label} (expected failure)" | tee -a "${LOG_FILE}"
}

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}"
rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}"
cd "${WORKSPACE_DIR}"

DATA_SIMPLE="${TMP_BASE}/scale_simple.csv"
DATA_GROUP="${TMP_BASE}/scale_group.csv"
DATA_REVERSE="${TMP_BASE}/scale_reverse.csv"
DATA_MISSING="${TMP_BASE}/scale_missing.csv"
DATA_SEMI="${TMP_BASE}/scale_semicolon.csv"
DATA_DEFAULT_VARS="${TMP_BASE}/scale_default_vars.csv"
DATA_NON_NUMERIC="${TMP_BASE}/scale_non_numeric.csv"
DATA_COERCE="${TMP_BASE}/scale_coerce.csv"
DATA_SINGLE="${TMP_BASE}/scale_single.csv"
DATA_OMEGA_INSUFF_N="${TMP_BASE}/scale_omega_insufficient_n.csv"
DATA_OMEGA_CORR="${TMP_BASE}/scale_omega_corr_missing.csv"
DATA_OMEGA_FACTANAL="${TMP_BASE}/scale_omega_factanal.csv"
DATA_LOG_OFF="${TMP_BASE}/scale_log_off.csv"
DATA_DIGITS="${TMP_BASE}/scale_digits.csv"
DATA_DROP_EMPTY="${TMP_BASE}/scale_drop_empty.csv"
DATA_APPEND="${TMP_BASE}/scale_append.csv"
DATA_INTERACTIVE="${TMP_BASE}/scale_interactive.csv"
INTERACTIVE_INPUT="${TMP_BASE}/scale_interactive_input.txt"
RDS_PATH="${TMP_BASE}/scale_rds.rds"
RDATA_PATH="${TMP_BASE}/scale_rdata.RData"
RDATA_DF="scale_rdata_df"
SAV_PATH="${TMP_BASE}/scale_sav.sav"
COERCE_TYPES_RDS="${TMP_BASE}/scale_coerce_types.rds"

cat >"${DATA_SIMPLE}" <<'EOF_DATA'
item1,item2,item3
1,2,1
2,3,2
3,4,3
4,5,3
5,6,4
EOF_DATA

cat >"${DATA_GROUP}" <<'EOF_DATA'
group,item1,item2,item3
A,1,2,1
A,2,3,2
B,3,4,3
B,4,5,3
NA,5,6,4
EOF_DATA

cat >"${DATA_REVERSE}" <<'EOF_DATA'
item1,item2,item3
1,1,5
2,2,4
3,3,3
4,4,2
5,5,1
EOF_DATA

cat >"${DATA_MISSING}" <<'EOF_DATA'
item1,item2,item3
1,2,1
2,NA,2
3,4,NA
4,5,3
EOF_DATA

cat >"${DATA_SEMI}" <<'EOF_DATA'
1;2
2;3
3;4
4;5
EOF_DATA

cat >"${DATA_DEFAULT_VARS}" <<'EOF_DATA'
group,item1,item2,label
A,1,2,x
B,2,3,y
A,3,4,z
EOF_DATA

cat >"${DATA_NON_NUMERIC}" <<'EOF_DATA'
item1,item2
a,b
c,d
EOF_DATA

cat >"${DATA_COERCE}" <<'EOF_DATA'
item1,item2
1,2
bad,3
2,4
EOF_DATA

cat >"${DATA_SINGLE}" <<'EOF_DATA'
item1
1
2
3
EOF_DATA

cat >"${DATA_OMEGA_INSUFF_N}" <<'EOF_DATA'
item1,item2,item3
1,1,1
2,2,2
3,3,NA
EOF_DATA

cat >"${DATA_OMEGA_CORR}" <<'EOF_DATA'
item1,item2,item3
2,1,2
2,2,3
2,3,4
2,4,5
EOF_DATA

cat >"${DATA_OMEGA_FACTANAL}" <<'EOF_DATA'
item1,item2,item3
1,1,1
2,2,2
3,3,3
4,4,4
5,5,5
EOF_DATA

cat >"${DATA_LOG_OFF}" <<'EOF_DATA'
item1,item2
1,2
2,3
3,4
EOF_DATA

cat >"${DATA_DIGITS}" <<'EOF_DATA'
item1,item2
1,1
2,2
3,3
EOF_DATA

cat >"${DATA_DROP_EMPTY}" <<'EOF_DATA'
item1,item2
3,3
3,3
3,3
EOF_DATA

cat >"${DATA_APPEND}" <<'EOF_DATA'
item1,item2,item3
1,2,1
2,3,2
3,4,3
EOF_DATA

cat >"${DATA_INTERACTIVE}" <<'EOF_DATA'
item1,item2,item3
1,2,1
2,3,2
3,4,3
EOF_DATA

cat >"${INTERACTIVE_INPUT}" <<EOF_INPUT
csv
${DATA_INTERACTIVE}
,
TRUE
item1,item2,item3


pairwise
sum
FALSE
FALSE
2

interactive prompt test
TRUE
EOF_INPUT

run_ok "help output" bash -c "Rscript \"${R_SCRIPT_DIR}/scale.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Usage:"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
if [[ ! -f "${PARQUET_GOLDEN}" ]]; then
  echo "[FAIL] missing parquet: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

VARS_BASE="f1_1,f1_2,f1_3_rev,f1_4"
start_count="$(log_count "${LOG_PATH}")"
run_ok "baseline csv" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_GOLDEN}" --vars "${VARS_BASE}" --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean --omega TRUE
check_scale_log "${LOG_PATH}" "${start_count}" vars="${VARS_BASE}" reverse="f1_3_rev" score=mean omega=true missing=pairwise
assert_contains "${NLSS_REPORT_PATH}" "Reverse-scored items: f1_3_rev (min = 1, max = 5)."

start_count="$(log_count "${LOG_PATH}")"
run_ok "grouped parquet" Rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_GOLDEN}" --vars "${VARS_BASE}" --group group2 --omega FALSE
check_scale_log "${LOG_PATH}" "${start_count}" group=group2 omega=false min_groups=2

start_count="$(log_count "${LOG_PATH}")"
run_ok "direct workspace" bash -c "cd \"${DATASET_DIR}\" && Rscript \"${R_SCRIPT_DIR}/scale.R\" --vars \"${VARS_BASE}\" --omega FALSE"
check_scale_log "${LOG_PATH}" "${start_count}" vars="${VARS_BASE}" omega=false

run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); ${RDATA_DF} <- df; save(${RDATA_DF}, file = \"${RDATA_PATH}\")"

RDS_LABEL="$(basename "${RDS_PATH}")"
RDS_LABEL="${RDS_LABEL%.*}"
RDS_LOG_PATH="${WORKSPACE_DIR}/${RDS_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${RDS_LOG_PATH}")"
run_ok "rds input" Rscript "${R_SCRIPT_DIR}/scale.R" --rds "${RDS_PATH}" --vars "${VARS_BASE}" --omega FALSE
check_scale_log "${RDS_LOG_PATH}" "${start_count}" vars="${VARS_BASE}" omega=false

RDATA_LOG_PATH="${WORKSPACE_DIR}/${RDATA_DF}/analysis_log.jsonl"
start_count="$(log_count "${RDATA_LOG_PATH}")"
run_ok "rdata input" Rscript "${R_SCRIPT_DIR}/scale.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --vars "${VARS_BASE}" --omega FALSE
check_scale_log "${RDATA_LOG_PATH}" "${start_count}" vars="${VARS_BASE}" omega=false

if [ "${HAS_SAV}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
  SAV_LABEL="$(basename "${SAV_PATH}")"
  SAV_LABEL="${SAV_LABEL%.*}"
  SAV_LOG_PATH="${WORKSPACE_DIR}/${SAV_LABEL}/analysis_log.jsonl"
  start_count="$(log_count "${SAV_LOG_PATH}")"
  run_ok "sav input" Rscript "${R_SCRIPT_DIR}/scale.R" --sav "${SAV_PATH}" --vars "${VARS_BASE}" --omega FALSE
  check_scale_log "${SAV_LOG_PATH}" "${start_count}" vars="${VARS_BASE}" omega=false
fi

SEMI_LABEL="$(basename "${DATA_SEMI}")"
SEMI_LABEL="${SEMI_LABEL%.*}"
SEMI_LOG_PATH="${WORKSPACE_DIR}/${SEMI_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${SEMI_LOG_PATH}")"
run_ok "csv sep/header" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SEMI}" --sep ";" --header FALSE --vars V1,V2 --omega FALSE
check_scale_log "${SEMI_LOG_PATH}" "${start_count}" vars="V1,V2" omega=false

DEFAULT_LABEL="$(basename "${DATA_DEFAULT_VARS}")"
DEFAULT_LABEL="${DEFAULT_LABEL%.*}"
DEFAULT_LOG_PATH="${WORKSPACE_DIR}/${DEFAULT_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${DEFAULT_LOG_PATH}")"
run_ok "default vars" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_DEFAULT_VARS}" --group group --omega FALSE
check_scale_log "${DEFAULT_LOG_PATH}" "${start_count}" vars="item1,item2" group=group omega=false

start_count="$(log_count "${DEFAULT_LOG_PATH}")"
run_ok "vars include group" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_DEFAULT_VARS}" --vars item1,item2,group --group group --omega FALSE
check_scale_log "${DEFAULT_LOG_PATH}" "${start_count}" vars="item1,item2" group=group omega=false

MISSING_LABEL="$(basename "${DATA_MISSING}")"
MISSING_LABEL="${MISSING_LABEL%.*}"
MISSING_LOG_PATH="${WORKSPACE_DIR}/${MISSING_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${MISSING_LOG_PATH}")"
run_ok "missing listwise" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_MISSING}" --vars item1,item2,item3 --missing listwise --omega FALSE
check_scale_log "${MISSING_LOG_PATH}" "${start_count}" missing=complete omega=false

SIMPLE_LABEL="$(basename "${DATA_SIMPLE}")"
SIMPLE_LABEL="${SIMPLE_LABEL%.*}"
SIMPLE_LOG_PATH="${WORKSPACE_DIR}/${SIMPLE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${SIMPLE_LOG_PATH}")"
run_ok "score avg" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,item2,item3 --score avg --omega FALSE
check_scale_log "${SIMPLE_LOG_PATH}" "${start_count}" score=mean score_method=mean omega=false

REVERSE_LABEL="$(basename "${DATA_REVERSE}")"
REVERSE_LABEL="${REVERSE_LABEL%.*}"
REVERSE_LOG_PATH="${WORKSPACE_DIR}/${REVERSE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${REVERSE_LOG_PATH}")"
run_ok "reverse observed" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_REVERSE}" --vars item1,item2,item3 --reverse item2 --omega FALSE
check_scale_log "${REVERSE_LOG_PATH}" "${start_count}" reverse=item2 omega=false
REVERSE_REPORT="${WORKSPACE_DIR}/${REVERSE_LABEL}/report_canonical.md"
assert_contains "${REVERSE_REPORT}" "Reverse-scored items: item2 (using observed min/max)."

start_count="$(log_count "${REVERSE_LOG_PATH}")"
run_ok "reverse invalid min" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_REVERSE}" --vars item1,item2,item3 --reverse item2 --reverse-min bad --reverse-max 5 --omega FALSE
check_scale_log "${REVERSE_LOG_PATH}" "${start_count}" reverse=item2 omega=false
assert_contains "${REVERSE_REPORT}" "Reverse-scored items: item2 (using observed min/max)."

NON_NUM_LABEL="$(basename "${DATA_NON_NUMERIC}")"
NON_NUM_LABEL="${NON_NUM_LABEL%.*}"
NON_NUM_LOG_PATH="${WORKSPACE_DIR}/${NON_NUM_LABEL}/analysis_log.jsonl"
run_expect_fail "vars unknown" "${SIMPLE_LOG_PATH}" "Unknown variables:" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,missing
run_expect_fail "reverse unknown" "${SIMPLE_LOG_PATH}" "Unknown reverse items:" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,item2,item3 --reverse item2,missing
run_expect_fail "group missing" "${SIMPLE_LOG_PATH}" "Grouping variable not found" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,item2,item3 --group missing
run_expect_fail "no numeric default" "${NON_NUM_LOG_PATH}" "No item variables available" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_NON_NUMERIC}"

COERCE_LABEL="$(basename "${DATA_COERCE}")"
COERCE_LABEL="${COERCE_LABEL%.*}"
COERCE_LOG_PATH="${WORKSPACE_DIR}/${COERCE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${COERCE_LOG_PATH}")"
run_ok "coerce true" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_COERCE}" --vars item1,item2 --coerce TRUE --omega FALSE
check_scale_log "${COERCE_LOG_PATH}" "${start_count}" coerce=true omega=false
assert_contains "${LOG_FILE}" "coercion introduced 1 NA values for item1"
run_expect_fail "coerce false" "${COERCE_LOG_PATH}" "Item is not numeric" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_COERCE}" --vars item1,item2 --coerce FALSE --omega FALSE

run_ok "prepare coerce types" Rscript -e "df <- data.frame(item1 = factor(c('1','2','3')), item2 = c(TRUE, FALSE, TRUE), item3 = as.Date(c('2020-01-01','2020-01-02','2020-01-03'))); saveRDS(df, \"${COERCE_TYPES_RDS}\")"
COERCE_TYPES_LABEL="$(basename "${COERCE_TYPES_RDS}")"
COERCE_TYPES_LABEL="${COERCE_TYPES_LABEL%.*}"
COERCE_TYPES_LOG_PATH="${WORKSPACE_DIR}/${COERCE_TYPES_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${COERCE_TYPES_LOG_PATH}")"
run_ok "coerce types" Rscript "${R_SCRIPT_DIR}/scale.R" --rds "${COERCE_TYPES_RDS}" --vars item1,item2,item3 --coerce TRUE --omega FALSE
check_scale_log "${COERCE_TYPES_LOG_PATH}" "${start_count}" coerce=true omega=false

SINGLE_LABEL="$(basename "${DATA_SINGLE}")"
SINGLE_LABEL="${SINGLE_LABEL%.*}"
SINGLE_LOG_PATH="${WORKSPACE_DIR}/${SINGLE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${SINGLE_LOG_PATH}")"
run_ok "omega insufficient items" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SINGLE}" --vars item1 --omega TRUE
check_scale_log "${SINGLE_LOG_PATH}" "${start_count}" omega=true omega_status=insufficient_items
SINGLE_REPORT="${WORKSPACE_DIR}/${SINGLE_LABEL}/report_canonical.md"

INSUFF_LABEL="$(basename "${DATA_OMEGA_INSUFF_N}")"
INSUFF_LABEL="${INSUFF_LABEL%.*}"
INSUFF_LOG_PATH="${WORKSPACE_DIR}/${INSUFF_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${INSUFF_LOG_PATH}")"
run_ok "omega insufficient n" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_OMEGA_INSUFF_N}" --vars item1,item2,item3 --omega TRUE
check_scale_log "${INSUFF_LOG_PATH}" "${start_count}" omega=true omega_status=insufficient_n

CORR_LABEL="$(basename "${DATA_OMEGA_CORR}")"
CORR_LABEL="${CORR_LABEL%.*}"
CORR_LOG_PATH="${WORKSPACE_DIR}/${CORR_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${CORR_LOG_PATH}")"
run_ok "omega correlation missing" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_OMEGA_CORR}" --vars item1,item2,item3 --omega TRUE
check_scale_log "${CORR_LOG_PATH}" "${start_count}" omega=true omega_status=correlation_missing

FACT_LABEL="$(basename "${DATA_OMEGA_FACTANAL}")"
FACT_LABEL="${FACT_LABEL%.*}"
FACT_LOG_PATH="${WORKSPACE_DIR}/${FACT_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${FACT_LOG_PATH}")"
run_ok "omega factanal failed" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_OMEGA_FACTANAL}" --vars item1,item2,item3 --omega TRUE
check_scale_log "${FACT_LOG_PATH}" "${start_count}" omega=true omega_status=factanal_failed

DROP_LABEL="$(basename "${DATA_DROP_EMPTY}")"
DROP_LABEL="${DROP_LABEL%.*}"
DROP_LOG_PATH="${WORKSPACE_DIR}/${DROP_LABEL}/analysis_log.jsonl"
DROP_REPORT="${WORKSPACE_DIR}/${DROP_LABEL}/report_canonical.md"
start_count="$(log_count "${DROP_LOG_PATH}")"
run_ok "drop empty columns" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_DROP_EMPTY}" --vars item1,item2 --omega FALSE
check_scale_log "${DROP_LOG_PATH}" "${start_count}" omega=false
assert_not_contains "${DROP_REPORT}" "| r_it |"
assert_not_contains "${DROP_REPORT}" "| r_drop |"
assert_not_contains "${DROP_REPORT}" "| alpha_if_deleted |"

PROMPT_LABEL="$(basename "${DATA_SIMPLE}")"
PROMPT_LABEL="${PROMPT_LABEL%.*}"
PROMPT_LOG_PATH="${WORKSPACE_DIR}/${PROMPT_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${PROMPT_LOG_PATH}")"
run_ok "user prompt" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,item2,item3 --omega FALSE --user-prompt "scale test prompt"
check_scale_log "${PROMPT_LOG_PATH}" "${start_count}" omega=false user_prompt="scale test prompt"

LOG_OFF_LABEL="$(basename "${DATA_LOG_OFF}")"
LOG_OFF_LABEL="${LOG_OFF_LABEL%.*}"
LOG_OFF_PATH="${WORKSPACE_DIR}/${LOG_OFF_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${LOG_OFF_PATH}")"
run_ok "log disabled" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_LOG_OFF}" --vars item1,item2 --omega FALSE --log FALSE
end_count="$(log_count "${LOG_OFF_PATH}")"
assert_log_unchanged "${start_count}" "${end_count}" "log disabled"

DIGITS_LABEL="$(basename "${DATA_DIGITS}")"
DIGITS_LABEL="${DIGITS_LABEL%.*}"
DIGITS_LOG_PATH="${WORKSPACE_DIR}/${DIGITS_LABEL}/analysis_log.jsonl"
DIGITS_REPORT="${WORKSPACE_DIR}/${DIGITS_LABEL}/report_canonical.md"
start_count="$(log_count "${DIGITS_LOG_PATH}")"
run_ok "digits rounding" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_DIGITS}" --vars item1,item2 --omega FALSE --digits 1
check_scale_log "${DIGITS_LOG_PATH}" "${start_count}" digits=1 omega=false
assert_contains "${DIGITS_REPORT}" "Total score (sum) M = 4.0, SD = 2.0."

APPEND_LABEL="$(basename "${DATA_APPEND}")"
APPEND_LABEL="${APPEND_LABEL%.*}"
APPEND_LOG_PATH="${WORKSPACE_DIR}/${APPEND_LABEL}/analysis_log.jsonl"
APPEND_REPORT="${WORKSPACE_DIR}/${APPEND_LABEL}/report_canonical.md"
start_count="$(log_count "${APPEND_LOG_PATH}")"
run_ok "append run 1" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_APPEND}" --vars item1,item2,item3 --omega FALSE
run_ok "append run 2" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_APPEND}" --vars item1,item2,item3 --omega FALSE
end_count="$(log_count "${APPEND_LOG_PATH}")"
if [ "$((end_count - start_count))" -lt 2 ]; then
  echo "[FAIL] append run did not add two log entries" | tee -a "${LOG_FILE}"
  exit 1
fi
assert_contains "${APPEND_REPORT}" "Table 1"
assert_contains "${APPEND_REPORT}" "Table 2"

if [[ -f "${TEMPLATE_PATH}" ]]; then
  cp "${TEMPLATE_PATH}" "${TEMPLATE_BAK}"
  cat >"${TEMPLATE_PATH}" <<'EOF_TEMPLATE'
---
tokens:
  title: "Scale Analysis TEMPLATE TEST"
  table_title: "Item analysis TEST."
  note_prefix: "*Note-TEST.*"
  narrative_heading: "**Narrative TEST**"
table:
  columns:
    - key: "item"
      label: "Item TEST"
    - key: "n"
      label: "N TEST"
    - key: "mean"
      label: "M TEST"
    - key: "missing_n"
      label: "Missing N TEST"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{group_label}} alpha={{alpha}} omega={{omega_total}}"
  join: "; "
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

{{table_title}}

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---
EOF_TEMPLATE

  TEMPLATE_LABEL="$(basename "${DATA_SIMPLE}")"
  TEMPLATE_LABEL="${TEMPLATE_LABEL%.*}"
  TEMPLATE_REPORT="${WORKSPACE_DIR}/${TEMPLATE_LABEL}/report_canonical.md"
  start_count="$(log_count "${SIMPLE_LOG_PATH}")"
  run_ok "template override" Rscript "${R_SCRIPT_DIR}/scale.R" --csv "${DATA_SIMPLE}" --vars item1,item2,item3 --omega FALSE
  check_scale_log "${SIMPLE_LOG_PATH}" "${start_count}" omega=false
  assert_contains "${TEMPLATE_REPORT}" "Scale Analysis TEMPLATE TEST"
  assert_contains "${TEMPLATE_REPORT}" "Item analysis TEST."
  assert_contains "${TEMPLATE_REPORT}" "*Note-TEST.*"
  assert_contains "${TEMPLATE_REPORT}" "**Narrative TEST**"
  assert_contains "${TEMPLATE_REPORT}" "Item TEST"
  assert_contains "${TEMPLATE_REPORT}" "N TEST"
  assert_contains "${TEMPLATE_REPORT}" "M TEST"
  assert_contains "${TEMPLATE_REPORT}" "Missing N TEST"
  assert_contains "${TEMPLATE_REPORT}" "alpha="
fi

INTERACTIVE_LABEL="$(basename "${DATA_INTERACTIVE}")"
INTERACTIVE_LABEL="${INTERACTIVE_LABEL%.*}"
INTERACTIVE_LOG_PATH="${WORKSPACE_DIR}/${INTERACTIVE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${INTERACTIVE_LOG_PATH}")"
run_ok "interactive" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/scale.R" --interactive
check_scale_log "${INTERACTIVE_LOG_PATH}" "${start_count}" user_prompt="interactive prompt test" omega=false

run_expect_fail "rdata missing df" "${RDATA_LOG_PATH}" "--df is required" Rscript "${R_SCRIPT_DIR}/scale.R" --rdata "${RDATA_PATH}" --vars "${VARS_BASE}"

echo "scale tests: OK" | tee -a "${LOG_FILE}"
