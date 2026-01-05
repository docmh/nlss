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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_efa_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"
PREP_LABEL_SCRIPT="${ROOT_DIR}/tests/smoke/prepare_labeled_dataset.R"
GOLDEN_VALUES_DIR="${ROOT_DIR}/tests/values"
GOLDEN_EFA_SUMMARY_PATH="${GOLDEN_VALUES_DIR}/efa_summary_golden.csv"
GOLDEN_EFA_LOADINGS_PATH="${GOLDEN_VALUES_DIR}/efa_loadings_golden.csv"
GOLDEN_EFA_EIGEN_PATH="${GOLDEN_VALUES_DIR}/efa_eigen_golden.csv"
CHECK_EFA_SUMMARY_SCRIPT="${GOLDEN_VALUES_DIR}/check_efa_summary_golden.py"
CHECK_EFA_LOADINGS_SCRIPT="${GOLDEN_VALUES_DIR}/check_efa_loadings_golden.py"
CHECK_EFA_EIGEN_SCRIPT="${GOLDEN_VALUES_DIR}/check_efa_eigen_golden.py"

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

WORKSPACE_DIR="${RUN_ROOT}/efa_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/efa"
LOG_FILE="${RUN_ROOT}/efa_test.log"
HELP_PATH="${TMP_BASE}/efa-help.txt"
TEMPLATE_OVERRIDE_PATH="${TMP_BASE}/efa-template-override.md"

CSV_SEMI_PATH="${TMP_BASE}/efa_semicolon.csv"
CSV_NOHEADER_PATH="${TMP_BASE}/efa_noheader.csv"
CSV_POLY_PATH="${TMP_BASE}/efa_poly.csv"
CSV_TETRA_PATH="${TMP_BASE}/efa_tetra.csv"
CSV_TETRA_BAD_PATH="${TMP_BASE}/efa_tetra_bad.csv"
CSV_COERCE_PATH="${TMP_BASE}/efa_coerce.csv"
CSV_TEMPLATE_PATH="${TMP_BASE}/efa_template.csv"
CSV_INTERACTIVE_PATH="${TMP_BASE}/efa_interactive.csv"
INTERACTIVE_INPUT="${TMP_BASE}/efa_interactive.txt"

RDS_PATH="${TMP_BASE}/efa_labeled.rds"
RDATA_PATH="${TMP_BASE}/efa_labeled.RData"
RDATA_DF="efa_df"
SAV_PATH="${TMP_BASE}/efa_labeled_sav.sav"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"

cleanup() {
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

cd "${WORKSPACE_DIR}"

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
if [[ ! -f "${GOLDEN_EFA_SUMMARY_PATH}" ]]; then
  echo "[FAIL] missing golden summary values: ${GOLDEN_EFA_SUMMARY_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [[ ! -f "${GOLDEN_EFA_LOADINGS_PATH}" ]]; then
  echo "[FAIL] missing golden loadings values: ${GOLDEN_EFA_LOADINGS_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [[ ! -f "${GOLDEN_EFA_EIGEN_PATH}" ]]; then
  echo "[FAIL] missing golden eigen values: ${GOLDEN_EFA_EIGEN_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [[ ! -f "${CHECK_EFA_SUMMARY_SCRIPT}" ]]; then
  echo "[FAIL] missing golden summary check script: ${CHECK_EFA_SUMMARY_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [[ ! -f "${CHECK_EFA_LOADINGS_SCRIPT}" ]]; then
  echo "[FAIL] missing golden loadings check script: ${CHECK_EFA_LOADINGS_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [[ ! -f "${CHECK_EFA_EIGEN_SCRIPT}" ]]; then
  echo "[FAIL] missing golden eigen check script: ${CHECK_EFA_EIGEN_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" jsonlite >/dev/null 2>&1; then
  echo "[FAIL] jsonlite not installed." | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" arrow >/dev/null 2>&1; then
  echo "[FAIL] arrow not installed; efa tests require parquet support." | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" psych >/dev/null 2>&1; then
  echo "[FAIL] psych not installed." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
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

assert_log_unchanged() {
  local before="$1"
  local after="$2"
  local label="$3"
  if [ "${before}" -ne "${after}" ]; then
    echo "[FAIL] ${label} (expected log count unchanged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

check_log() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" "$@"
}

check_efa_summary_golden() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local case_id="$1"; shift
  "${PYTHON_BIN}" "${CHECK_EFA_SUMMARY_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_EFA_SUMMARY_PATH}" "${case_id}"
}

check_efa_loadings_golden() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local case_id="$1"; shift
  "${PYTHON_BIN}" "${CHECK_EFA_LOADINGS_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_EFA_LOADINGS_PATH}" "${case_id}"
}

check_efa_eigen_golden() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local case_id="$1"; shift
  "${PYTHON_BIN}" "${CHECK_EFA_EIGEN_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_EFA_EIGEN_PATH}" "${case_id}"
}

check_log_value() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local path="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${path}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
path = sys.argv[3]
expected = sys.argv[4]

segments = [seg for seg in path.split(".") if seg]

entries = []
with log_path.open("r", encoding="utf-8") as handle:
    for idx, line in enumerate(handle, start=1):
        if idx <= start_count:
            continue
        if line.strip():
            try:
                entries.append(json.loads(line))
            except json.JSONDecodeError:
                continue

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "efa":
        entry = candidate
        break

if entry is None:
    print("No efa log entry found.")
    sys.exit(1)

value = entry
for seg in segments:
    if isinstance(value, dict):
        value = value.get(seg)
    else:
        value = None
        break

if expected in ("-", "", "None"):
    if value not in (None, ""):
        print(f"Expected empty for {path}, got {value}")
        sys.exit(1)
else:
    if str(value) != expected:
        print(f"Expected {path}={expected}, got {value}")
        sys.exit(1)
PY
}

check_log_status() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local status="$1"; shift
  check_log_value "${log_path}" "${start_count}" "results.status" "${status}"
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

run_expect_status() {
  local label="$1"; shift
  local log_path="$1"; shift
  local status="$1"; shift
  local start_count
  start_count="$(log_count "${log_path}")"
  echo "[RUN-EXPECT-STATUS] ${label}" | tee -a "${LOG_FILE}"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  check_log_status "${log_path}" "${start_count}" "${status}"
  echo "[PASS] ${label} (status ${status})" | tee -a "${LOG_FILE}"
}

cat >"${CSV_SEMI_PATH}" <<'EOF_DATA'
item1;item2;item3;item4
1;2;3;4
2;3;4;5
3;4;5;6
4;5;6;7
5;6;7;8
EOF_DATA

cat >"${CSV_NOHEADER_PATH}" <<'EOF_DATA'
1,2,3,4
2,3,4,5
3,4,5,6
4,5,6,7
5,6,7,8
EOF_DATA

cat >"${CSV_POLY_PATH}" <<'EOF_DATA'
p1,p2,p3,p4
1,2,3,4
2,3,4,1
3,4,2,2
4,1,1,3
2,2,3,4
3,3,4,1
4,4,1,2
1,1,2,3
2,3,3,4
3,2,4,1
4,3,1,2
1,4,2,3
EOF_DATA

cat >"${CSV_TETRA_PATH}" <<'EOF_DATA'
b1,b2,b3
0,1,0
1,1,0
0,0,1
1,0,1
0,1,1
1,0,0
0,0,0
1,1,1
0,1,0
1,0,1
EOF_DATA

cat >"${CSV_TETRA_BAD_PATH}" <<'EOF_DATA'
b1,b2,b3
0,1,2
1,0,1
2,1,0
0,2,1
1,0,2
EOF_DATA

cat >"${CSV_COERCE_PATH}" <<'EOF_DATA'
item1,item2,item3
1,2,3
2,3,4
x,4,5
3,5,6
4,6,7
EOF_DATA

cat >"${CSV_TEMPLATE_PATH}" <<'EOF_DATA'
item1,item2,item3,item4
1,2,3,4
2,3,4,5
3,4,5,6
4,5,6,7
5,6,7,8
EOF_DATA

cat >"${CSV_INTERACTIVE_PATH}" <<'EOF_DATA'
item1,item2,item3
1,2,3
2,3,4
3,4,5
4,5,6
EOF_DATA

cat >"${INTERACTIVE_INPUT}" <<EOF_INPUT
csv
${CSV_INTERACTIVE_PATH}
,
TRUE
item1,item2,item3

pca
varimax
eigen
1
pearson
complete
0.3
TRUE
FALSE
2

interactive efa prompt
TRUE
EOF_INPUT

run_ok "help output" bash -c "Rscript \"${R_SCRIPT_DIR}/efa.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Usage:"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [[ ! -f "${PARQUET_GOLDEN}" ]]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

BASE_VARS="f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev"

start_count="$(log_count "${LOG_PATH}")"
run_ok "efa default" Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}"
check_log "${LOG_PATH}" "${start_count}" \
  vars="${BASE_VARS}" method=pca rotation=varimax n_factors_rule=eigen cor=pearson missing=complete loading_cutoff=0.3 sort_loadings=true min_load_rows=8 check_sorted=true
run_ok "efa golden summary (default)" check_efa_summary_golden "${LOG_PATH}" "${start_count}" "efa_default_pca_eigen"
run_ok "efa golden loadings (default)" check_efa_loadings_golden "${LOG_PATH}" "${start_count}" "efa_default_pca_eigen"
run_ok "efa golden eigen (default)" check_efa_eigen_golden "${LOG_PATH}" "${start_count}" "efa_default_pca_eigen"

start_count="$(log_count "${LOG_PATH}")"
run_ok "efa eigen threshold" Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}" --method pa --rotation none --n-factors eigen --eigen-threshold 1.2 --digits 3 --user-prompt "explicit eigen threshold"
check_log "${LOG_PATH}" "${start_count}" \
  vars="${BASE_VARS}" method=pa rotation=none n_factors_rule=eigen cor=pearson missing=complete eigen_threshold=1.2 digits=3
check_log_value "${LOG_PATH}" "${start_count}" "user_prompt" "explicit eigen threshold"
run_ok "efa golden summary (pa eigen threshold)" check_efa_summary_golden "${LOG_PATH}" "${start_count}" "efa_pa_eigen_threshold"
run_ok "efa golden loadings (pa eigen threshold)" check_efa_loadings_golden "${LOG_PATH}" "${start_count}" "efa_pa_eigen_threshold"
run_ok "efa golden eigen (pa eigen threshold)" check_efa_eigen_golden "${LOG_PATH}" "${start_count}" "efa_pa_eigen_threshold"

start_count="$(log_count "${LOG_PATH}")"
run_ok "efa fixed minres" Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}" --method minres --rotation varimax --n-factors 2 --missing pairwise --cor spearman --loading-cutoff 0.4 --sort-loadings FALSE --coerce TRUE
check_log "${LOG_PATH}" "${start_count}" \
  vars="${BASE_VARS}" method=minres rotation=varimax n_factors_rule=fixed n_factors=2 cor=spearman missing=pairwise loading_cutoff=0.4 sort_loadings=false coerce=true
assert_contains "${NLSS_REPORT_PATH}" "Loadings < 0.40"
run_ok "efa golden summary (minres fixed)" check_efa_summary_golden "${LOG_PATH}" "${start_count}" "efa_minres_fixed_spearman_pairwise"
run_ok "efa golden loadings (minres fixed)" check_efa_loadings_golden "${LOG_PATH}" "${start_count}" "efa_minres_fixed_spearman_pairwise"
run_ok "efa golden eigen (minres fixed)" check_efa_eigen_golden "${LOG_PATH}" "${start_count}" "efa_minres_fixed_spearman_pairwise"

start_count="$(log_count "${LOG_PATH}")"
run_ok "efa oblimin uls" Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}" --method uls --rotation oblimin --n-factors 2 --sort-loadings TRUE
check_log "${LOG_PATH}" "${start_count}" \
  vars="${BASE_VARS}" method=uls rotation=oblimin n_factors_rule=fixed n_factors=2 sort_loadings=true check_sorted=true

for method in ml gls wls alpha; do
  start_count="$(log_count "${LOG_PATH}")"
  run_ok "efa method ${method}" Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}" --method "${method}" --rotation none --n-factors 2 --sort-loadings TRUE
  check_log "${LOG_PATH}" "${start_count}" \
    vars="${BASE_VARS}" method="${method}" rotation=none n_factors_rule=fixed n_factors=2 sort_loadings=true
done

SEMI_LABEL="$(basename "${CSV_SEMI_PATH}")"
SEMI_LABEL="${SEMI_LABEL%.*}"
SEMI_LOG_PATH="${WORKSPACE_DIR}/${SEMI_LABEL}/analysis_log.jsonl"
SEMI_REPORT_PATH="${WORKSPACE_DIR}/${SEMI_LABEL}/report_canonical.md"
start_count="$(log_count "${SEMI_LOG_PATH}")"
run_ok "efa csv sep" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_SEMI_PATH}" --sep ";" --vars item1,item2,item3,item4 --n-factors 2
check_log "${SEMI_LOG_PATH}" "${start_count}" vars=item1,item2,item3,item4 n_factors_rule=fixed n_factors=2

NOHEADER_LABEL="$(basename "${CSV_NOHEADER_PATH}")"
NOHEADER_LABEL="${NOHEADER_LABEL%.*}"
NOHEADER_LOG_PATH="${WORKSPACE_DIR}/${NOHEADER_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${NOHEADER_LOG_PATH}")"
run_ok "efa csv no header" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_NOHEADER_PATH}" --header FALSE --vars V1,V2,V3,V4 --n-factors 2
check_log "${NOHEADER_LOG_PATH}" "${start_count}" vars=V1,V2,V3,V4 n_factors_rule=fixed n_factors=2

POLY_LABEL="$(basename "${CSV_POLY_PATH}")"
POLY_LABEL="${POLY_LABEL%.*}"
POLY_LOG_PATH="${WORKSPACE_DIR}/${POLY_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${POLY_LOG_PATH}")"
run_ok "efa polychoric" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_POLY_PATH}" --vars p1,p2,p3,p4 --cor polychoric --n-factors 2 --rotation varimax
check_log "${POLY_LOG_PATH}" "${start_count}" vars=p1,p2,p3,p4 cor=polychoric n_factors_rule=fixed n_factors=2
run_ok "efa golden summary (polychoric)" check_efa_summary_golden "${POLY_LOG_PATH}" "${start_count}" "efa_polychoric_fixed"
run_ok "efa golden loadings (polychoric)" check_efa_loadings_golden "${POLY_LOG_PATH}" "${start_count}" "efa_polychoric_fixed"
run_ok "efa golden eigen (polychoric)" check_efa_eigen_golden "${POLY_LOG_PATH}" "${start_count}" "efa_polychoric_fixed"

TETRA_LABEL="$(basename "${CSV_TETRA_PATH}")"
TETRA_LABEL="${TETRA_LABEL%.*}"
TETRA_LOG_PATH="${WORKSPACE_DIR}/${TETRA_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${TETRA_LOG_PATH}")"
run_ok "efa tetrachoric" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_TETRA_PATH}" --vars b1,b2,b3 --cor tetrachoric --n-factors 1 --rotation none
check_log "${TETRA_LOG_PATH}" "${start_count}" vars=b1,b2,b3 cor=tetrachoric n_factors_rule=fixed n_factors=1 rotation=none
run_ok "efa golden summary (tetrachoric)" check_efa_summary_golden "${TETRA_LOG_PATH}" "${start_count}" "efa_tetrachoric_fixed"
run_ok "efa golden loadings (tetrachoric)" check_efa_loadings_golden "${TETRA_LOG_PATH}" "${start_count}" "efa_tetrachoric_fixed"
run_ok "efa golden eigen (tetrachoric)" check_efa_eigen_golden "${TETRA_LOG_PATH}" "${start_count}" "efa_tetrachoric_fixed"

TETRA_BAD_LABEL="$(basename "${CSV_TETRA_BAD_PATH}")"
TETRA_BAD_LABEL="${TETRA_BAD_LABEL%.*}"
TETRA_BAD_LOG_PATH="${WORKSPACE_DIR}/${TETRA_BAD_LABEL}/analysis_log.jsonl"
run_expect_status "efa tetrachoric invalid" "${TETRA_BAD_LOG_PATH}" "invalid_input" \
  Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_TETRA_BAD_PATH}" --vars b1,b2,b3 --cor tetrachoric --n-factors 1

COERCE_LABEL="$(basename "${CSV_COERCE_PATH}")"
COERCE_LABEL="${COERCE_LABEL%.*}"
COERCE_LOG_PATH="${WORKSPACE_DIR}/${COERCE_LABEL}/analysis_log.jsonl"
run_expect_status "efa coerce false" "${COERCE_LOG_PATH}" "invalid_input" \
  Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_COERCE_PATH}" --vars item1,item2,item3 --coerce FALSE --n-factors 2
start_count="$(log_count "${COERCE_LOG_PATH}")"
run_ok "efa coerce true" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_COERCE_PATH}" --vars item1,item2,item3 --coerce TRUE --n-factors 2
check_log "${COERCE_LOG_PATH}" "${start_count}" vars=item1,item2,item3 coerce=true n_factors_rule=fixed n_factors=2

LOG_OFF_LABEL="$(basename "${CSV_TEMPLATE_PATH}")"
LOG_OFF_LABEL="${LOG_OFF_LABEL%.*}"
LOG_OFF_LOG_PATH="${WORKSPACE_DIR}/${LOG_OFF_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${LOG_OFF_LOG_PATH}")"
run_ok "efa vars default" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_TEMPLATE_PATH}" --n-factors 2
check_log "${LOG_OFF_LOG_PATH}" "${start_count}" vars=item1,item2,item3,item4 n_factors_rule=fixed n_factors=2

start_count="$(log_count "${LOG_OFF_LOG_PATH}")"
run_ok "efa log off" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_TEMPLATE_PATH}" --vars item1,item2,item3,item4 --n-factors 2 --log FALSE
end_count="$(log_count "${LOG_OFF_LOG_PATH}")"
assert_log_unchanged "${start_count}" "${end_count}" "efa log off"

cat >"${TEMPLATE_OVERRIDE_PATH}" <<'EOF_TEMPLATE'
---
tokens:
  title: "EFA TEMPLATE TEST"
  table_title: "Loadings TEMPLATE TEST."
  note_prefix: "*Note-TEST.*"
  narrative_heading: "**Narrative TEST**"
table:
  columns:
    - key: "item"
      label: "Item TEST"
    - key: "loading"
      label: "Loading TEST"
note:
  template: "{{note_default}}"
narrative:
  template: "{{narrative_default}}"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---
EOF_TEMPLATE

TEMPLATE_LABEL="$(basename "${CSV_TEMPLATE_PATH}")"
TEMPLATE_LABEL="${TEMPLATE_LABEL%.*}"
TEMPLATE_REPORT="${WORKSPACE_DIR}/${TEMPLATE_LABEL}/report_canonical.md"
run_ok "efa template flag" Rscript "${R_SCRIPT_DIR}/efa.R" --csv "${CSV_TEMPLATE_PATH}" --vars item1,item2,item3,item4 --n-factors 2 --template "${TEMPLATE_OVERRIDE_PATH}"
assert_contains "${TEMPLATE_REPORT}" "EFA TEMPLATE TEST"
assert_contains "${TEMPLATE_REPORT}" "Loadings TEMPLATE TEST."
assert_contains "${TEMPLATE_REPORT}" "*Note-TEST.*"
assert_contains "${TEMPLATE_REPORT}" "**Narrative TEST**"
assert_contains "${TEMPLATE_REPORT}" "Item TEST"
assert_contains "${TEMPLATE_REPORT}" "Loading TEST"

run_ok "prepare labeled dataset" bash -c "Rscript \"${PREP_LABEL_SCRIPT}\" \"${DATA_GOLDEN}\" \"${RDS_PATH}\" \"${RDATA_PATH}\" \"${RDATA_DF}\""
if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare labeled sav" bash -c "Rscript \"${PREP_LABEL_SCRIPT}\" \"${DATA_GOLDEN}\" \"${RDS_PATH}\" \"${RDATA_PATH}\" \"${RDATA_DF}\" \"${SAV_PATH}\""
fi

RDS_LABEL="$(basename "${RDS_PATH}")"
RDS_LABEL="${RDS_LABEL%.*}"
RDS_LOG_PATH="${WORKSPACE_DIR}/${RDS_LABEL}/analysis_log.jsonl"
RDS_REPORT_PATH="${WORKSPACE_DIR}/${RDS_LABEL}/report_canonical.md"
start_count="$(log_count "${RDS_LOG_PATH}")"
run_ok "efa labeled rds" Rscript "${R_SCRIPT_DIR}/efa.R" --rds "${RDS_PATH}" --vars f1_1,f1_2,f1_3_rev,f1_4 --group group3 --n-factors 2
check_log "${RDS_LOG_PATH}" "${start_count}" group=group3 n_factors_rule=fixed n_factors=2
run_ok "efa golden summary (grouped group3)" check_efa_summary_golden "${RDS_LOG_PATH}" "${start_count}" "efa_grouped_fixed_group3"
run_ok "efa golden loadings (grouped group3)" check_efa_loadings_golden "${RDS_LOG_PATH}" "${start_count}" "efa_grouped_fixed_group3"
run_ok "efa golden eigen (grouped group3)" check_efa_eigen_golden "${RDS_LOG_PATH}" "${start_count}" "efa_grouped_fixed_group3"
assert_contains "${RDS_REPORT_PATH}" "LBL_F1_1"
assert_contains "${RDS_REPORT_PATH}" "LBL_GroupA"

RDATA_LOG_PATH="${WORKSPACE_DIR}/${RDATA_DF}/analysis_log.jsonl"
start_count="$(log_count "${RDATA_LOG_PATH}")"
run_ok "efa labeled rdata" Rscript "${R_SCRIPT_DIR}/efa.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --vars f1_1,f1_2,f1_3_rev,f1_4 --n-factors 2
check_log "${RDATA_LOG_PATH}" "${start_count}" n_factors_rule=fixed n_factors=2

if [ "${HAS_HAVEN}" -eq 1 ]; then
  SAV_LABEL="$(basename "${SAV_PATH}")"
  SAV_LABEL="${SAV_LABEL%.*}"
  SAV_LOG_PATH="${WORKSPACE_DIR}/${SAV_LABEL}/analysis_log.jsonl"
  start_count="$(log_count "${SAV_LOG_PATH}")"
  run_ok "efa labeled sav" Rscript "${R_SCRIPT_DIR}/efa.R" --sav "${SAV_PATH}" --vars f1_1,f1_2,f1_3_rev,f1_4 --n-factors 2
  check_log "${SAV_LOG_PATH}" "${start_count}" n_factors_rule=fixed n_factors=2
fi

INTERACTIVE_LABEL="$(basename "${CSV_INTERACTIVE_PATH}")"
INTERACTIVE_LABEL="${INTERACTIVE_LABEL%.*}"
INTERACTIVE_LOG_PATH="${WORKSPACE_DIR}/${INTERACTIVE_LABEL}/analysis_log.jsonl"
start_count="$(log_count "${INTERACTIVE_LOG_PATH}")"
run_ok "efa interactive" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/efa.R" --interactive
check_log_value "${INTERACTIVE_LOG_PATH}" "${start_count}" "user_prompt" "interactive efa prompt"

run_expect_status "efa missing vars" "${LOG_PATH}" "invalid_input" \
  Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars missing_item1,missing_item2

run_expect_status "efa missing group" "${LOG_PATH}" "invalid_input" \
  Rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_GOLDEN}" --vars "${BASE_VARS}" --group missing_group --n-factors 2

run_expect_fail "efa rdata missing df" Rscript "${R_SCRIPT_DIR}/efa.R" --rdata "${RDATA_PATH}"

if [[ -f "${NLSS_REPORT_PATH}" ]]; then
  assert_contains "${NLSS_REPORT_PATH}" "Exploratory Factor Analysis"
fi

echo "EFA tests completed." | tee -a "${LOG_FILE}"
