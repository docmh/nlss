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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_sem_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"
GOLDEN_VALUES_DIR="${ROOT_DIR}/tests/values"
GOLDEN_FIT_PATH="${GOLDEN_VALUES_DIR}/sem_fit_golden.csv"
GOLDEN_PARAMS_PATH="${GOLDEN_VALUES_DIR}/sem_params_golden.csv"
GOLDEN_R2_PATH="${GOLDEN_VALUES_DIR}/sem_r2_golden.csv"
GOLDEN_INVARIANCE_PATH="${GOLDEN_VALUES_DIR}/sem_invariance_golden.csv"
GOLDEN_MODINDICES_PATH="${GOLDEN_VALUES_DIR}/sem_modindices_golden.csv"
CHECK_FIT_SCRIPT="${GOLDEN_VALUES_DIR}/check_sem_fit_golden.py"
CHECK_PARAMS_SCRIPT="${GOLDEN_VALUES_DIR}/check_sem_params_golden.py"
CHECK_R2_SCRIPT="${GOLDEN_VALUES_DIR}/check_sem_r2_golden.py"
CHECK_INVARIANCE_SCRIPT="${GOLDEN_VALUES_DIR}/check_sem_invariance_golden.py"
CHECK_MODINDICES_SCRIPT="${GOLDEN_VALUES_DIR}/check_sem_modindices_golden.py"

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

WORKSPACE_DIR="${RUN_ROOT}/sem_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/sem"
LOG_FILE="${RUN_ROOT}/sem_test.log"

mkdir -p "${RUN_ROOT}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"

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

if [ ! -f "${GOLDEN_FIT_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_FIT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${GOLDEN_PARAMS_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_PARAMS_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${GOLDEN_R2_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_R2_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${GOLDEN_INVARIANCE_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_INVARIANCE_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${GOLDEN_MODINDICES_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_MODINDICES_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_FIT_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_FIT_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_PARAMS_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_PARAMS_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_R2_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_R2_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_INVARIANCE_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_INVARIANCE_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_MODINDICES_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_MODINDICES_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" lavaan >/dev/null 2>&1; then
  echo "[FAIL] lavaan not installed." | tee -a "${LOG_FILE}"
  exit 1
fi
HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
rm -f "${NLSS_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${LOG_PATH}"
cd "${WORKSPACE_DIR}"

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

check_log_value() {
  local start_count="$1"; shift
  local path="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${LOG_PATH}" "${start_count}" "${path}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
path = sys.argv[3]
expected = sys.argv[4]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "sem":
        entry = candidate
        break
if entry is None:
    fail("No sem log entry found.")

value = entry
for part in path.split("."):
    if isinstance(value, dict) and part in value:
        value = value[part]
    else:
        value = None
        break

if expected == "-":
    sys.exit(0)

if isinstance(value, bool) and expected.lower() in ("true", "false"):
    exp = expected.lower() == "true"
    if value != exp:
        fail(f"Expected {path}={exp}, got {value}")
    sys.exit(0)

try:
    exp_num = float(expected)
    if isinstance(value, (int, float)):
        if value is None or abs(value - exp_num) > 1e-6:
            fail(f"Expected {path}≈{exp_num}, got {value}")
        sys.exit(0)
except ValueError:
    pass

if isinstance(value, list):
    value_text = ",".join([str(v) for v in value])
else:
    value_text = "" if value is None else str(value)

if value_text != expected:
    fail(f"Expected {path}='{expected}', got '{value_text}'")
PY
}

check_log_list_contains() {
  local start_count="$1"; shift
  local path="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${LOG_PATH}" "${start_count}" "${path}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
path = sys.argv[3]
expected = sys.argv[4]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "sem":
        entry = candidate
        break
if entry is None:
    fail("No sem log entry found.")

value = entry
for part in path.split("."):
    if isinstance(value, dict) and part in value:
        value = value[part]
    else:
        value = None
        break

expected_items = [item.strip() for item in expected.split(",") if item.strip()]
if not expected_items:
    sys.exit(0)

if isinstance(value, list):
    actual_items = [str(item) for item in value if str(item)]
elif value is None:
    actual_items = []
else:
    actual_items = [item.strip() for item in str(value).split(",") if item.strip()]

missing = [item for item in expected_items if item not in actual_items]
if missing:
    fail(f"Expected {path} to include {missing}, got {actual_items}")
PY
}

check_invariance_rows() {
  local start_count="$1"; shift
  local min_rows="$1"; shift
  "${PYTHON_BIN}" - "${LOG_PATH}" "${start_count}" "${min_rows}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
min_rows = int(sys.argv[3])

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "sem":
        entry = candidate
        break
if entry is None:
    fail("No sem log entry found.")

fit = (entry.get("results") or {}).get("fit")
if not isinstance(fit, list) or len(fit) < min_rows:
    fail(f"Expected invariance fit table with at least {min_rows} rows")
PY
}

check_sem_fit_golden() {
  local log_path="$1"
  local start_count="$2"
  local case_id="$3"
  "${PYTHON_BIN}" "${CHECK_FIT_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_FIT_PATH}" "${case_id}"
}

check_sem_params_golden() {
  local log_path="$1"
  local start_count="$2"
  local case_id="$3"
  "${PYTHON_BIN}" "${CHECK_PARAMS_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_PARAMS_PATH}" "${case_id}"
}

check_sem_r2_golden() {
  local log_path="$1"
  local start_count="$2"
  local case_id="$3"
  "${PYTHON_BIN}" "${CHECK_R2_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_R2_PATH}" "${case_id}"
}

check_sem_invariance_golden() {
  local log_path="$1"
  local start_count="$2"
  local case_id="$3"
  "${PYTHON_BIN}" "${CHECK_INVARIANCE_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_INVARIANCE_PATH}" "${case_id}"
}

check_sem_modindices_golden() {
  local log_path="$1"
  local start_count="$2"
  local case_id="$3"
  "${PYTHON_BIN}" "${CHECK_MODINDICES_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_MODINDICES_PATH}" "${case_id}"
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
  if check_log "${start_count}" "${status}" "-" "standard" "-" "-" "-" "-" "-" "-"; then
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

FACTORS="F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
FACTORS_TWO="F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev"
ORDERED_ITEMS="f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev,f3_1,f3_2,f3_3,f3_4"
SIMPLE_FACTORS="F1=f1_1,f1_2,f1_3_rev,f1_4"
ORDERED_SIMPLE="f1_1,f1_2,f1_3_rev,f1_4"
TINY_FACTORS="F1=f1_1,f1_2,f1_3_rev"
ORDERED_TINY="f1_1,f1_2,f1_3_rev"
INVARIANCE_MODEL="F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4"
FIT_INDICES_GOLDEN="chisq,df,cfi,tli,rmsea,srmr"
MODEL_FILE="${TMP_BASE}/custom_model.txt"
RDS_PATH="${TMP_BASE}/golden_dataset.rds"
RDATA_PATH="${TMP_BASE}/golden_dataset.RData"
SAV_PATH="${TMP_BASE}/golden_dataset.sav"
INTERACTIVE_INPUT="${TMP_BASE}/interactive_input.txt"
HELP_PATH="${TMP_BASE}/sem_help.txt"

cat >"${MODEL_FILE}" <<'EOF'
mediator ~ a*x1
outcome_reg ~ b*mediator + c*x1
indirect := a*b
total := c + indirect
EOF

run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); golden_dataset <- df; save(golden_dataset, file = \"${RDATA_PATH}\")"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${INTERACTIVE_INPUT}" <<EOF
csv
${DATA_GOLDEN}
,
TRUE
path
outcome_reg
x1,x2

MLR
fiml
robust
standard
0.95
FALSE
200
std.all
chisq,df,cfi
TRUE
0
FALSE
2

TRUE
EOF

run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/sem.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Structural equation modeling"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

start=$(log_count "${LOG_PATH}")
run_ok "path golden (fit/params/r2)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2 --estimator ML --missing listwise --se standard --ci standard --conf-level 0.95 --std std.all --fit "${FIT_INDICES_GOLDEN}" --r2 TRUE
run_ok "sem fit golden (path)" check_sem_fit_golden "${LOG_PATH}" "${start}" "path_basic_fit"
run_ok "sem params golden (path x1)" check_sem_params_golden "${LOG_PATH}" "${start}" "path_basic_x1"
run_ok "sem params golden (path x2)" check_sem_params_golden "${LOG_PATH}" "${start}" "path_basic_x2"
run_ok "sem r2 golden (path outcome_reg)" check_sem_r2_golden "${LOG_PATH}" "${start}" "path_basic_r2_outcome_reg"

start=$(log_count "${LOG_PATH}")
run_ok "cfa golden (fit/params/modindices)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS_TWO}" --estimator ML --missing listwise --se standard --ci standard --conf-level 0.95 --std std.all --fit "${FIT_INDICES_GOLDEN}" --modindices 1
run_ok "sem fit golden (cfa)" check_sem_fit_golden "${LOG_PATH}" "${start}" "cfa_basic_fit"
run_ok "sem params golden (cfa loading)" check_sem_params_golden "${LOG_PATH}" "${start}" "cfa_basic_loading_f1_1"
run_ok "sem params golden (cfa covariance)" check_sem_params_golden "${LOG_PATH}" "${start}" "cfa_basic_cov_f1_f2"
run_ok "sem modindices golden (cfa top1)" check_sem_modindices_golden "${LOG_PATH}" "${start}" "cfa_modindices_top1"

start=$(log_count "${LOG_PATH}")
run_ok "cfa grouped golden (params)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS_TWO}" --group group2 --estimator ML --missing listwise --se standard --ci standard --conf-level 0.95 --std std.all --fit "${FIT_INDICES_GOLDEN}"
run_ok "sem params golden (cfa grouped control)" check_sem_params_golden "${LOG_PATH}" "${start}" "cfa_group_control_loading_f1_1"

start=$(log_count "${LOG_PATH}")
run_ok "mediation golden (fit/params/r2)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator --y outcome_reg --estimator ML --missing listwise --se standard --ci standard --conf-level 0.95 --std std.all --fit "${FIT_INDICES_GOLDEN}" --r2 TRUE
run_ok "sem fit golden (mediation)" check_sem_fit_golden "${LOG_PATH}" "${start}" "mediation_basic_fit"
run_ok "sem params golden (mediation indirect)" check_sem_params_golden "${LOG_PATH}" "${start}" "mediation_indirect_mediator"
run_ok "sem r2 golden (mediation mediator)" check_sem_r2_golden "${LOG_PATH}" "${start}" "mediation_r2_mediator"

start=$(log_count "${LOG_PATH}")
run_ok "invariance golden (configural/metric)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --group group2 --model "${INVARIANCE_MODEL}" --invariance "configural,metric" --estimator ML --missing listwise --se standard --ci standard --conf-level 0.95 --std std.all --fit "${FIT_INDICES_GOLDEN}"
run_ok "sem invariance golden (configural)" check_sem_invariance_golden "${LOG_PATH}" "${start}" "invariance_configural"
run_ok "sem invariance golden (metric)" check_sem_invariance_golden "${LOG_PATH}" "${start}" "invariance_metric"

start=$(log_count "${LOG_PATH}")
run_ok "path csv input" Rscript "${R_SCRIPT_DIR}/sem.R" --csv "${DATA_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "path rds input" Rscript "${R_SCRIPT_DIR}/sem.R" --rds "${RDS_PATH}" --analysis path --dv outcome_reg --ivs x1,x2
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "path rdata input" Rscript "${R_SCRIPT_DIR}/sem.R" --rdata "${RDATA_PATH}" --df golden_dataset --analysis path --dv outcome_reg --ivs x1,x2
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH}")
  run_ok "path sav input" Rscript "${R_SCRIPT_DIR}/sem.R" --sav "${SAV_PATH}" --analysis path --dv outcome_reg --ivs x1,x2
  check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"
fi

start=$(log_count "${LOG_PATH}")
run_ok "interactive path" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/sem.R" --interactive
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "cfa factors (mlr)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS}" --estimator MLR --missing fiml --se robust --ci standard --fit "chisq,df,cfi,tli,rmsea,srmr" --std std.all --r2 TRUE
check_log "${start}" "ok" "cfa" "standard" "10" "chisq,df,cfi,tli,rmsea,srmr" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "cfa ordered (wlsmv)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS}" --estimator WLSMV --ordered "${ORDERED_ITEMS}" --missing listwise --se standard
check_log "${start}" "ok" "cfa" "standard" "10" "chisq,df,cfi,rmsea,srmr" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "cfa grouped (group2)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS}" --group group2
check_log "${start}" "ok" "cfa" "standard" "10" "chisq,df,cfi" "-" "false" "false" "true"

start=$(log_count "${LOG_PATH}")
run_ok "cfa group equal (loadings/intercepts)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS}" --group group2 --group-equal loadings,intercepts
check_log "${start}" "ok" "cfa" "standard" "10" "chisq,df,cfi" "-" "false" "false" "true"
check_log_list_contains "${start}" "options.group_equal" "loadings,intercepts"

start=$(log_count "${LOG_PATH}")
run_ok "cfa missing pairwise" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${SIMPLE_FACTORS}" --estimator ML --missing pairwise --se standard
check_log "${start}" "ok" "cfa" "standard" "4" "chisq,df,cfi" "-" "false" "false" "false"
check_log_value "${start}" "options.estimator" "ML"
check_log_value "${start}" "options.missing" "pairwise"

for estimator in MLM MLMV MLMVS GLS ULS; do
  start=$(log_count "${LOG_PATH}")
  run_ok "cfa estimator ${estimator}" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${SIMPLE_FACTORS}" --estimator "${estimator}" --missing listwise --se standard
  check_log "${start}" "ok" "cfa" "standard" "4" "chisq,df,cfi" "-" "false" "false" "false"
  check_log_value "${start}" "options.estimator" "${estimator}"
done

for estimator in ULSMV DWLS; do
  start=$(log_count "${LOG_PATH}")
  run_ok "cfa estimator ${estimator} (ordered)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${TINY_FACTORS}" --estimator "${estimator}" --ordered "${ORDERED_TINY}" --missing listwise --se standard
  check_log "${start}" "ok" "cfa" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"
  check_log_value "${start}" "options.estimator" "${estimator}"
done

start=$(log_count "${LOG_PATH}")
run_ok "cfa std.lv" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${SIMPLE_FACTORS}" --std std.lv
check_log "${start}" "ok" "cfa" "standard" "4" "chisq,df,cfi" "-" "false" "false" "false"
check_log_value "${start}" "options.std" "std.lv"

start=$(log_count "${LOG_PATH}")
run_ok "path builder" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2,mediator --missing listwise
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "sem builder (dv/ivs)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --dv outcome_reg --ivs x1,x2
check_log "${start}" "ok" "sem" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "sem inline model" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --model "outcome_reg ~ x1 + x2"
check_log "${start}" "ok" "sem" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "sem paths alias" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --paths "outcome_reg ~ x1 + x2"
check_log "${start}" "ok" "sem" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "path std none" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2 --std none
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"
check_log_value "${start}" "options.std" "none"

start=$(log_count "${LOG_PATH}")
run_ok "path fit aic/bic (r2 off)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2 --fit "aic,bic" --r2 FALSE
check_log "${start}" "ok" "path" "standard" "3" "aic,bic" "-" "false" "false" "false"
check_log_list_contains "${start}" "options.fit" "aic,bic"

before_log=$(log_count "${LOG_PATH}")
run_ok "path log disabled" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2 --log FALSE
after_log=$(log_count "${LOG_PATH}")
assert_log_unchanged "${before_log}" "${after_log}" "path log disabled"

start=$(log_count "${LOG_PATH}")
run_ok "path user prompt" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2 --user-prompt "sem test prompt"
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "-" "false" "false" "false"
check_log_value "${start}" "user_prompt" "sem test prompt"

start=$(log_count "${LOG_PATH}")
run_ok "mediation bootstrap" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator --y outcome_reg --estimator ML --bootstrap TRUE --bootstrap-samples 200 --se bootstrap --ci bootstrap
check_log "${start}" "ok" "mediation" "standard" "5" "chisq,df,cfi" "-" "false" "true" "false"

start=$(log_count "${LOG_PATH}")
run_ok "mediation serial" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator,x2 --y outcome_reg --serial TRUE --covariates age
check_log "${start}" "ok" "mediation" "standard" "5" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "mediation parallel" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator,x2 --y outcome_reg --serial FALSE --covariates age
check_log "${start}" "ok" "mediation" "standard" "5" "chisq,df,cfi" "-" "false" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "mediation bca (conf 0.9)" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator --y outcome_reg --estimator ML --bootstrap TRUE --bootstrap-samples 200 --se bootstrap --ci bca --conf-level 0.9
check_log "${start}" "ok" "mediation" "standard" "5" "chisq,df,cfi" "-" "false" "true" "false"
check_log_value "${start}" "options.ci" "bca"
check_log_value "${start}" "options.conf_level" "0.9"

start=$(log_count "${LOG_PATH}")
run_ok "sem model file" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --model-file "${MODEL_FILE}"
check_log "${start}" "ok" "sem" "standard" "4" "chisq,df,cfi" "-" "false" "false" "false"
assert_contains "${NLSS_REPORT_PATH}" "indirect := a*b"

start=$(log_count "${LOG_PATH}")
run_ok "sem modindices and residuals" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2,mediator --modindices 10 --residuals TRUE
check_log "${start}" "ok" "path" "standard" "3" "chisq,df,cfi" "true" "true" "false" "false"

start=$(log_count "${LOG_PATH}")
run_ok "invariance configural/metric" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --group group2 --model "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4" --invariance "configural,metric"
check_log "${start}" "ok" "invariance" "invariance" "-" "step,cfi,rmsea" "-" "false" "false" "true"

start=$(log_count "${LOG_PATH}")
run_ok "invariance full steps" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --group group2 --model "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4" --invariance "configural,metric,scalar,strict"
check_log "${start}" "ok" "invariance" "invariance" "-" "step,cfi,rmsea" "-" "false" "false" "true"
check_log_list_contains "${start}" "options.invariance" "configural,metric,scalar,strict"

start=$(log_count "${LOG_PATH}")
run_ok "invariance custom group equal" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --group group2 --model "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4" --group-equal loadings,intercepts
check_log_value "${start}" "results.status" "ok"
check_log_value "${start}" "options.analysis" "invariance"
check_invariance_rows "${start}" 1
check_log_list_contains "${start}" "options.invariance" "custom"

TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.sem.default)"
TEMPLATE_MEDIATION_ORIG="$(get_config_value templates.sem.mediation)"
TEMPLATE_INVARIANCE_ORIG="$(get_config_value templates.sem.invariance)"

TEMPLATE_DEFAULT_TMP="${TMP_BASE}/sem_default_template.md"
TEMPLATE_MEDIATION_TMP="${TMP_BASE}/sem_mediation_template.md"
TEMPLATE_INVARIANCE_TMP="${TMP_BASE}/sem_invariance_template.md"

cp "$(resolve_template_source "${TEMPLATE_DEFAULT_ORIG}")" "${TEMPLATE_DEFAULT_TMP}"
cp "$(resolve_template_source "${TEMPLATE_MEDIATION_ORIG}")" "${TEMPLATE_MEDIATION_TMP}"
cp "$(resolve_template_source "${TEMPLATE_INVARIANCE_ORIG}")" "${TEMPLATE_INVARIANCE_TMP}"

printf "\n\nSEM_TEMPLATE_DEFAULT\n" >>"${TEMPLATE_DEFAULT_TMP}"
printf "\n\nSEM_TEMPLATE_MEDIATION\n" >>"${TEMPLATE_MEDIATION_TMP}"
printf "\n\nSEM_TEMPLATE_INVARIANCE\n" >>"${TEMPLATE_INVARIANCE_TMP}"

set_config_value templates.sem.default "${TEMPLATE_DEFAULT_TMP}"
set_config_value templates.sem.mediation "${TEMPLATE_MEDIATION_TMP}"
set_config_value templates.sem.invariance "${TEMPLATE_INVARIANCE_TMP}"

run_ok "template override default" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis path --dv outcome_reg --ivs x1,x2
assert_contains "${NLSS_REPORT_PATH}" "SEM_TEMPLATE_DEFAULT"

run_ok "template override mediation" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator --y outcome_reg
assert_contains "${NLSS_REPORT_PATH}" "SEM_TEMPLATE_MEDIATION"

run_ok "template override invariance" Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --group group2 --model "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4" --invariance "configural,metric"
assert_contains "${NLSS_REPORT_PATH}" "SEM_TEMPLATE_INVARIANCE"

set_config_value templates.sem.default "${TEMPLATE_DEFAULT_ORIG}"
set_config_value templates.sem.mediation "${TEMPLATE_MEDIATION_ORIG}"
set_config_value templates.sem.invariance "${TEMPLATE_INVARIANCE_ORIG}"

run_expect_invalid "missing model" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem
run_expect_invalid "invalid factors" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "F1"
run_expect_invalid "serial needs two mediators" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis mediation --x x1 --m mediator --y outcome_reg --serial TRUE
run_expect_invalid "invariance missing group" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis invariance --model "F1 =~ f1_1 + f1_2"
run_expect_invalid "unknown ordered variable" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis cfa --factors "${FACTORS}" --ordered not_a_var
run_expect_invalid "unknown model variable" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --model "outcome_reg ~ not_a_var"
run_expect_invalid "model file missing" invalid_input Rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_GOLDEN}" --analysis sem --model-file "${TMP_BASE}/missing_model.txt"

echo "SEM tests: OK" | tee -a "${LOG_FILE}"
