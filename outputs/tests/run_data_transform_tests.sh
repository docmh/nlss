#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/outputs/tests/check_data_transform_log.py"
CHECK_PKG_SCRIPT="${ROOT_DIR}/outputs/tests/check_r_package.R"

get_config_value() {
  python3 - "$CONFIG_PATH" "$1" <<'PY'
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

set_config_value() {
  python3 - "$CONFIG_PATH" "$1" "$2" <<'PY'
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
  if [[ "${path}" == /* || "${path}" =~ ^[A-Za-z]: ]]; then
    echo "${path}"
    return 0
  fi
  echo "${ROOT_DIR}/${path#./}"
}

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="core-stats-workspace.yml"
fi

DATA_DIR_CFG="$(get_config_value tests.data_dir)"
if [ -z "${DATA_DIR_CFG}" ]; then
  DATA_DIR_CFG="./outputs/tests"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_GOLDEN_CFG="$(get_config_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"

RUNS_BASE_CFG="$(get_config_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${CORE_STATS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${CORE_STATS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
fi

WORKSPACE_DIR="${RUN_ROOT}/data_transform_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/data_transform"
LOG_FILE="${RUN_ROOT}/data_transform_test.log"

mkdir -p "${RUN_ROOT}"

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
  echo "Rscript not found. Install R or use scripts/run_rscript.ps1 on Windows." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${CHECK_SCRIPT}" ]; then
  echo "[FAIL] missing check script: ${CHECK_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

cd "${WORKSPACE_DIR}"

if ! Rscript "${CHECK_PKG_SCRIPT}" arrow >/dev/null 2>&1; then
  echo "[FAIL] arrow not installed." | tee -a "${LOG_FILE}"
  exit 1
fi

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

log_count() {
  python3 - "$1" <<'PY'
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
  python3 "${CHECK_SCRIPT}" "${log_path}" "$@"
}

run_expect_fail() {
  local label="$1"; shift
  local log_path="$1"; shift
  echo "[RUN-EXPECT-FAIL] ${label}" | tee -a "${LOG_FILE}"
  local start_count
  start_count="$(log_count "${log_path}")"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  local end_count
  end_count="$(log_count "${log_path}")"
  if [ "${end_count}" -ne "${start_count}" ]; then
    echo "[FAIL] ${label} (log changed)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
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
  echo "${ROOT_DIR}/core-stats/assets/${value}"
}

prepare_dataset() {
  local label="$1"
  local csv_path="${TMP_BASE}/${label}.csv"
  cp "${DATA_GOLDEN}" "${csv_path}"
  run_ok "init workspace ${label}" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${csv_path}" --log FALSE >&2
  local dataset_dir="${WORKSPACE_DIR}/${label}"
  local parquet_path="${dataset_dir}/${label}.parquet"
  if [ ! -f "${parquet_path}" ]; then
    echo "[FAIL] missing parquet copy: ${parquet_path}" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "${csv_path}|${dataset_dir}|${parquet_path}"
}

HELP_PATH="${TMP_BASE}/data_transform_help.txt"
run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/data_transform.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Data transformation"

clean_info="$(prepare_dataset "data_transform_clean")"
IFS="|" read -r CLEAN_CSV CLEAN_DIR CLEAN_PARQUET <<< "${clean_info}"
CLEAN_LOG="${CLEAN_DIR}/analysis_log.jsonl"
CLEAN_REPORT="${CLEAN_DIR}/apa_report.md"

start="$(log_count "${CLEAN_LOG}")"
run_ok "data_transform clean" Rscript "${R_SCRIPT_DIR}/data_transform.R" \
  --parquet "${CLEAN_PARQUET}" \
  --calc "score_avg=(pre_score+mid_score+post_score)/3" \
  --transform "skewed_var=log" \
  --standardize x1 \
  --recode "group3=A:1,B:2,C:3" \
  --percentile-bins "outlier_var=4" \
  --bins "age=50,0,100" \
  --rename "group3_rec:group3_num" \
  --drop zero_var \
  --confirm-drop

check_log "${CLEAN_LOG}" "${start}" 8 "calc,transform,standardize,recode,percentile_bin,bin,rename,drop" "true" \
  calc="score_avg=(pre_score+mid_score+post_score)/3" \
  transform="skewed_var=log" \
  standardize="x1" \
  recode="group3=A:1,B:2,C:3" \
  percentile_bins="outlier_var=4" \
  bins="age=50,0,100" \
  rename="group3_rec:group3_num" \
  drop="zero_var"

assert_contains "${CLEAN_REPORT}" "Data Transformations"
assert_contains "${CLEAN_REPORT}" "Transformation log."
assert_contains "${CLEAN_REPORT}" "Table 1"
assert_contains "${CLEAN_REPORT}" "Standardized"
assert_contains "${CLEAN_REPORT}" "Renamed"
assert_contains "${CLEAN_REPORT}" "sorted breaks"
assert_contains "${CLEAN_REPORT}" "Actions:"
assert_contains "${CLEAN_REPORT}" "Step 1:"

no_log_info="$(prepare_dataset "data_transform_no_log")"
IFS="|" read -r NO_LOG_CSV NO_LOG_DIR NO_LOG_PARQUET <<< "${no_log_info}"
NO_LOG_PATH="${NO_LOG_DIR}/analysis_log.jsonl"

start="$(log_count "${NO_LOG_PATH}")"
run_ok "data_transform log disabled" Rscript "${R_SCRIPT_DIR}/data_transform.R" \
  --parquet "${NO_LOG_PARQUET}" \
  --calc "score_avg=(pre_score+mid_score+post_score)/3" \
  --log FALSE
end="$(log_count "${NO_LOG_PATH}")"
assert_log_unchanged "${start}" "${end}" "data_transform log disabled"

template_info="$(prepare_dataset "data_transform_template")"
IFS="|" read -r TEMPLATE_CSV TEMPLATE_DIR TEMPLATE_PARQUET <<< "${template_info}"
TEMPLATE_REPORT="${TEMPLATE_DIR}/apa_report.md"

TEMPLATE_KEY="templates.data_transform.default"
TEMPLATE_ORIG="$(get_config_value "${TEMPLATE_KEY}")"
TEMPLATE_TMP="${TMP_BASE}/data_transform_template.md"
cp "$(resolve_template_source "${TEMPLATE_ORIG}")" "${TEMPLATE_TMP}"
sed -i 's/title: "Data Transformations"/title: "Data Transformations TEMPLATE TEST"/' "${TEMPLATE_TMP}"
sed -i 's/table_title: "Transformation log\."/table_title: "Transformation log TEST."/' "${TEMPLATE_TMP}"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_TMP}"
sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_TMP}"

set_config_value "${TEMPLATE_KEY}" "${TEMPLATE_TMP}"

run_ok "template override" Rscript "${R_SCRIPT_DIR}/data_transform.R" \
  --parquet "${TEMPLATE_PARQUET}" \
  --calc "score_avg=(pre_score+mid_score+post_score)/3"

assert_contains "${TEMPLATE_REPORT}" "Data Transformations TEMPLATE TEST"
assert_contains "${TEMPLATE_REPORT}" "Transformation log TEST."
assert_contains "${TEMPLATE_REPORT}" "*Note-TEST.*"
assert_contains "${TEMPLATE_REPORT}" "**Narrative TEST**"

set_config_value "${TEMPLATE_KEY}" "${TEMPLATE_ORIG}"

invalid_info="$(prepare_dataset "data_transform_invalid")"
IFS="|" read -r INVALID_CSV INVALID_DIR INVALID_PARQUET <<< "${invalid_info}"
INVALID_LOG="${INVALID_DIR}/analysis_log.jsonl"

run_expect_fail "drop requires confirm" "${INVALID_LOG}" Rscript "${R_SCRIPT_DIR}/data_transform.R" \
  --parquet "${INVALID_PARQUET}" \
  --drop zero_var
run_expect_fail "unknown transform var" "${INVALID_LOG}" Rscript "${R_SCRIPT_DIR}/data_transform.R" \
  --parquet "${INVALID_PARQUET}" \
  --transform "missing_var=log"

echo "[DONE] data_transform deliberate tests finished" | tee -a "${LOG_FILE}"
