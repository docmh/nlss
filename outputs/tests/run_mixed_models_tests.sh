#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
CHECK_SCRIPT="${ROOT_DIR}/outputs/tests/check_mixed_models_log.py"
CHECK_PKG_SCRIPT="${ROOT_DIR}/outputs/tests/check_r_package.R"
PREP_SCRIPT="${ROOT_DIR}/outputs/tests/mixed_models_prep.R"
DATA_GOLDEN="${ROOT_DIR}/outputs/tests/golden_dataset.csv"

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
        lines[idx] = (" " * indent) + f'{key_name}: "{value}"'
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

WORKSPACE_DIR="${RUN_ROOT}/mixed_models_workspace"
TMP_BASE="${RUN_ROOT}/tmp/mixed_models"
LOG_FILE="${RUN_ROOT}/mixed_models_test.log"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${TMP_BASE}"

export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

: > "${LOG_FILE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R or use scripts/run_rscript.ps1 on Windows." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

cleanup() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}"
}
trap cleanup EXIT

set_config_value defaults.output_dir "${WORKSPACE_DIR}"

DATASET_LABEL="mixed_models_long"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
APA_REPORT_PATH="${DATASET_DIR}/apa_report.md"
MIXED_DATA_PATH="${TMP_BASE}/${DATASET_LABEL}.csv"

rm -f "${APA_REPORT_PATH}" "${LOG_PATH}"

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
  python3 "${CHECK_SCRIPT}" "$@"
}

run_expect_status() {
  local label="$1"; shift
  local status="$1"; shift
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_FILE}"
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
  local placeholders=(- - - - - - - - - - - - - - - - - - -)
  if check_log "${LOG_PATH}" "${start_count}" "${status}" "${placeholders[@]}"; then
    echo "[PASS] ${label} (${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

run_expect_status_log() {
  local log_path="$1"; shift
  local label="$1"; shift
  local status="$1"; shift
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_FILE}"
  local start_count
  start_count="$(log_count "${log_path}")"
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  local placeholders=(- - - - - - - - - - - - - - - - - - -)
  if check_log "${log_path}" "${start_count}" "${status}" "${placeholders[@]}"; then
    echo "[PASS] ${label} (${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

run_ok "mixed_models prep" Rscript "${PREP_SCRIPT}" "${DATA_GOLDEN}" "${MIXED_DATA_PATH}"
run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${MIXED_DATA_PATH}"

if ! Rscript "${CHECK_PKG_SCRIPT}" lme4 >/dev/null 2>&1; then
  echo "[FAIL] mixed_models tests require the lme4 package." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_EMMEANS=0
if Rscript "${CHECK_PKG_SCRIPT}" emmeans >/dev/null 2>&1; then
  HAS_EMMEANS=1
fi

HAS_LMERTEST=0
if Rscript "${CHECK_PKG_SCRIPT}" lmerTest >/dev/null 2>&1; then
  HAS_LMERTEST=1
fi

DF_METHOD_EXPECT="satterthwaite"
if [ "${HAS_LMERTEST}" -eq 0 ]; then
  DF_METHOD_EXPECT="none"
fi

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models clean formula" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --reml TRUE \
  --type III \
  --df-method satterthwaite \
  --diagnostics TRUE
check_log "${LOG_PATH}" "${start_count}" - "score ~ time + group3 + x1 + (1|id)" - "(1|id)" "III" "${DF_METHOD_EXPECT}" none TRUE none none none 0.95 bobyqa 100000 TRUE gt0 - - gt0 absent

echo "[PASS] mixed_models clean formula (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models fixed/random standardize" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv score \
  --fixed time,group3,x1 \
  --random "1|id" \
  --standardize predictors \
  --type II \
  --diagnostics FALSE \
  --reml FALSE \
  --maxfun 20000
check_log "${LOG_PATH}" "${start_count}" - - score "(1|id)" "II" none predictors FALSE none none none 0.95 bobyqa 20000 FALSE gt0 - - 0 present

echo "[PASS] mixed_models fixed/random standardize (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models random shorthand" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv score \
  --fixed time \
  --random id \
  --type I \
  --df-method none \
  --conf-level 0.9 \
  --optimizer nloptwrap \
  --maxfun 5000
check_log "${LOG_PATH}" "${start_count}" - - score "(1|id)" "I" none none TRUE none none none 0.9 nloptwrap 5000 TRUE gt0 - - gt0 absent

echo "[PASS] mixed_models random shorthand (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models emmeans contrasts" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time * group3 + x1 + (1|id)" \
  --emmeans "time*group3" \
  --contrasts pairwise \
  --p-adjust holm \
  --conf-level 0.9
if [ "${HAS_EMMEANS}" -eq 1 ]; then
  emmeans_rows="gt0"
  contrast_rows="gt0"
else
  emmeans_rows="0"
  contrast_rows="0"
fi
check_log "${LOG_PATH}" "${start_count}" - "score ~ time * group3 + x1 + (1|id)" - "(1|id)" "III" "${DF_METHOD_EXPECT}" none TRUE "time*group3" pairwise holm 0.9 bobyqa 100000 TRUE gt0 "${emmeans_rows}" "${contrast_rows}" gt0 absent

echo "[PASS] mixed_models emmeans contrasts (log)" | tee -a "${LOG_FILE}"

run_expect_status "mixed_models missing random" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv score \
  --fixed time,group3

run_expect_status "mixed_models missing dv" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --fixed time \
  --random "1|id"

run_expect_status "mixed_models missing variable" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + missing_var + (1|id)"

run_expect_status "mixed_models nonnumeric dv" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv gender \
  --fixed time \
  --random "1|id"

EMPTY_DATA_PATH="${TMP_BASE}/mixed_models_empty.csv"
Rscript - "${MIXED_DATA_PATH}" "${EMPTY_DATA_PATH}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
output_path <- args[2]
input <- read.csv(input_path, stringsAsFactors = FALSE)
input$score <- NA
write.csv(input, output_path, row.names = FALSE)
RS

run_ok "init workspace (empty)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${EMPTY_DATA_PATH}"

EMPTY_LOG_PATH="${WORKSPACE_DIR}/mixed_models_empty/analysis_log.jsonl"
run_expect_status_log "${EMPTY_LOG_PATH}" "mixed_models no complete cases" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${WORKSPACE_DIR}/mixed_models_empty/mixed_models_empty.parquet" \
  --formula "score ~ time + (1|id)"

echo "[DONE] mixed_models deliberate tests finished" | tee -a "${LOG_FILE}"
