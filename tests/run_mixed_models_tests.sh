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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_mixed_models_log.py"
CHECK_PKG_SCRIPT="${ROOT_DIR}/tests/check_r_package.R"
PREP_SCRIPT="${ROOT_DIR}/tests/mixed_models_prep.R"

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

WORKSPACE_DIR="${RUN_ROOT}/mixed_models_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/mixed_models"
LOG_FILE="${RUN_ROOT}/mixed_models_test.log"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"

export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

: > "${LOG_FILE}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

cleanup() {
  if [ -f "${CONFIG_BAK}" ]; then
    cp "${CONFIG_BAK}" "${CONFIG_PATH}"
    rm -f "${CONFIG_BAK}"
  fi
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

DATASET_LABEL="mixed_models_long"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
MIXED_DATA_PATH="${TMP_BASE}/${DATASET_LABEL}.csv"
CSV_SEMI_LABEL="mixed_models_csv_semi"
CSV_SEMI_PATH="${TMP_BASE}/${CSV_SEMI_LABEL}.csv"
CSV_SEMI_DIR="${WORKSPACE_DIR}/${CSV_SEMI_LABEL}"
CSV_SEMI_LOG_PATH="${CSV_SEMI_DIR}/analysis_log.jsonl"
RDS_LABEL="mixed_models_rds"
RDS_PATH="${TMP_BASE}/${RDS_LABEL}.rds"
RDS_DIR="${WORKSPACE_DIR}/${RDS_LABEL}"
RDS_LOG_PATH="${RDS_DIR}/analysis_log.jsonl"
RDATA_DF_NAME="mixed_models_rdata_df"
RDATA_LABEL="${RDATA_DF_NAME}"
RDATA_PATH="${TMP_BASE}/mixed_models_rdata.RData"
RDATA_DIR="${WORKSPACE_DIR}/${RDATA_LABEL}"
RDATA_LOG_PATH="${RDATA_DIR}/analysis_log.jsonl"
RDS_BAD_PATH="${TMP_BASE}/mixed_models_bad.rds"
SAV_LABEL="mixed_models_sav"
SAV_PATH="${TMP_BASE}/${SAV_LABEL}.sav"
SAV_DIR="${WORKSPACE_DIR}/${SAV_LABEL}"
SAV_LOG_PATH="${SAV_DIR}/analysis_log.jsonl"
CONTRAST_JSON_PATH="${TMP_BASE}/mixed_models_contrasts.json"

rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}"
cd "${WORKSPACE_DIR}"

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

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

assert_log_unchanged() {
  local before="$1"
  local after="$2"
  local label="$3"
  if [ "${after}" -ne "${before}" ]; then
    echo "[FAIL] ${label} (expected log count unchanged)" | tee -a "${LOG_FILE}"
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

expect_log() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local status="-"
  local formula="-"
  local dv="-"
  local random="-"
  local type="-"
  local df_method="-"
  local standardize="-"
  local diagnostics="-"
  local emmeans="-"
  local contrasts="-"
  local p_adjust="-"
  local conf_level="-"
  local optimizer="-"
  local maxfun="-"
  local reml="-"
  local fixed_rows="-"
  local emmeans_rows="-"
  local contrast_rows="-"
  local diagnostics_rows="-"
  local std_beta="-"
  local digits="-"
  local user_prompt="-"
  local shapiro="-"
  local r2_expect=""
  local icc_expect=""

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --status)
        status="$2"
        shift 2
        ;;
      --formula)
        formula="$2"
        shift 2
        ;;
      --dv)
        dv="$2"
        shift 2
        ;;
      --random)
        random="$2"
        shift 2
        ;;
      --type)
        type="$2"
        shift 2
        ;;
      --df-method)
        df_method="$2"
        shift 2
        ;;
      --standardize)
        standardize="$2"
        shift 2
        ;;
      --diagnostics)
        diagnostics="$2"
        shift 2
        ;;
      --emmeans)
        emmeans="$2"
        shift 2
        ;;
      --contrasts)
        contrasts="$2"
        shift 2
        ;;
      --p-adjust)
        p_adjust="$2"
        shift 2
        ;;
      --conf-level)
        conf_level="$2"
        shift 2
        ;;
      --optimizer)
        optimizer="$2"
        shift 2
        ;;
      --maxfun)
        maxfun="$2"
        shift 2
        ;;
      --reml)
        reml="$2"
        shift 2
        ;;
      --fixed-rows)
        fixed_rows="$2"
        shift 2
        ;;
      --emmeans-rows)
        emmeans_rows="$2"
        shift 2
        ;;
      --contrast-rows)
        contrast_rows="$2"
        shift 2
        ;;
      --diagnostics-rows)
        diagnostics_rows="$2"
        shift 2
        ;;
      --std-beta)
        std_beta="$2"
        shift 2
        ;;
      --digits)
        digits="$2"
        shift 2
        ;;
      --user-prompt)
        user_prompt="$2"
        shift 2
        ;;
      --shapiro)
        shapiro="$2"
        shift 2
        ;;
      --r2)
        r2_expect="$2"
        shift 2
        ;;
      --icc)
        icc_expect="$2"
        shift 2
        ;;
      *)
        echo "[FAIL] unknown expect flag: $1" | tee -a "${LOG_FILE}"
        exit 1
        ;;
    esac
  done

  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" \
    "${status}" "${formula}" "${dv}" "${random}" "${type}" "${df_method}" "${standardize}" "${diagnostics}" \
    "${emmeans}" "${contrasts}" "${p_adjust}" "${conf_level}" "${optimizer}" "${maxfun}" "${reml}" \
    "${fixed_rows}" "${emmeans_rows}" "${contrast_rows}" "${diagnostics_rows}" "${std_beta}" \
    "${digits}" "${user_prompt}" "${shapiro}" \
    ${r2_expect:+ "r2=${r2_expect}"} ${icc_expect:+ "icc=${icc_expect}"}
}

expect_log_main() {
  local start_count="$1"; shift
  expect_log "${LOG_PATH}" "${start_count}" "$@"
}

run_expect_invalid() {
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
  if expect_log_main "${start_count}" --status "${status}"; then
    echo "[PASS] ${label} (${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

run_expect_invalid_log() {
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
  if expect_log "${log_path}" "${start_count}" --status "${status}"; then
    echo "[PASS] ${label} (${status})" | tee -a "${LOG_FILE}"
  else
    echo "[FAIL] ${label} (status ${status} not logged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
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

run_ok "mixed_models prep" Rscript "${PREP_SCRIPT}" "${DATA_GOLDEN}" "${MIXED_DATA_PATH}"

cat >"${CONTRAST_JSON_PATH}" <<'EOF'
{
  "term": "group3",
  "contrasts": {
    "A_vs_B": {"A": 1, "B": -1, "C": 0},
    "A_vs_C": [1, 0, -1]
  }
}
EOF
run_ok "mixed_models format prep" Rscript - "${MIXED_DATA_PATH}" "${CSV_SEMI_PATH}" "${RDS_PATH}" "${RDATA_PATH}" "${RDATA_DF_NAME}" "${RDS_BAD_PATH}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
csv_semi_path <- args[2]
rds_path <- args[3]
rdata_path <- args[4]
rdata_df_name <- args[5]
rds_bad_path <- args[6]
df <- read.csv(input_path, stringsAsFactors = FALSE)
write.table(df, csv_semi_path, sep = ";", row.names = FALSE, col.names = TRUE)
saveRDS(df, rds_path)
assign(rdata_df_name, df)
other_df <- df[1:min(5, nrow(df)), , drop = FALSE]
save(list = c(rdata_df_name, "other_df"), file = rdata_path)
saveRDS(list(note = "not a data frame"), rds_bad_path)
RS
run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${MIXED_DATA_PATH}"

if ! Rscript "${CHECK_PKG_SCRIPT}" lme4 >/dev/null 2>&1; then
  echo "[FAIL] mixed_models tests require the lme4 package." | tee -a "${LOG_FILE}"
  exit 1
fi
if ! Rscript "${CHECK_PKG_SCRIPT}" performance >/dev/null 2>&1; then
  echo "[FAIL] mixed_models tests require the performance package." | tee -a "${LOG_FILE}"
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

HAS_PBKRTEST=0
if Rscript "${CHECK_PKG_SCRIPT}" pbkrtest >/dev/null 2>&1; then
  HAS_PBKRTEST=1
fi

HAS_HAVEN=0
if Rscript "${CHECK_PKG_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi

HAS_FOREIGN=0
if Rscript "${CHECK_PKG_SCRIPT}" foreign >/dev/null 2>&1; then
  HAS_FOREIGN=1
fi

DF_METHOD_EXPECT="satterthwaite"
if [ "${HAS_LMERTEST}" -eq 0 ]; then
  DF_METHOD_EXPECT="none"
fi

DF_METHOD_KR_EXPECT="kenward-roger"
if [ "${HAS_LMERTEST}" -eq 0 ]; then
  DF_METHOD_KR_EXPECT="none"
fi

HAS_SAV=0
if [ "${HAS_HAVEN}" -eq 1 ]; then
  HAS_SAV=1
  run_ok "mixed_models sav prep" Rscript - "${MIXED_DATA_PATH}" "${SAV_PATH}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
sav_path <- args[2]
df <- read.csv(input_path, stringsAsFactors = FALSE)
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("haven not available")
}
haven::write_sav(df, sav_path)
RS
else
  echo "[WARN] skipping SAV input tests (haven not installed)" | tee -a "${LOG_FILE}"
fi

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models clean formula" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --reml TRUE \
  --type III \
  --df-method satterthwaite \
  --diagnostics TRUE \
  --max-shapiro-n 100000
expect_log_main "${start_count}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent \
  --r2 present \
  --icc present \
  --digits 2 \
  --shapiro present

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
expect_log_main "${start_count}" \
  --dv score \
  --random "(1|id)" \
  --type II \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize predictors \
  --diagnostics FALSE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 20000 \
  --reml FALSE \
  --fixed-rows gt0 \
  --diagnostics-rows 0 \
  --std-beta present

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
expect_log_main "${start_count}" \
  --dv score \
  --random "(1|id)" \
  --type I \
  --df-method none \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.9 \
  --optimizer nloptwrap \
  --maxfun 5000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent

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
expect_log_main "${start_count}" \
  --formula "score ~ time * group3 + x1 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans "time*group3" \
  --contrasts pairwise \
  --p-adjust holm \
  --conf-level 0.9 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --emmeans-rows "${emmeans_rows}" \
  --contrast-rows "${contrast_rows}" \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models emmeans contrasts (log)" | tee -a "${LOG_FILE}"

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  start_count="$(log_count "${LOG_PATH}")"
  run_ok "mixed_models custom contrasts" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time + group3 + (1|id)" \
    --emmeans "group3" \
    --contrasts custom \
    --contrast-file "${CONTRAST_JSON_PATH}"
  expect_log_main "${start_count}" \
    --formula "score ~ time + group3 + (1|id)" \
    --random "(1|id)" \
    --type III \
    --df-method "${DF_METHOD_EXPECT}" \
    --standardize none \
    --diagnostics TRUE \
    --emmeans "group3" \
    --contrasts custom \
    --p-adjust holm \
    --conf-level 0.95 \
    --optimizer bobyqa \
    --maxfun 100000 \
    --reml TRUE \
    --fixed-rows gt0 \
    --emmeans-rows gt0 \
    --contrast-rows gt0 \
    --diagnostics-rows gt0 \
    --std-beta absent
  echo "[PASS] mixed_models custom contrasts (log)" | tee -a "${LOG_FILE}"

  start_count="$(log_count "${LOG_PATH}")"
  run_ok "mixed_models method contrasts" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time + group3 + (1|id)" \
    --emmeans "group3" \
    --contrasts trt.vs.ctrl
  expect_log_main "${start_count}" \
    --formula "score ~ time + group3 + (1|id)" \
    --random "(1|id)" \
    --type III \
    --df-method "${DF_METHOD_EXPECT}" \
    --standardize none \
    --diagnostics TRUE \
    --emmeans "group3" \
    --contrasts trt.vs.ctrl \
    --p-adjust holm \
    --conf-level 0.95 \
    --optimizer bobyqa \
    --maxfun 100000 \
    --reml TRUE \
    --fixed-rows gt0 \
    --emmeans-rows gt0 \
    --contrast-rows gt0 \
    --diagnostics-rows gt0 \
    --std-beta absent
  echo "[PASS] mixed_models method contrasts (log)" | tee -a "${LOG_FILE}"
else
  echo "[SKIP] planned contrasts (emmeans not installed)" | tee -a "${LOG_FILE}"
fi

if [ "${HAS_LMERTEST}" -eq 1 ] && [ "${HAS_PBKRTEST}" -eq 0 ]; then
  run_expect_fail "mixed_models df-method kenward-roger missing pbkrtest" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time + group3 + (1|id)" \
    --type II \
    --df-method kenward-roger
else
  start_count="$(log_count "${LOG_PATH}")"
  run_ok "mixed_models df-method kenward-roger" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time + group3 + (1|id)" \
    --type II \
    --df-method kenward-roger
  expect_log_main "${start_count}" \
    --formula "score ~ time + group3 + (1|id)" \
    --random "(1|id)" \
    --type II \
    --df-method "${DF_METHOD_KR_EXPECT}" \
    --standardize none \
    --diagnostics TRUE \
    --emmeans none \
    --contrasts none \
    --p-adjust none \
    --conf-level 0.95 \
    --optimizer bobyqa \
    --maxfun 100000 \
    --reml TRUE \
    --fixed-rows gt0 \
    --diagnostics-rows gt0 \
    --std-beta absent
  echo "[PASS] mixed_models df-method kenward-roger (log)" | tee -a "${LOG_FILE}"
fi

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models emmeans only" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + (1|id)" \
  --emmeans time
if [ "${HAS_EMMEANS}" -eq 1 ]; then
  emmeans_only_rows="gt0"
else
  emmeans_only_rows="0"
fi
expect_log_main "${start_count}" \
  --formula "score ~ time + group3 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans time \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --emmeans-rows "${emmeans_only_rows}" \
  --contrast-rows 0 \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models emmeans only (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models contrasts without emmeans" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + (1|id)" \
  --contrasts pairwise
expect_log_main "${start_count}" \
  --formula "score ~ time + group3 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --emmeans-rows 0 \
  --contrast-rows 0 \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models contrasts without emmeans (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${CSV_SEMI_LOG_PATH}")"
run_ok "mixed_models csv semicolon" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --csv "${CSV_SEMI_PATH}" \
  --sep ";" \
  --header TRUE \
  --formula "score ~ time + group3 + (1|id)"
expect_log "${CSV_SEMI_LOG_PATH}" "${start_count}" \
  --formula "score ~ time + group3 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models csv semicolon (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${RDS_LOG_PATH}")"
run_ok "mixed_models rds input" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --rds "${RDS_PATH}" \
  --formula "score ~ time + group3 + (1|id)"
expect_log "${RDS_LOG_PATH}" "${start_count}" \
  --formula "score ~ time + group3 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models rds input (log)" | tee -a "${LOG_FILE}"

start_count="$(log_count "${RDATA_LOG_PATH}")"
run_ok "mixed_models rdata input" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --rdata "${RDATA_PATH}" \
  --df "${RDATA_DF_NAME}" \
  --formula "score ~ time + group3 + (1|id)"
expect_log "${RDATA_LOG_PATH}" "${start_count}" \
  --formula "score ~ time + group3 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent

echo "[PASS] mixed_models rdata input (log)" | tee -a "${LOG_FILE}"

if [ "${HAS_SAV}" -eq 1 ]; then
  start_count="$(log_count "${SAV_LOG_PATH}")"
  run_ok "mixed_models sav input" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --sav "${SAV_PATH}" \
    --formula "score ~ time + group3 + (1|id)"
  expect_log "${SAV_LOG_PATH}" "${start_count}" \
    --formula "score ~ time + group3 + (1|id)" \
    --random "(1|id)" \
    --type III \
    --df-method "${DF_METHOD_EXPECT}" \
    --standardize none \
    --diagnostics TRUE \
    --emmeans none \
    --contrasts none \
    --p-adjust none \
    --conf-level 0.95 \
    --optimizer bobyqa \
    --maxfun 100000 \
    --reml TRUE \
    --fixed-rows gt0 \
    --diagnostics-rows gt0 \
    --std-beta absent
  echo "[PASS] mixed_models sav input (log)" | tee -a "${LOG_FILE}"
fi

start_count="$(log_count "${LOG_PATH}")"
run_ok "mixed_models option normalization" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --type IV \
  --df-method bogus \
  --standardize weird \
  --conf-level 2 \
  --maxfun -10 \
  --max-shapiro-n 2 \
  --digits 4 \
  --user-prompt "edge prompt"
expect_log_main "${start_count}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --random "(1|id)" \
  --type III \
  --df-method "${DF_METHOD_EXPECT}" \
  --standardize none \
  --diagnostics TRUE \
  --emmeans none \
  --contrasts none \
  --p-adjust none \
  --conf-level 0.95 \
  --optimizer bobyqa \
  --maxfun 100000 \
  --reml TRUE \
  --fixed-rows gt0 \
  --diagnostics-rows gt0 \
  --std-beta absent \
  --digits 4 \
  --user-prompt "edge prompt" \
  --shapiro absent

echo "[PASS] mixed_models option normalization (log)" | tee -a "${LOG_FILE}"

reset_report() {
  rm -f "${NLSS_REPORT_PATH}"
}

TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.mixed_models.default)"
TEMPLATE_EMMEANS_ORIG="$(get_config_value templates.mixed_models.emmeans)"
TEMPLATE_DEFAULT_TMP="${TMP_BASE}/mixed_models_default_template.md"
TEMPLATE_EMMEANS_TMP="${TMP_BASE}/mixed_models_emmeans_template.md"

reset_report
run_ok "mixed_models template baseline" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --log FALSE

if [ ! -f "${NLSS_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${NLSS_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

assert_contains "${NLSS_REPORT_PATH}" "Mixed Models"
assert_contains "${NLSS_REPORT_PATH}" "Fixed Effects Estimates"
assert_contains "${NLSS_REPORT_PATH}" "## Narrative"

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  reset_report
  run_ok "mixed_models emmeans template baseline" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time * group3 + x1 + (1|id)" \
    --emmeans "time*group3" \
    --contrasts pairwise \
    --p-adjust holm \
    --log FALSE

  assert_contains "${NLSS_REPORT_PATH}" "Mixed Models: Marginal Means"
  assert_contains "${NLSS_REPORT_PATH}" "Estimated Marginal Means and Contrasts"
fi

cp "$(resolve_template_source "${TEMPLATE_DEFAULT_ORIG}")" "${TEMPLATE_DEFAULT_TMP}"
cp "$(resolve_template_source "${TEMPLATE_EMMEANS_ORIG}")" "${TEMPLATE_EMMEANS_TMP}"

sed -i 's/title: "Mixed Models"/title: "Mixed Models TEMPLATE TEST"/' "${TEMPLATE_DEFAULT_TMP}"
sed -i 's/table_title: "Fixed Effects Estimates"/table_title: "Fixed Effects Estimates TEST"/' "${TEMPLATE_DEFAULT_TMP}"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_DEFAULT_TMP}"
sed -i 's/narrative_heading: "## Narrative"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_DEFAULT_TMP}"

sed -i 's/title: "Mixed Models: Marginal Means"/title: "Mixed Models: Marginal Means TEMPLATE TEST"/' "${TEMPLATE_EMMEANS_TMP}"
sed -i 's/table_title: "Estimated Marginal Means and Contrasts"/table_title: "Estimated Marginal Means TEST"/' "${TEMPLATE_EMMEANS_TMP}"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_EMMEANS_TMP}"
sed -i 's/narrative_heading: "## Narrative"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_EMMEANS_TMP}"

set_config_value templates.mixed_models.default "${TEMPLATE_DEFAULT_TMP}"
set_config_value templates.mixed_models.emmeans "${TEMPLATE_EMMEANS_TMP}"

reset_report
run_ok "mixed_models template modified" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --log FALSE

assert_contains "${NLSS_REPORT_PATH}" "Mixed Models TEMPLATE TEST"
assert_contains "${NLSS_REPORT_PATH}" "Fixed Effects Estimates TEST"
assert_contains "${NLSS_REPORT_PATH}" "*Note-TEST.*"
assert_contains "${NLSS_REPORT_PATH}" "**Narrative TEST**"

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  reset_report
  run_ok "mixed_models emmeans template modified" \
    Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time * group3 + x1 + (1|id)" \
    --emmeans "time*group3" \
    --contrasts pairwise \
    --p-adjust holm \
    --log FALSE

  assert_contains "${NLSS_REPORT_PATH}" "Mixed Models: Marginal Means TEMPLATE TEST"
  assert_contains "${NLSS_REPORT_PATH}" "Estimated Marginal Means TEST"
  assert_contains "${NLSS_REPORT_PATH}" "*Note-TEST.*"
  assert_contains "${NLSS_REPORT_PATH}" "**Narrative TEST**"
else
  echo "[WARN] skipping emmeans template tests (emmeans not installed)" | tee -a "${LOG_FILE}"
fi

set_config_value templates.mixed_models.default "${TEMPLATE_DEFAULT_ORIG}"
set_config_value templates.mixed_models.emmeans "${TEMPLATE_EMMEANS_ORIG}"

before_log="$(log_count "${LOG_PATH}")"
run_ok "mixed_models log disabled" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + (1|id)" \
  --log FALSE
after_log="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_log}" "${after_log}" "mixed_models log disabled"

run_expect_invalid "mixed_models missing random" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv score \
  --fixed time,group3

run_expect_invalid "mixed_models missing dv" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --fixed time \
  --random "1|id"

run_expect_invalid "mixed_models missing variable" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + missing_var + (1|id)"

run_expect_invalid "mixed_models nonnumeric dv" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --dv gender \
  --fixed time \
  --random "1|id"

run_expect_invalid "mixed_models formula missing random" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3"

run_expect_invalid "mixed_models invalid formula" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + (1|id"

run_expect_invalid "mixed_models contrasts missing file" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + (1|id)" \
  --emmeans group3 \
  --contrasts custom

run_expect_fail "mixed_models rdata missing df" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --rdata "${RDATA_PATH}" \
  --formula "score ~ time + (1|id)"

run_expect_fail "mixed_models rds not dataframe" \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --rds "${RDS_BAD_PATH}" \
  --formula "score ~ time + (1|id)"

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
run_expect_invalid_log "${EMPTY_LOG_PATH}" "mixed_models no complete cases" invalid_input \
  Rscript "${R_SCRIPT_DIR}/mixed_models.R" \
  --parquet "${WORKSPACE_DIR}/mixed_models_empty/mixed_models_empty.parquet" \
  --formula "score ~ time + (1|id)"

echo "[DONE] mixed_models deliberate tests finished" | tee -a "${LOG_FILE}"
