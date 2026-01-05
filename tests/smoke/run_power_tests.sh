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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_power_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"
GOLDEN_VALUES_DIR="${ROOT_DIR}/tests/values"
GOLDEN_POWER_PATH="${GOLDEN_VALUES_DIR}/power_golden.csv"
CHECK_POWER_GOLDEN_SCRIPT="${GOLDEN_VALUES_DIR}/check_power_golden.py"

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

WORKSPACE_DIR="${RUN_ROOT}/power_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/power"
LOG_FILE="${RUN_ROOT}/power_test.log"

mkdir -p "${RUN_ROOT}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
REPORT_PATH="${DATASET_DIR}/report_canonical.md"
RDS_PATH="${TMP_BASE}/power_rds.rds"
RDATA_PATH="${TMP_BASE}/power_rdata.RData"
CSV_SEMI_PATH="${TMP_BASE}/power_semicolon.csv"
SAV_PATH="${TMP_BASE}/power_data.sav"
INTERACTIVE_INPUT="${TMP_BASE}/power_interactive.txt"
TEMPLATE_PATH="${TMP_BASE}/power_template.md"

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
if [ ! -f "${GOLDEN_POWER_PATH}" ]; then
  echo "[FAIL] missing golden values: ${GOLDEN_POWER_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${CHECK_POWER_GOLDEN_SCRIPT}" ]; then
  echo "[FAIL] missing golden check script: ${CHECK_POWER_GOLDEN_SCRIPT}" | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_R_PACKAGE_SCRIPT}" pwr >/dev/null 2>&1; then
  echo "[FAIL] pwr not installed." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_SEMPOWER=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" semPower >/dev/null 2>&1; then
  HAS_SEMPOWER=1
fi
HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

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
  local log_path="$1"; shift
  local start_count="$1"; shift
  local status="$1"; shift
  local analysis="$1"; shift
  local mode="$1"; shift
  local metric="$1"; shift
  local min_rows="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" "${status}" "${analysis}" "${mode}" "${metric}" "${min_rows}" "$@"
}

check_power_golden() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local case_id="$1"; shift
  "${PYTHON_BIN}" "${CHECK_POWER_GOLDEN_SCRIPT}" "${log_path}" "${start_count}" "${GOLDEN_POWER_PATH}" "${case_id}"
}

run_cmd() {
  "$@" 2>&1 | tee -a "${LOG_FILE}"
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  run_cmd "$@"
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

run_expect_log() {
  local label="$1"; shift
  local status="$1"; shift
  local analysis="$1"; shift
  local mode="$1"; shift
  local metric="$1"; shift
  local start
  start="$(log_count "${LOG_PATH}")"
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_FILE}"
  set +e
  run_cmd "$@"
  cmd_status=$?
  set -e
  if [ "${cmd_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (expected failure)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  check_log "${LOG_PATH}" "${start}" "${status}" "${analysis}" "${mode}" "${metric}" "1"
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

dataset_label() {
  local path="$1"
  local label
  label="$(basename "${path}")"
  echo "${label%.*}"
}

dataset_dir() {
  local path="$1"
  echo "${WORKSPACE_DIR}/$(dataset_label "${path}")"
}

dataset_log() {
  local path="$1"
  echo "$(dataset_dir "${path}")/analysis_log.jsonl"
}

dataset_nlss() {
  local path="$1"
  echo "$(dataset_dir "${path}")/report_canonical.md"
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

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); power_rdata <- df; save(power_rdata, file = \"${RDATA_PATH}\")"
run_ok "prepare semicolon csv" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write.table(df, \"${CSV_SEMI_PATH}\", sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)"
if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${TEMPLATE_PATH}" <<'TEMPLATE'
---
tokens:
  title: "Power analysis"
  table_title: "Power analysis results."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "analysis"
      label: "Analysis"
    - key: "mode"
      label: "Mode"
    - key: "effect_metric"
      label: "Effect"
    - key: "effect_size"
      label: "Effect size"
    - key: "alpha"
      label: "alpha"
    - key: "power"
      label: "Power"
    - key: "n_total"
      label: "N"
      drop_if_empty: true
    - key: "n1"
      label: "n1"
      drop_if_empty: true
    - key: "n2"
      label: "n2"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n"
  drop_empty: true
---
POWER_TEMPLATE_MARKER

{{table_body}}

{{narrative}}
TEMPLATE

printf "%s\n" \
  "csv" \
  "${DATA_GOLDEN}" \
  "," \
  "TRUE" \
  "ttest" \
  "apriori" \
  "d" \
  "0.5" \
  "0.05" \
  "0.8" \
  "two.sided" \
  "two-sample" \
  "1" \
  "0" \
  "" \
  "" \
  "" \
  "" \
  "2" \
  "1" \
  "" \
  "0.05" \
  "0.08" \
  "FALSE" \
  "" \
  "" \
  "" \
  "" \
  "" \
  "" \
  "" \
  "2" \
  "" \
  "interactive power test" \
  "TRUE" \
  > "${INTERACTIVE_INPUT}"

RDS_LOG_PATH="$(dataset_log "${RDS_PATH}")"
RDATA_LOG_PATH="$(dataset_log "${RDATA_PATH}")"
CSV_SEMI_LOG_PATH="$(dataset_log "${CSV_SEMI_PATH}")"
SAV_LOG_PATH="$(dataset_log "${SAV_PATH}")"

start="$(log_count "${RDS_LOG_PATH}")"
run_ok "rds input" Rscript "${R_SCRIPT_DIR}/power.R" --rds "${RDS_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
check_log "${RDS_LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1"

start="$(log_count "${RDATA_LOG_PATH}")"
run_ok "rdata input" Rscript "${R_SCRIPT_DIR}/power.R" --rdata "${RDATA_PATH}" --df power_rdata --analysis correlation --mode posthoc --effect-size 0.2 --n 100
check_log "${RDATA_LOG_PATH}" "${start}" "-" "correlation" "posthoc" "r" "1"

start="$(log_count "${CSV_SEMI_LOG_PATH}")"
run_ok "csv semicolon input" Rscript "${R_SCRIPT_DIR}/power.R" --csv "${CSV_SEMI_PATH}" --sep ";" --header TRUE --analysis correlation --mode posthoc --effect-size 0.2 --n 120
check_log "${CSV_SEMI_LOG_PATH}" "${start}" "-" "correlation" "posthoc" "r" "1"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start="$(log_count "${SAV_LOG_PATH}")"
  run_ok "sav input" Rscript "${R_SCRIPT_DIR}/power.R" --sav "${SAV_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
  check_log "${SAV_LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1"
fi

start="$(log_count "${LOG_PATH}")"
run_ok "interactive ttest" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/power.R" --interactive
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest apriori" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8 --alpha 0.05
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1" "effect_source=user"
run_ok "power golden (ttest apriori two-sample)" check_power_golden "${LOG_PATH}" "${start}" "ttest_apriori_two_sample"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest apriori estimate effect" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --estimate-effect TRUE --vars outcome_anova --group group2 --power 0.8 --alpha 0.05
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1" "effect_source=estimated"
run_ok "power golden (ttest apriori estimated)" check_power_golden "${LOG_PATH}" "${start}" "ttest_apriori_estimated"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest posthoc two-sample" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode posthoc --t-type two-sample --effect-size 0.4 --n1 40 --n2 60 --alternative greater
check_log "${LOG_PATH}" "${start}" "-" "ttest" "posthoc" "d" "1"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest sensitivity one-sample" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode sensitivity --t-type one-sample --n 40 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "ttest" "sensitivity" "d" "1" "power_min=0.8"

start="$(log_count "${LOG_PATH}")"
run_ok "anova posthoc" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode posthoc --groups 3 --n-per-group 25 --effect-metric f --effect-size 0.25
check_log "${LOG_PATH}" "${start}" "-" "anova" "posthoc" "f" "1"
run_ok "power golden (anova posthoc)" check_power_golden "${LOG_PATH}" "${start}" "anova_posthoc"

start="$(log_count "${LOG_PATH}")"
run_ok "anova apriori eta2" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode apriori --groups 3 --effect-metric eta2 --effect-size 0.06 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "anova" "apriori" "eta2" "1"
run_ok "power golden (anova apriori eta2)" check_power_golden "${LOG_PATH}" "${start}" "anova_apriori_eta2"

start="$(log_count "${LOG_PATH}")"
run_ok "correlation sensitivity" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode sensitivity --n 100 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "correlation" "sensitivity" "r" "1"
run_ok "power golden (correlation sensitivity)" check_power_golden "${LOG_PATH}" "${start}" "corr_sensitivity"

start="$(log_count "${LOG_PATH}")"
run_ok "correlation posthoc" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode posthoc --effect-size 0.25 --n 150 --alternative greater
check_log "${LOG_PATH}" "${start}" "-" "correlation" "posthoc" "r" "1"
run_ok "power golden (correlation posthoc greater)" check_power_golden "${LOG_PATH}" "${start}" "corr_posthoc_greater"

start="$(log_count "${LOG_PATH}")"
run_ok "regression posthoc r2" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode posthoc --effect-metric r2 --effect-size 0.13 --n 120 --u 3
check_log "${LOG_PATH}" "${start}" "-" "regression" "posthoc" "r2" "1"
run_ok "power golden (regression posthoc r2)" check_power_golden "${LOG_PATH}" "${start}" "reg_posthoc_r2"

start="$(log_count "${LOG_PATH}")"
run_ok "regression sensitivity" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode sensitivity --n 100 --u 3 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "regression" "sensitivity" "f2" "1" "power_min=0.8"
run_ok "power golden (regression sensitivity)" check_power_golden "${LOG_PATH}" "${start}" "reg_sensitivity"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest posthoc n-total less" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode posthoc --t-type one-sample --effect-size 0.3 --n-total 60 --alternative less
check_log "${LOG_PATH}" "${start}" "-" "ttest" "posthoc" "d" "1" "t_type=one-sample" "alternative=less" "n_total=60"
run_ok "power golden (ttest posthoc one-sample less)" check_power_golden "${LOG_PATH}" "${start}" "ttest_posthoc_one_sample_less"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest posthoc ratio" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode posthoc --t-type two-sample --effect-size 0.3 --n-per-group 30 --ratio 1.5
check_log "${LOG_PATH}" "${start}" "-" "ttest" "posthoc" "d" "1" "t_type=two-sample" "ratio=1.5" "n1=30" "n2=45"
run_ok "power golden (ttest posthoc ratio)" check_power_golden "${LOG_PATH}" "${start}" "ttest_posthoc_ratio"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest sensitivity two-sample" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode sensitivity --t-type two-sample --n 90 --ratio 2 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "ttest" "sensitivity" "d" "1" "t_type=two-sample" "ratio=2"
run_ok "power golden (ttest sensitivity two-sample ratio)" check_power_golden "${LOG_PATH}" "${start}" "ttest_sensitivity_two_sample_ratio"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest estimate effect paired" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type paired --estimate-effect TRUE --x pre_score --y post_score --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1" "t_type=paired" "effect_source=estimated"

start="$(log_count "${LOG_PATH}")"
run_ok "ttest estimate effect one-sample" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type one-sample --estimate-effect TRUE --vars outcome_anova --mu 0 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1" "t_type=one-sample" "effect_source=estimated"

start="$(log_count "${LOG_PATH}")"
run_ok "anova sensitivity" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode sensitivity --groups 4 --n-per-group 20 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "anova" "sensitivity" "f" "1" "groups=4"
run_ok "power golden (anova sensitivity)" check_power_golden "${LOG_PATH}" "${start}" "anova_sensitivity"

start="$(log_count "${LOG_PATH}")"
run_ok "anova posthoc n-total" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode posthoc --groups 3 --n 90 --effect-metric f --effect-size 0.25
check_log "${LOG_PATH}" "${start}" "-" "anova" "posthoc" "f" "1" "n_per_group=30"

start="$(log_count "${LOG_PATH}")"
run_ok "anova estimate effect" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode apriori --estimate-effect TRUE --dv outcome_anova --group group3 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "anova" "apriori" "eta2" "1" "effect_source=estimated"
run_ok "power golden (anova estimated)" check_power_golden "${LOG_PATH}" "${start}" "anova_estimated"

start="$(log_count "${LOG_PATH}")"
run_ok "correlation estimate effect" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode apriori --estimate-effect TRUE --x x1 --y x2 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "correlation" "apriori" "r" "1" "effect_source=estimated"
run_ok "power golden (correlation estimated)" check_power_golden "${LOG_PATH}" "${start}" "corr_estimated"

start="$(log_count "${LOG_PATH}")"
run_ok "correlation posthoc less" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode posthoc --effect-size 0.2 --n 120 --alternative less
check_log "${LOG_PATH}" "${start}" "-" "correlation" "posthoc" "r" "1" "alternative=less"

start="$(log_count "${LOG_PATH}")"
run_ok "regression apriori f2" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode apriori --effect-metric f2 --effect-size 0.15 --u 3 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "regression" "apriori" "f2" "1" "u=3"
run_ok "power golden (regression apriori f2)" check_power_golden "${LOG_PATH}" "${start}" "reg_apriori_f2"

start="$(log_count "${LOG_PATH}")"
run_ok "regression estimate effect" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode apriori --estimate-effect TRUE --dv outcome_reg --ivs x1,x2,x3 --u 3 --power 0.8
check_log "${LOG_PATH}" "${start}" "-" "regression" "apriori" "r2" "1" "effect_source=estimated" "u=3"
run_ok "power golden (regression estimated)" check_power_golden "${LOG_PATH}" "${start}" "reg_estimated"

if [ "${HAS_SEMPOWER}" -eq 1 ]; then
  start="$(log_count "${LOG_PATH}")"
  run_ok "sem apriori" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
  check_log "${LOG_PATH}" "${start}" "-" "sem" "apriori" "rmsea" "1"
  run_ok "power golden (sem apriori)" check_power_golden "${LOG_PATH}" "${start}" "sem_apriori"
  start="$(log_count "${LOG_PATH}")"
  run_ok "sem posthoc" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode posthoc --df 120 --rmsea0 0.05 --rmsea1 0.08 --n 200
  check_log "${LOG_PATH}" "${start}" "-" "sem" "posthoc" "rmsea" "1" "n_total=200" "rmsea0=0.05" "rmsea1=0.08"
  run_ok "power golden (sem posthoc)" check_power_golden "${LOG_PATH}" "${start}" "sem_posthoc"
  start="$(log_count "${LOG_PATH}")"
  run_ok "sem sensitivity" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode sensitivity --df 120 --rmsea0 0.05 --rmsea1 0.08 --n 120 --power 0.8
  check_log "${LOG_PATH}" "${start}" "-" "sem" "sensitivity" "rmsea" "1" "n_total=120" "rmsea0=0.05"
  run_ok "power golden (sem sensitivity)" check_power_golden "${LOG_PATH}" "${start}" "sem_sensitivity"
else
  echo "[WARN] semPower not installed; skipping SEM power test." | tee -a "${LOG_FILE}"
fi

start="$(log_count "${LOG_PATH}")"
run_ok "template override" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.4 --power 0.8 --template "${TEMPLATE_PATH}"
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1"
assert_contains "${REPORT_PATH}" "POWER_TEMPLATE_MARKER"

start="$(log_count "${LOG_PATH}")"
run_ok "digits formatting" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.1234 --power 0.8 --digits 4
check_log "${LOG_PATH}" "${start}" "-" "ttest" "apriori" "d" "1"
assert_contains "${REPORT_PATH}" "0.1234"

start="$(log_count "${LOG_PATH}")"
run_ok "user prompt" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode posthoc --effect-size 0.25 --n 140 --user-prompt power_deliberate_prompt
check_log "${LOG_PATH}" "${start}" "-" "correlation" "posthoc" "r" "1" "user_prompt=power_deliberate_prompt"

before_count="$(log_count "${LOG_PATH}")"
run_ok "log disabled" Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8 --log FALSE
after_count="$(log_count "${LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "log disabled"

run_expect_log "missing effect size" "invalid_input" "ttest" "apriori" "d" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type two-sample --power 0.8

run_expect_log "invalid effect metric" "invalid_input" "ttest" "apriori" "r" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --effect-metric r --effect-size 0.3 --power 0.8

run_expect_log "missing n for correlation posthoc" "invalid_input" "correlation" "posthoc" "r" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode posthoc --effect-size 0.3

run_expect_log "ttest paired missing x/y estimate effect" "invalid_input" "ttest" "apriori" "d" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode apriori --t-type paired --estimate-effect TRUE --power 0.8

run_expect_log "anova estimate effect missing group" "invalid_input" "anova" "apriori" "eta2" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode apriori --estimate-effect TRUE --dv outcome_anova --power 0.8

run_expect_log "correlation estimate effect missing x/y" "invalid_input" "correlation" "apriori" "r" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis correlation --mode apriori --estimate-effect TRUE --power 0.8

run_expect_log "regression estimate effect missing ivs" "invalid_input" "regression" "apriori" "r2" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode apriori --estimate-effect TRUE --dv outcome_reg --u 3 --power 0.8

run_expect_log "invalid u for regression apriori" "invalid_input" "regression" "apriori" "f2" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode apriori --effect-size 0.15 --effect-metric f2 --power 0.8 --u 0

run_expect_log "anova groups too small" "invalid_input" "anova" "apriori" "f" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis anova --mode apriori --groups 1 --effect-size 0.25 --power 0.8

run_expect_log "missing n for ttest posthoc" "invalid_input" "ttest" "posthoc" "d" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis ttest --mode posthoc --t-type one-sample --effect-size 0.3

run_expect_log "regression sample too small" "invalid_input" "regression" "posthoc" "f2" \
  Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis regression --mode posthoc --effect-size 0.2 --effect-metric f2 --n 2 --u 3

if [ "${HAS_SEMPOWER}" -eq 1 ]; then
  run_expect_log "sem estimate effect unsupported" "invalid_input" "sem" "apriori" "rmsea" \
    Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8 --estimate-effect TRUE

  run_expect_log "sem missing df" "invalid_input" "sem" "apriori" "rmsea" \
    Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode apriori --rmsea0 0.05 --rmsea1 0.08 --power 0.8

  run_expect_log "sem invalid rmsea1" "invalid_input" "sem" "apriori" "rmsea" \
    Rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 -0.01 --power 0.8
fi

echo "[DONE] power tests finished" | tee -a "${LOG_FILE}"
