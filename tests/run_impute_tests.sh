# Copyright (c) 2026 Mike Hammes
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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_impute_log.py"
CHECK_PKG_SCRIPT="${ROOT_DIR}/tests/check_r_package.R"

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

WORKSPACE_DIR="${RUN_ROOT}/impute_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/impute"
LOG_FILE="${RUN_ROOT}/impute_test.log"
RDS_PATH="${TMP_BASE}/impute_input_rds.rds"
RDATA_PATH="${TMP_BASE}/impute_input_rdata.RData"
CSV_SEMI_PATH="${TMP_BASE}/impute_semicolon.csv"
SAV_PATH="${TMP_BASE}/impute_input_sav.sav"
INTERACTIVE_INPUT="${TMP_BASE}/impute_interactive.txt"
CLI_TEMPLATE="${TMP_BASE}/impute_cli_template.md"

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
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
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

HAS_HAVEN=0
if Rscript "${CHECK_PKG_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
HAS_MICE=0
if Rscript "${CHECK_PKG_SCRIPT}" mice >/dev/null 2>&1; then
  HAS_MICE=1
fi
HAS_VIM=0
if Rscript "${CHECK_PKG_SCRIPT}" VIM >/dev/null 2>&1; then
  HAS_VIM=1
fi

AUTO_ENGINE="simple"
if [ "${HAS_MICE}" -eq 1 ]; then
  AUTO_ENGINE="mice"
elif [ "${HAS_VIM}" -eq 1 ]; then
  AUTO_ENGINE="knn"
fi

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

check_log() {
  local log_path="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "$@"
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

assert_parquet_columns() {
  local label="$1"; shift
  local parquet_path="$1"; shift
  local columns="$1"
  run_ok "${label}" Rscript -e "library(arrow); df <- read_parquet(\"${parquet_path}\"); cols <- strsplit(\"${columns}\", \",\", fixed = TRUE)[[1]]; cols <- cols[cols != \"\"]; missing <- setdiff(cols, names(df)); if (length(missing) > 0) stop(paste(\"Missing columns:\", paste(missing, collapse = \", \")))"
}

assert_parquet_no_missing() {
  local label="$1"; shift
  local parquet_path="$1"; shift
  local column="$1"
  run_ok "${label}" Rscript -e "library(arrow); df <- read_parquet(\"${parquet_path}\"); missing <- sum(is.na(df[['${column}']])); if (missing != 0) stop(paste(\"Expected no missing values in\", \"${column}\", \"but found\", missing))"
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

run_ok "prepare rds/rdata inputs" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); impute_rdata <- df; save(impute_rdata, file = \"${RDATA_PATH}\")"
run_ok "prepare semicolon csv" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write.table(df, \"${CSV_SEMI_PATH}\", sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)"
if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

HELP_PATH="${TMP_BASE}/impute_help.txt"
run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/impute.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Imputation"

clean_info="$(prepare_dataset "impute_clean")"
IFS="|" read -r CLEAN_CSV CLEAN_DIR CLEAN_PARQUET <<< "${clean_info}"
CLEAN_LOG="${CLEAN_DIR}/analysis_log.jsonl"
CLEAN_REPORT="${CLEAN_DIR}/report_canonical.md"

start="$(log_count "${CLEAN_LOG}")"
run_ok "impute clean" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${CLEAN_PARQUET}" \
  --vars age,income,cat_var \
  --engine simple \
  --numeric-method median \
  --categorical-method mode \
  --indicator FALSE

check_log "${CLEAN_LOG}" "${start}" 3 "age_imp,income_imp,cat_var_imp" "simple" "false" "true" \
  engine="simple"

assert_contains "${CLEAN_REPORT}" "Imputation Summary"
assert_contains "${CLEAN_REPORT}" "Imputation summary."
assert_contains "${CLEAN_REPORT}" "Table 1"
assert_parquet_columns "impute clean columns" "${CLEAN_PARQUET}" "age_imp,income_imp,cat_var_imp"
assert_parquet_no_missing "impute clean age_imp no missing" "${CLEAN_PARQUET}" "age_imp"
assert_parquet_no_missing "impute clean income_imp no missing" "${CLEAN_PARQUET}" "income_imp"
assert_parquet_no_missing "impute clean cat_var_imp no missing" "${CLEAN_PARQUET}" "cat_var_imp"

indicator_info="$(prepare_dataset "impute_indicator")"
IFS="|" read -r IND_CSV IND_DIR IND_PARQUET <<< "${indicator_info}"
IND_LOG="${IND_DIR}/analysis_log.jsonl"

start="$(log_count "${IND_LOG}")"
run_ok "impute indicator" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${IND_PARQUET}" \
  --vars age,cat_var,all_missing_var \
  --engine simple \
  --numeric-method mean \
  --categorical-method mode \
  --indicator TRUE \
  --indicator-suffix _miss

check_log "${IND_LOG}" "${start}" 3 "age_imp,cat_var_imp,all_missing_var_imp" "simple" "true" "true"

auto_engine_info="$(prepare_dataset "impute_auto_engine")"
IFS="|" read -r AUTO_CSV AUTO_DIR AUTO_PARQUET <<< "${auto_engine_info}"
AUTO_LOG="${AUTO_DIR}/analysis_log.jsonl"
start="$(log_count "${AUTO_LOG}")"
run_ok "impute auto engine" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${AUTO_PARQUET}" \
  --vars age,income \
  --engine auto \
  --indicator FALSE

check_log "${AUTO_LOG}" "${start}" 2 "age_imp,income_imp" "${AUTO_ENGINE}" "false" "true" \
  engine_requested="auto"

auto_method_info="$(prepare_dataset "impute_auto_method")"
IFS="|" read -r AUTO_METHOD_CSV AUTO_METHOD_DIR AUTO_METHOD_PARQUET <<< "${auto_method_info}"
AUTO_METHOD_LOG="${AUTO_METHOD_DIR}/analysis_log.jsonl"
AUTO_METHOD_REPORT="${AUTO_METHOD_DIR}/report_canonical.md"
start="$(log_count "${AUTO_METHOD_LOG}")"
run_ok "impute auto method" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${AUTO_METHOD_PARQUET}" \
  --vars age \
  --engine simple \
  --numeric-method auto \
  --skew-threshold 0.3 \
  --digits 3 \
  --indicator FALSE

check_log "${AUTO_METHOD_LOG}" "${start}" 1 "age_imp" "simple" "false" "true" \
  numeric_method="auto" \
  skew_threshold="0.3" \
  method.age="mean"
assert_contains "${AUTO_METHOD_REPORT}" "Rounding digits: 3"

mode_random_info="$(prepare_dataset "impute_mode_random")"
IFS="|" read -r MODE_RANDOM_CSV MODE_RANDOM_DIR MODE_RANDOM_PARQUET <<< "${mode_random_info}"
MODE_RANDOM_LOG="${MODE_RANDOM_DIR}/analysis_log.jsonl"
start="$(log_count "${MODE_RANDOM_LOG}")"
run_ok "impute mode/random" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${MODE_RANDOM_PARQUET}" \
  --vars age,cat_var \
  --engine simple \
  --numeric-method mode \
  --categorical-method random \
  --indicator FALSE

check_log "${MODE_RANDOM_LOG}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true" \
  numeric_method="mode" \
  categorical_method="random" \
  method.age="mode" \
  method.cat_var="random"

random_suffix_info="$(prepare_dataset "impute_random_suffix")"
IFS="|" read -r RANDOM_CSV RANDOM_DIR RANDOM_PARQUET <<< "${random_suffix_info}"
RANDOM_LOG="${RANDOM_DIR}/analysis_log.jsonl"
start="$(log_count "${RANDOM_LOG}")"
run_ok "impute random suffix" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${RANDOM_PARQUET}" \
  --vars high_missing_var \
  --engine simple \
  --numeric-method random \
  --seed 321 \
  --suffix _rand \
  --indicator FALSE

check_log "${RANDOM_LOG}" "${start}" 1 "high_missing_var_rand" "simple" "false" "true" \
  numeric_method="random" \
  seed="321" \
  suffix="_rand" \
  method.high_missing_var="random"

constant_info="$(prepare_dataset "impute_constant_global")"
IFS="|" read -r CONST_CSV CONST_DIR CONST_PARQUET <<< "${constant_info}"
CONST_LOG="${CONST_DIR}/analysis_log.jsonl"
start="$(log_count "${CONST_LOG}")"
run_ok "impute constant" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${CONST_PARQUET}" \
  --vars age \
  --engine simple \
  --numeric-method constant \
  --constant 0 \
  --indicator FALSE

check_log "${CONST_LOG}" "${start}" 1 "age_imp" "simple" "false" "true" \
  numeric_method="constant" \
  constant="0" \
  method.age="constant"

value_map_info="$(prepare_dataset "impute_value_map")"
IFS="|" read -r VALUE_CSV VALUE_DIR VALUE_PARQUET <<< "${value_map_info}"
VALUE_LOG="${VALUE_DIR}/analysis_log.jsonl"
start="$(log_count "${VALUE_LOG}")"
run_ok "impute value map" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${VALUE_PARQUET}" \
  --vars age,cat_var \
  --engine simple \
  --numeric-method constant \
  --categorical-method constant \
  --value-map "age=999|cat_var=missing" \
  --indicator FALSE

check_log "${VALUE_LOG}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true" \
  numeric_method="constant" \
  categorical_method="constant" \
  method.age="constant" \
  method.cat_var="constant"

method_map_info="$(prepare_dataset "impute_method_map")"
IFS="|" read -r METHOD_CSV METHOD_DIR METHOD_PARQUET <<< "${method_map_info}"
METHOD_LOG="${METHOD_DIR}/analysis_log.jsonl"
start="$(log_count "${METHOD_LOG}")"
run_ok "impute method map" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${METHOD_PARQUET}" \
  --vars age,cat_var \
  --engine simple \
  --numeric-method mean \
  --categorical-method random \
  --method-map "age=median|cat_var=mode" \
  --indicator FALSE

check_log "${METHOD_LOG}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true" \
  numeric_method="mean" \
  categorical_method="random" \
  method.age="median" \
  method.cat_var="mode"

no_log_info="$(prepare_dataset "impute_no_log")"
IFS="|" read -r NO_LOG_CSV NO_LOG_DIR NO_LOG_PARQUET <<< "${no_log_info}"
NO_LOG_PATH="${NO_LOG_DIR}/analysis_log.jsonl"

start="$(log_count "${NO_LOG_PATH}")"
run_ok "impute log disabled" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${NO_LOG_PARQUET}" \
  --vars age,cat_var \
  --engine simple \
  --numeric-method median \
  --categorical-method mode \
  --log FALSE
end="$(log_count "${NO_LOG_PATH}")"
assert_log_unchanged "${start}" "${end}" "impute log disabled"

template_info="$(prepare_dataset "impute_template")"
IFS="|" read -r TEMPLATE_CSV TEMPLATE_DIR TEMPLATE_PARQUET <<< "${template_info}"
TEMPLATE_REPORT="${TEMPLATE_DIR}/report_canonical.md"

TEMPLATE_KEY="templates.impute.default"
TEMPLATE_ORIG="$(get_config_value "${TEMPLATE_KEY}")"
TEMPLATE_TMP="${TMP_BASE}/impute_template.md"
cp "$(resolve_template_source "${TEMPLATE_ORIG}")" "${TEMPLATE_TMP}"
sed -i 's/title: "Imputation Summary"/title: "Imputation Summary TEMPLATE TEST"/' "${TEMPLATE_TMP}"
sed -i 's/table_title: "Imputation summary\."/table_title: "Imputation summary TEST."/' "${TEMPLATE_TMP}"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_TMP}"
sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_TMP}"

set_config_value "${TEMPLATE_KEY}" "${TEMPLATE_TMP}"

run_ok "template override" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${TEMPLATE_PARQUET}" \
  --vars age,cat_var \
  --engine simple

assert_contains "${TEMPLATE_REPORT}" "Imputation Summary TEMPLATE TEST"
assert_contains "${TEMPLATE_REPORT}" "Imputation summary TEST."
assert_contains "${TEMPLATE_REPORT}" "*Note-TEST.*"
assert_contains "${TEMPLATE_REPORT}" "**Narrative TEST**"

set_config_value "${TEMPLATE_KEY}" "${TEMPLATE_ORIG}"

cli_template_info="$(prepare_dataset "impute_template_cli")"
IFS="|" read -r CLI_CSV CLI_DIR CLI_PARQUET <<< "${cli_template_info}"
CLI_REPORT="${CLI_DIR}/report_canonical.md"
CLI_LOG="${CLI_DIR}/analysis_log.jsonl"

cat >"${CLI_TEMPLATE}" <<'TEMPLATE'
---
tokens:
  title: "Impute CLI Template"
---
IMPUTE_CLI_TEMPLATE_MARKER

{{table_body}}
{{narrative_default}}
TEMPLATE

start="$(log_count "${CLI_LOG}")"
run_ok "template cli override" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${CLI_PARQUET}" \
  --vars age,cat_var \
  --engine simple \
  --template "${CLI_TEMPLATE}" \
  --user-prompt "impute cli prompt"

assert_contains "${CLI_REPORT}" "IMPUTE_CLI_TEMPLATE_MARKER"
check_log "${CLI_LOG}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true" \
  user_prompt="impute cli prompt"

RDS_LOG_PATH="$(dataset_log "${RDS_PATH}")"
start="$(log_count "${RDS_LOG_PATH}")"
run_ok "impute rds input" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --rds "${RDS_PATH}" \
  --vars age,cat_var \
  --engine simple \
  --numeric-method median \
  --categorical-method mode \
  --indicator FALSE
check_log "${RDS_LOG_PATH}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true"

RDATA_LOG_PATH="$(dataset_log "${RDATA_PATH}")"
start="$(log_count "${RDATA_LOG_PATH}")"
run_ok "impute rdata input" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --rdata "${RDATA_PATH}" \
  --df impute_rdata \
  --vars age,cat_var \
  --engine simple \
  --numeric-method median \
  --categorical-method mode \
  --indicator FALSE
check_log "${RDATA_LOG_PATH}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true"

CSV_SEMI_LOG_PATH="$(dataset_log "${CSV_SEMI_PATH}")"
start="$(log_count "${CSV_SEMI_LOG_PATH}")"
run_ok "impute csv semicolon input" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --csv "${CSV_SEMI_PATH}" \
  --sep ";" \
  --header TRUE \
  --vars age,cat_var \
  --engine simple \
  --numeric-method median \
  --categorical-method mode \
  --indicator FALSE
check_log "${CSV_SEMI_LOG_PATH}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  SAV_LOG_PATH="$(dataset_log "${SAV_PATH}")"
  start="$(log_count "${SAV_LOG_PATH}")"
  run_ok "impute sav input" Rscript "${R_SCRIPT_DIR}/impute.R" \
    --sav "${SAV_PATH}" \
    --vars age,cat_var \
    --engine simple \
    --numeric-method median \
    --categorical-method mode \
    --indicator FALSE
  check_log "${SAV_LOG_PATH}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true"
fi

printf "%s\n" \
  "csv" \
  "${DATA_GOLDEN}" \
  "," \
  "TRUE" \
  "age,cat_var" \
  "simple" \
  "median" \
  "mode" \
  "" \
  "" \
  "" \
  "_imp" \
  "FALSE" \
  "_miss" \
  "1" \
  "5" \
  "5" \
  "5" \
  "" \
  "2" \
  "" \
  "interactive prompt" \
  "TRUE" \
  > "${INTERACTIVE_INPUT}"

INTERACTIVE_LOG_PATH="$(dataset_log "${DATA_GOLDEN}")"
start="$(log_count "${INTERACTIVE_LOG_PATH}")"
run_ok "impute interactive" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/impute.R" --interactive
check_log "${INTERACTIVE_LOG_PATH}" "${start}" 2 "age_imp,cat_var_imp" "simple" "false" "true" \
  user_prompt="interactive prompt"

invalid_info="$(prepare_dataset "impute_invalid")"
IFS="|" read -r INVALID_CSV INVALID_DIR INVALID_PARQUET <<< "${invalid_info}"
INVALID_LOG="${INVALID_DIR}/analysis_log.jsonl"

run_expect_fail "impute missing constant" "${INVALID_LOG}" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${INVALID_PARQUET}" \
  --vars age \
  --engine simple \
  --numeric-method constant

invalid_engine_info="$(prepare_dataset "impute_invalid_engine")"
IFS="|" read -r INVALID_ENGINE_CSV INVALID_ENGINE_DIR INVALID_ENGINE_PARQUET <<< "${invalid_engine_info}"
INVALID_ENGINE_LOG="${INVALID_ENGINE_DIR}/analysis_log.jsonl"
run_expect_fail "impute invalid engine" "${INVALID_ENGINE_LOG}" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${INVALID_ENGINE_PARQUET}" \
  --vars age \
  --engine invalid_engine

invalid_method_info="$(prepare_dataset "impute_invalid_method")"
IFS="|" read -r INVALID_METHOD_CSV INVALID_METHOD_DIR INVALID_METHOD_PARQUET <<< "${invalid_method_info}"
INVALID_METHOD_LOG="${INVALID_METHOD_DIR}/analysis_log.jsonl"
run_expect_fail "impute invalid categorical method" "${INVALID_METHOD_LOG}" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${INVALID_METHOD_PARQUET}" \
  --vars cat_var \
  --engine simple \
  --categorical-method mean

invalid_map_info="$(prepare_dataset "impute_invalid_map")"
IFS="|" read -r INVALID_MAP_CSV INVALID_MAP_DIR INVALID_MAP_PARQUET <<< "${invalid_map_info}"
INVALID_MAP_LOG="${INVALID_MAP_DIR}/analysis_log.jsonl"
run_expect_fail "impute invalid method map" "${INVALID_MAP_LOG}" Rscript "${R_SCRIPT_DIR}/impute.R" \
  --parquet "${INVALID_MAP_PARQUET}" \
  --vars age \
  --engine simple \
  --method-map "missing_var=mean"

if [ "${HAS_MICE}" -eq 1 ]; then
  mice_info="$(prepare_dataset "impute_mice")"
  IFS="|" read -r MICE_CSV MICE_DIR MICE_PARQUET <<< "${mice_info}"
  MICE_LOG="${MICE_DIR}/analysis_log.jsonl"
  start="$(log_count "${MICE_LOG}")"
  run_ok "impute mice" Rscript "${R_SCRIPT_DIR}/impute.R" \
    --parquet "${MICE_PARQUET}" \
    --vars age,income,cat_var \
    --engine mice \
    --m 1 \
    --maxit 2 \
    --seed 123
  check_log "${MICE_LOG}" "${start}" 3 "age_imp,income_imp,cat_var_imp" "mice" "false" "true" \
    m="2" \
    maxit="2" \
    seed="123"
else
  echo "[SKIP] mice not installed." | tee -a "${LOG_FILE}"
fi

if [ "${HAS_VIM}" -eq 1 ]; then
  knn_info="$(prepare_dataset "impute_knn")"
  IFS="|" read -r KNN_CSV KNN_DIR KNN_PARQUET <<< "${knn_info}"
  KNN_LOG="${KNN_DIR}/analysis_log.jsonl"
  start="$(log_count "${KNN_LOG}")"
  run_ok "impute knn" Rscript "${R_SCRIPT_DIR}/impute.R" \
    --parquet "${KNN_PARQUET}" \
    --vars age,income,cat_var \
    --engine knn \
    --k 3
  check_log "${KNN_LOG}" "${start}" 3 "age_imp,income_imp,cat_var_imp" "knn" "false" "true" \
    k="3"
else
  echo "[SKIP] VIM not installed." | tee -a "${LOG_FILE}"
fi

echo "[DONE] impute deliberate tests finished" | tee -a "${LOG_FILE}"
