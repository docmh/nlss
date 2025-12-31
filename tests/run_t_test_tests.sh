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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_t_test_log.py"
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
  DATA_DIR_CFG="./tests/data"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_MAIN="${DATA_DIR}/t_test_main.csv"
DATA_SMALL="${DATA_DIR}/t_test_small.csv"

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

WORKSPACE_DIR="${RUN_ROOT}/t_test_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/t_test"
LOG_FILE="${RUN_ROOT}/t_test_test.log"
HELP_PATH="${TMP_BASE}/t_test_help.txt"
INTERACTIVE_INPUT="${TMP_BASE}/t_test_interactive_input.txt"
RDS_PATH="${TMP_BASE}/t_test_input_rds.rds"
RDATA_PATH="${TMP_BASE}/t_test_input_rdata.RData"
RDATA_BAD_PATH="${TMP_BASE}/t_test_bad_rdata.RData"
SAV_PATH="${TMP_BASE}/t_test_input_sav.sav"
CSV_SEMI_PATH="${TMP_BASE}/t_test_semicolon.csv"
CSV_NOHEADER_PATH="${TMP_BASE}/t_test_noheader.csv"
CSV_ONEGROUP_PATH="${TMP_BASE}/t_test_one_group.csv"
CSV_GROUP_MISSING_PATH="${TMP_BASE}/t_test_group_missing.csv"
CSV_SMALL_GROUPS_PATH="${TMP_BASE}/t_test_small_groups.csv"
CSV_PAIRED_TINY_PATH="${TMP_BASE}/t_test_paired_tiny.csv"
CSV_INTERACTIVE_PATH="${TMP_BASE}/t_test_interactive.csv"
CSV_LOG_OFF_PATH="${TMP_BASE}/t_test_log_off.csv"
CSV_USER_PROMPT_PATH="${TMP_BASE}/t_test_user_prompt.csv"
CSV_NO_NUMERIC_PATH="${TMP_BASE}/t_test_no_numeric.csv"

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

if [ ! -f "${DATA_MAIN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_MAIN}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_SMALL}" ]; then
  echo "[FAIL] missing dataset: ${DATA_SMALL}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${TMP_BASE}"
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
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "${start_count}" "$@"
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
  local log_path="$1"; shift
  local mode="$1"; shift
  local check_args=()
  while [ "$#" -gt 0 ]; do
    if [ "$1" = "--" ]; then
      shift
      break
    fi
    check_args+=("$1")
    shift
  done
  if [ "$#" -eq 0 ]; then
    echo "[FAIL] ${label} (missing command)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  echo "[RUN-EXPECT-INVALID] ${label}" | tee -a "${LOG_FILE}"
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
  if check_log "${log_path}" "${start_count}" "${status}" "${mode}" "${check_args[@]}"; then
    echo "[PASS] ${label} (status ${status})" | tee -a "${LOG_FILE}"
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

require_file() {
  local path="$1"
  if [ ! -f "${path}" ]; then
    echo "[FAIL] missing file: ${path}" | tee -a "${LOG_FILE}"
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

dataset_label() {
  local path="$1"
  local base
  base="$(basename "${path}")"
  echo "${base%.*}"
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

dataset_parquet() {
  local path="$1"
  local label
  label="$(dataset_label "${path}")"
  echo "$(dataset_dir "${path}")/${label}.parquet"
}

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi

run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); ttest_rdata <- df; save(ttest_rdata, file = \"${RDATA_PATH}\")"
run_ok "prepare bad rdata" Rscript -e "bad_obj <- 1:5; save(bad_obj, file = \"${RDATA_BAD_PATH}\")"
run_ok "prepare semicolon csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write.table(df[, c('score', 'score2')], \"${CSV_SEMI_PATH}\", sep = ';', row.names = FALSE, quote = FALSE)"
run_ok "prepare no-header csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write.table(df[, c('score', 'score2')], \"${CSV_NOHEADER_PATH}\", sep = ';', row.names = FALSE, col.names = FALSE, quote = FALSE)"
run_ok "prepare one-group csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); df\$group <- 'A'; write.csv(df, \"${CSV_ONEGROUP_PATH}\", row.names = FALSE)"
run_ok "prepare missing-group csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); df\$group <- NA; write.csv(df, \"${CSV_GROUP_MISSING_PATH}\", row.names = FALSE)"
run_ok "prepare small-groups csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); df <- df[!is.na(df\$group), ]; g1 <- df[df\$group == 'A', ]; g2 <- df[df\$group == 'B', ]; if (nrow(g1) < 2 || nrow(g2) < 1) stop('Not enough rows for small-group dataset.'); df_small <- rbind(g1[1:2, ], g2[1, ]); write.csv(df_small, \"${CSV_SMALL_GROUPS_PATH}\", row.names = FALSE)"
run_ok "prepare paired tiny csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); df <- df[!is.na(df\$pre) & !is.na(df\$post), c('pre', 'post')]; if (nrow(df) < 1) stop('No complete pairs for paired tiny dataset.'); write.csv(df[1, ], \"${CSV_PAIRED_TINY_PATH}\", row.names = FALSE)"
run_ok "prepare no-numeric csv" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); df <- df[, c('group', 'group3', 'nonnumeric')]; write.csv(df, \"${CSV_NO_NUMERIC_PATH}\", row.names = FALSE)"
run_ok "copy interactive csv" cp "${DATA_MAIN}" "${CSV_INTERACTIVE_PATH}"
run_ok "copy log-off csv" cp "${DATA_MAIN}" "${CSV_LOG_OFF_PATH}"
run_ok "copy user-prompt csv" cp "${DATA_MAIN}" "${CSV_USER_PROMPT_PATH}"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${INTERACTIVE_INPUT}" <<PROMPT
csv
${CSV_INTERACTIVE_PATH}
,
TRUE
one-sample
score
0
two.sided
0.95
FALSE
FALSE
1000

2

TRUE
PROMPT

MAIN_LOG_PATH="$(dataset_log "${DATA_MAIN}")"
MAIN_NLSS_PATH="$(dataset_nlss "${DATA_MAIN}")"
MAIN_PARQUET_PATH="$(dataset_parquet "${DATA_MAIN}")"
SMALL_LOG_PATH="$(dataset_log "${DATA_SMALL}")"
SMALL_NLSS_PATH="$(dataset_nlss "${DATA_SMALL}")"
INTERACTIVE_LOG_PATH="$(dataset_log "${CSV_INTERACTIVE_PATH}")"
RDS_LOG_PATH="$(dataset_log "${RDS_PATH}")"
CSV_SEMI_LOG_PATH="$(dataset_log "${CSV_SEMI_PATH}")"
CSV_NOHEADER_LOG_PATH="$(dataset_log "${CSV_NOHEADER_PATH}")"
CSV_ONEGROUP_LOG_PATH="$(dataset_log "${CSV_ONEGROUP_PATH}")"
CSV_GROUP_MISSING_LOG_PATH="$(dataset_log "${CSV_GROUP_MISSING_PATH}")"
CSV_SMALL_GROUPS_LOG_PATH="$(dataset_log "${CSV_SMALL_GROUPS_PATH}")"
CSV_PAIRED_TINY_LOG_PATH="$(dataset_log "${CSV_PAIRED_TINY_PATH}")"
CSV_PAIRED_TINY_NLSS_PATH="$(dataset_nlss "${CSV_PAIRED_TINY_PATH}")"
CSV_SMALL_GROUPS_NLSS_PATH="$(dataset_nlss "${CSV_SMALL_GROUPS_PATH}")"
CSV_LOG_OFF_NLSS_PATH="$(dataset_nlss "${CSV_LOG_OFF_PATH}")"
CSV_LOG_OFF_LOG_PATH="$(dataset_log "${CSV_LOG_OFF_PATH}")"
CSV_USER_PROMPT_LOG_PATH="$(dataset_log "${CSV_USER_PROMPT_PATH}")"
CSV_USER_PROMPT_NLSS_PATH="$(dataset_nlss "${CSV_USER_PROMPT_PATH}")"
RDATA_LABEL="ttest_rdata"
RDATA_DIR="${WORKSPACE_DIR}/${RDATA_LABEL}"
RDATA_LOG_PATH="${RDATA_DIR}/analysis_log.jsonl"
RDATA_NLSS_PATH="${RDATA_DIR}/report_canonical.md"
SAV_LOG_PATH="$(dataset_log "${SAV_PATH}")"

run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/t_test.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "t-tests (base R)"
assert_contains "${HELP_PATH}" "Options:"

start=$(log_count "${INTERACTIVE_LOG_PATH}")
run_ok "interactive one-sample" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/t_test.R" --interactive
check_log "${INTERACTIVE_LOG_PATH}" "${start}" "-" "one_sample" "alternative=two.sided" "conf_level=0.95" "bootstrap=false"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "one-sample default" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample" "alternative=two.sided" "conf_level=0.95" "bootstrap=false" "digits=2" "mu=0"
require_file "${MAIN_PARQUET_PATH}"
require_file "${MAIN_NLSS_PATH}"
require_file "${MAIN_LOG_PATH}"
assert_contains "${MAIN_NLSS_PATH}" "t Test Results"
assert_contains "${MAIN_NLSS_PATH}" "## Narrative"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "one-sample greater mu" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --mu 0.3 --alternative greater
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample" "alternative=greater" "mu=0.3"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "one-sample less (conf-level/digits)" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --alternative less --conf-level 0.9 --digits 3
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample" "alternative=less" "conf_level=0.9" "digits=3"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "one-sample bootstrap" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score2 --bootstrap TRUE --bootstrap-samples 1000 --seed 123
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample" "bootstrap=true" "bootstrap_samples=1000"
assert_contains "${MAIN_NLSS_PATH}" "Bootstrap CIs use 1000 resamples."

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "independent welch" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --group group
check_log "${MAIN_LOG_PATH}" "${start}" "-" "independent" "var_equal=false"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "independent equal var" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score2 --group group --var-equal TRUE
check_log "${MAIN_LOG_PATH}" "${start}" "-" "independent" "var_equal=true"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "independent bootstrap" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --group group --bootstrap TRUE --bootstrap-samples 200 --seed 42
check_log "${MAIN_LOG_PATH}" "${start}" "-" "independent" "bootstrap=true" "bootstrap_samples=200"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "paired two pairs" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre,score --y post,score2
check_log "${MAIN_LOG_PATH}" "${start}" "-" "paired"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "paired bootstrap" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre --y post --bootstrap TRUE --bootstrap-samples 150 --seed 7
check_log "${MAIN_LOG_PATH}" "${start}" "-" "paired" "bootstrap=true" "bootstrap_samples=150"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "one-sample default vars" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}"
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "independent default vars" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --group group
check_log "${MAIN_LOG_PATH}" "${start}" "-" "independent"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "independent vars include group" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score,group --group group
check_log "${MAIN_LOG_PATH}" "${start}" "-" "independent"

start=$(log_count "${SMALL_LOG_PATH}")
run_ok "one-sample tiny n" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_SMALL}" --vars score
check_log "${SMALL_LOG_PATH}" "${start}" "-" "one_sample"
assert_contains "${SMALL_NLSS_PATH}" "could not be computed"

start=$(log_count "${CSV_SMALL_GROUPS_LOG_PATH}")
run_ok "independent tiny group n" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_SMALL_GROUPS_PATH}" --vars score --group group
check_log "${CSV_SMALL_GROUPS_LOG_PATH}" "${start}" "-" "independent"
assert_contains "${CSV_SMALL_GROUPS_NLSS_PATH}" "could not be computed"

start=$(log_count "${CSV_PAIRED_TINY_LOG_PATH}")
run_ok "paired tiny n" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_PAIRED_TINY_PATH}" --x pre --y post
check_log "${CSV_PAIRED_TINY_LOG_PATH}" "${start}" "-" "paired"
assert_contains "${CSV_PAIRED_TINY_NLSS_PATH}" "could not be computed"

start=$(log_count "${CSV_SEMI_LOG_PATH}")
run_ok "csv semicolon separator" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_SEMI_PATH}" --sep ";" --header TRUE --vars score
check_log "${CSV_SEMI_LOG_PATH}" "${start}" "-" "one_sample"

start=$(log_count "${CSV_NOHEADER_LOG_PATH}")
run_ok "csv no header" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_NOHEADER_PATH}" --sep ";" --header FALSE --vars V1
check_log "${CSV_NOHEADER_LOG_PATH}" "${start}" "-" "one_sample"

start=$(log_count "${RDS_LOG_PATH}")
run_ok "rds input" Rscript "${R_SCRIPT_DIR}/t_test.R" --rds "${RDS_PATH}" --vars score
check_log "${RDS_LOG_PATH}" "${start}" "-" "one_sample"

start=$(log_count "${RDATA_LOG_PATH}")
run_ok "rdata input" Rscript "${R_SCRIPT_DIR}/t_test.R" --rdata "${RDATA_PATH}" --df ttest_rdata --vars score
check_log "${RDATA_LOG_PATH}" "${start}" "-" "one_sample"
require_file "${RDATA_NLSS_PATH}"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "parquet input" Rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${MAIN_PARQUET_PATH}" --vars score
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${SAV_LOG_PATH}")
  run_ok "sav input" Rscript "${R_SCRIPT_DIR}/t_test.R" --sav "${SAV_PATH}" --vars score
  check_log "${SAV_LOG_PATH}" "${start}" "-" "one_sample"
fi

before_log=$(log_count "${CSV_LOG_OFF_LOG_PATH}")
run_ok "log disabled" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_LOG_OFF_PATH}" --vars score --log FALSE
require_file "${CSV_LOG_OFF_NLSS_PATH}"
after_log=$(log_count "${CSV_LOG_OFF_LOG_PATH}")
assert_log_unchanged "${before_log}" "${after_log}" "log disabled"

start=$(log_count "${CSV_USER_PROMPT_LOG_PATH}")
run_ok "user prompt logging" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_USER_PROMPT_PATH}" --vars score --user-prompt t_test_prompt
check_log "${CSV_USER_PROMPT_LOG_PATH}" "${start}" "-" "one_sample" "user_prompt=t_test_prompt"
require_file "${CSV_USER_PROMPT_NLSS_PATH}"

TEMPLATE_ORIG="$(get_config_value templates.t_test.default)"
TEMPLATE_TMP="${TMP_BASE}/t_test_default_template.md"
cp "$(resolve_template_source "${TEMPLATE_ORIG}")" "${TEMPLATE_TMP}"
printf "\n\nT_TEST_TEMPLATE_DEFAULT\n" >>"${TEMPLATE_TMP}"
set_config_value templates.t_test.default "${TEMPLATE_TMP}"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "template override default" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score
check_log "${MAIN_LOG_PATH}" "${start}" "-" "one_sample"
assert_contains "${MAIN_NLSS_PATH}" "T_TEST_TEMPLATE_DEFAULT"

set_config_value templates.t_test.default "${TEMPLATE_ORIG}"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "group levels expected negative" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --group group3 --expect-two-groups TRUE
check_log "${MAIN_LOG_PATH}" "${start}" "expected_invalid_input" "independent" "expect_two_groups=true"

run_expect_invalid "independent with 3 levels" "invalid_input" "${MAIN_LOG_PATH}" "independent" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --group group3

run_expect_invalid "independent with 1 level" "invalid_input" "${CSV_ONEGROUP_LOG_PATH}" "independent" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_ONEGROUP_PATH}" --vars score --group group

run_expect_invalid "independent with missing group" "invalid_input" "${CSV_GROUP_MISSING_LOG_PATH}" "independent" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_GROUP_MISSING_PATH}" --vars score --group group

run_expect_fail "independent missing group variable" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars score --group missing_group

run_expect_invalid "paired with group" "invalid_input" "${MAIN_LOG_PATH}" "paired" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre --y post --group group

run_expect_invalid "paired missing y" "invalid_input" "${MAIN_LOG_PATH}" "paired" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre

run_expect_invalid "paired missing x" "invalid_input" "${MAIN_LOG_PATH}" "paired" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --y post

run_expect_invalid "paired mismatch lists" "invalid_input" "${MAIN_LOG_PATH}" "paired" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre,score --y post

run_expect_invalid "paired unknown variable" "invalid_input" "${MAIN_LOG_PATH}" "paired" -- \
  Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre --y missing

run_expect_fail "paired non-numeric variable" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --x pre --y nonnumeric
run_expect_fail "non-numeric variable" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars nonnumeric
run_expect_fail "unknown variable" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${DATA_MAIN}" --vars unknown_var
run_expect_fail "rdata missing df" Rscript "${R_SCRIPT_DIR}/t_test.R" --rdata "${RDATA_PATH}" --vars score
run_expect_fail "rdata non-dataframe object" Rscript "${R_SCRIPT_DIR}/t_test.R" --rdata "${RDATA_BAD_PATH}" --df bad_obj --vars score
run_expect_fail "no numeric variables" Rscript "${R_SCRIPT_DIR}/t_test.R" --csv "${CSV_NO_NUMERIC_PATH}"

echo "t_test tests: OK" | tee -a "${LOG_FILE}"
