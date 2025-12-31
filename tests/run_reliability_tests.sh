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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_reliability_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/check_r_package.R"
MIXED_MODELS_PREP_SCRIPT="${ROOT_DIR}/tests/mixed_models_prep.R"

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

WORKSPACE_DIR="${RUN_ROOT}/reliability_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/reliability"
LOG_FILE="${RUN_ROOT}/reliability_test.log"

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
  echo "[FAIL] arrow not installed; reliability tests require parquet support." | tee -a "${LOG_FILE}"
  exit 1
fi

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

check_reliability_log() {
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

run_expect_failure() {
  local label="$1"; shift
  local log_path="$1"; shift
  local expected_status="$1"; shift
  local start_count
  start_count=$(log_count "${log_path}")
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} expected failure" | tee -a "${LOG_FILE}"
    exit 1
  fi
  check_reliability_log "${log_path}" "${start_count}" status="${expected_status}"
}

cd "${WORKSPACE_DIR}"

Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}" >>"${LOG_FILE}" 2>&1

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"

if [[ ! -f "${PARQUET_GOLDEN}" ]]; then
  echo "[FAIL] missing parquet: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score,mid_score,post_score --icc-model twoway-random --icc-type agreement --icc-unit single --missing pairwise >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=icc min_rows=1 vars=pre_score,mid_score,post_score icc_model=twoway-random icc_type=agreement icc_unit=single missing=complete

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score,mid_score,post_score --icc-model oneway --icc-type consistency --icc-unit average --conf-level 0.9 >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=icc min_rows=1 icc_model=oneway icc_type=consistency icc_unit=average conf_level=0.9

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score,mid_score,post_score --icc-model twoway-random --icc-type consistency --icc-unit single >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=icc min_rows=1 icc_model=twoway-random icc_type=consistency icc_unit=single

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score,mid_score,post_score --icc-model twoway-mixed --icc-type agreement --icc-unit average >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=icc min_rows=1 icc_model=twoway-mixed icc_type=consistency icc_unit=average

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis test_retest --vars pre_score,post_score --method spearman --conf-level 0.9 >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=test_retest min_rows=1 method=spearman conf_level=0.9

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis kappa --vars cat_var,cat_var2 --kappa-weight none >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=kappa min_rows=1 kappa_weight=none

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis kappa --vars cat_var,cat_var2 --kappa-weight none --group group3 >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=kappa min_rows=2 min_groups=2 group=group3

LONG_DATA_PATH="${TMP_BASE}/reliability_long.csv"
Rscript "${MIXED_MODELS_PREP_SCRIPT}" "${DATA_GOLDEN}" "${LONG_DATA_PATH}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${LONG_DATA_PATH}" >>"${LOG_FILE}" 2>&1
LONG_LABEL="reliability_long"
LONG_DIR="${WORKSPACE_DIR}/${LONG_LABEL}"
LONG_PARQUET="${LONG_DIR}/${LONG_LABEL}.parquet"
LONG_LOG_PATH="${LONG_DIR}/analysis_log.jsonl"

start_count=$(log_count "${LONG_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${LONG_PARQUET}" --analysis icc --format long --id id --rater time --score score --group group3 >>"${LOG_FILE}" 2>&1
check_reliability_log "${LONG_LOG_PATH}" "${start_count}" analysis=icc min_rows=1 min_groups=2 format=long id=id rater=time score=score group=group3

start_count=$(log_count "${LONG_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${LONG_PARQUET}" --analysis icc --format long --id id --rater time --expect-invalid TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${LONG_LOG_PATH}" "${start_count}" status=expected_invalid_input

ORDINAL_DATA="${TMP_BASE}/ordinal_kappa.csv"
ORDINAL_SCRIPT="${TMP_BASE}/ordinal_kappa_prep.R"
cat <<'RSCRIPT' > "${ORDINAL_SCRIPT}"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: ordinal_kappa_prep.R <input_csv> <output_csv>")
}
input_path <- args[1]
output_path <- args[2]
df <- read.csv(input_path, stringsAsFactors = FALSE)
df$ordinal_var2 <- df$ordinal_var
max_val <- max(df$ordinal_var2, na.rm = TRUE)
df$ordinal_var2[df$ordinal_var2 == max_val] <- df$ordinal_var2[df$ordinal_var2 == max_val] - 1
write.csv(df, output_path, row.names = FALSE)
RSCRIPT
Rscript "${ORDINAL_SCRIPT}" "${DATA_GOLDEN}" "${ORDINAL_DATA}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${ORDINAL_DATA}" >>"${LOG_FILE}" 2>&1
ORDINAL_LABEL="ordinal_kappa"
ORDINAL_DIR="${WORKSPACE_DIR}/${ORDINAL_LABEL}"
ORDINAL_PARQUET="${ORDINAL_DIR}/${ORDINAL_LABEL}.parquet"
ORDINAL_LOG_PATH="${ORDINAL_DIR}/analysis_log.jsonl"

start_count=$(log_count "${ORDINAL_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${ORDINAL_PARQUET}" --analysis kappa --vars ordinal_var,ordinal_var2 --kappa-weight quadratic >>"${LOG_FILE}" 2>&1
check_reliability_log "${ORDINAL_LOG_PATH}" "${start_count}" analysis=kappa min_rows=1 kappa_weight=quadratic

start_count=$(log_count "${ORDINAL_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${ORDINAL_PARQUET}" --analysis kappa --vars ordinal_var,ordinal_var2 --kappa-weight linear >>"${LOG_FILE}" 2>&1
check_reliability_log "${ORDINAL_LOG_PATH}" "${start_count}" analysis=kappa min_rows=1 kappa_weight=linear

KAPPA_CONST_DATA="${TMP_BASE}/kappa_constant.csv"
KAPPA_CONST_SCRIPT="${TMP_BASE}/kappa_constant_prep.R"
cat <<'RSCRIPT' > "${KAPPA_CONST_SCRIPT}"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: kappa_constant_prep.R <input_csv> <output_csv>")
}
input_path <- args[1]
output_path <- args[2]
df <- read.csv(input_path, stringsAsFactors = FALSE)
df$kappa_const1 <- "A"
df$kappa_const2 <- "A"
write.csv(df, output_path, row.names = FALSE)
RSCRIPT
Rscript "${KAPPA_CONST_SCRIPT}" "${DATA_GOLDEN}" "${KAPPA_CONST_DATA}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${KAPPA_CONST_DATA}" >>"${LOG_FILE}" 2>&1
KAPPA_CONST_LABEL="kappa_constant"
KAPPA_CONST_DIR="${WORKSPACE_DIR}/${KAPPA_CONST_LABEL}"
KAPPA_CONST_PARQUET="${KAPPA_CONST_DIR}/${KAPPA_CONST_LABEL}.parquet"
KAPPA_CONST_LOG_PATH="${KAPPA_CONST_DIR}/analysis_log.jsonl"

start_count=$(log_count "${KAPPA_CONST_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${KAPPA_CONST_PARQUET}" --analysis kappa --vars kappa_const1,kappa_const2 --kappa-weight none >>"${LOG_FILE}" 2>&1
check_reliability_log "${KAPPA_CONST_LOG_PATH}" "${start_count}" analysis=kappa min_rows=1 kappa_weight=none

COERCE_DATA="${TMP_BASE}/retest_coerce.csv"
COERCE_SCRIPT="${TMP_BASE}/retest_coerce_prep.R"
cat <<'RSCRIPT' > "${COERCE_SCRIPT}"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: retest_coerce_prep.R <input_csv> <output_csv>")
}
input_path <- args[1]
output_path <- args[2]
df <- read.csv(input_path, stringsAsFactors = FALSE)
df$pre_score <- as.character(df$pre_score)
df$post_score <- as.character(df$post_score)
df$pre_score[df$id %% 17 == 0] <- "bad"
df$post_score[df$id %% 19 == 0] <- "bad"
df$pre_score[df$id %% 10 == 0] <- NA
df$post_score[df$id %% 15 == 0] <- NA
write.csv(df, output_path, row.names = FALSE)
RSCRIPT
Rscript "${COERCE_SCRIPT}" "${DATA_GOLDEN}" "${COERCE_DATA}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${COERCE_DATA}" >>"${LOG_FILE}" 2>&1
COERCE_LABEL="retest_coerce"
COERCE_DIR="${WORKSPACE_DIR}/${COERCE_LABEL}"
COERCE_PARQUET="${COERCE_DIR}/${COERCE_LABEL}.parquet"
COERCE_LOG_PATH="${COERCE_DIR}/analysis_log.jsonl"

start_count=$(log_count "${COERCE_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${COERCE_PARQUET}" --analysis test_retest --vars pre_score,post_score --method pearson --missing pairwise --coerce TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${COERCE_LOG_PATH}" "${start_count}" analysis=test_retest min_rows=1 method=pearson missing=pairwise coerce=true

start_count=$(log_count "${COERCE_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${COERCE_PARQUET}" --analysis test_retest --vars pre_score,post_score --coerce FALSE --expect-invalid TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${COERCE_LOG_PATH}" "${start_count}" status=expected_invalid_input

RETEST_LONG_DATA="${TMP_BASE}/retest_long.csv"
RETEST_LONG_SCRIPT="${TMP_BASE}/retest_long_prep.R"
cat <<'RSCRIPT' > "${RETEST_LONG_SCRIPT}"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: retest_long_prep.R <input_csv> <output_csv>")
}
input_path <- args[1]
output_path <- args[2]
df <- read.csv(input_path, stringsAsFactors = FALSE)
long_df <- reshape(
  df,
  varying = c("pre_score", "post_score"),
  v.names = "score",
  timevar = "time",
  times = c("pre", "post"),
  idvar = "id",
  direction = "long"
)
long_df$time <- factor(long_df$time, levels = c("pre", "post"))
long_df$group3 <- factor(long_df$group3)
long_df <- long_df[!is.na(long_df$score), ]
output <- long_df[, c("id", "time", "score", "group3")]
write.csv(output, output_path, row.names = FALSE)
RSCRIPT
Rscript "${RETEST_LONG_SCRIPT}" "${DATA_GOLDEN}" "${RETEST_LONG_DATA}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${RETEST_LONG_DATA}" >>"${LOG_FILE}" 2>&1
RETEST_LONG_LABEL="retest_long"
RETEST_LONG_DIR="${WORKSPACE_DIR}/${RETEST_LONG_LABEL}"
RETEST_LONG_PARQUET="${RETEST_LONG_DIR}/${RETEST_LONG_LABEL}.parquet"
RETEST_LONG_LOG_PATH="${RETEST_LONG_DIR}/analysis_log.jsonl"

start_count=$(log_count "${RETEST_LONG_LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${RETEST_LONG_PARQUET}" --analysis test_retest --format long --id id --rater time --score score --group group3 --method pearson >>"${LOG_FILE}" 2>&1
check_reliability_log "${RETEST_LONG_LOG_PATH}" "${start_count}" analysis=test_retest min_rows=1 min_groups=2 format=long id=id rater=time score=score group=group3 method=pearson

TEMPLATE_PATH="${ROOT_DIR}/assets/reliability/default-template.md"
TEMPLATE_TMP="${TMP_BASE}/reliability-template-override.md"
TEMPLATE_MARKER="RELIABILITY_TEMPLATE_OVERRIDE"
if [[ ! -f "${TEMPLATE_PATH}" ]]; then
  echo "[FAIL] missing template: ${TEMPLATE_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
cp "${TEMPLATE_PATH}" "${TEMPLATE_TMP}"
printf "\n\n%s\n" "${TEMPLATE_MARKER}" >> "${TEMPLATE_TMP}"

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score,mid_score,post_score --template "${TEMPLATE_TMP}" >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" analysis=icc min_rows=1
assert_contains "${NLSS_REPORT_PATH}" "${TEMPLATE_MARKER}"

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis kappa --vars cat_var,cat_var2,ordinal_var --expect-invalid TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" status=expected_invalid_input

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis test_retest --vars pre_score,mid_score,post_score --expect-invalid TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" status=expected_invalid_input

start_count=$(log_count "${LOG_PATH}")
Rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_GOLDEN}" --analysis icc --vars pre_score --expect-invalid TRUE >>"${LOG_FILE}" 2>&1
check_reliability_log "${LOG_PATH}" "${start_count}" status=expected_invalid_input

echo "Reliability tests completed." | tee -a "${LOG_FILE}"
