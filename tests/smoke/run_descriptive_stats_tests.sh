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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_descriptive_stats_log.py"

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

RUNS_BASE_CFG="$(get_tests_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"

RUN_ID="$(date +%Y%m%d%H%M%S)"
if [ -n "${NLSS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${NLSS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
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

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="nlss-workspace.yml"
fi

WORKSPACE_DIR="${RUN_ROOT}/descriptive_stats_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
LOG_FILE="${RUN_ROOT}/descriptive_stats_test.log"
TMP_BASE="${RUN_ROOT}/tmp/descriptive_stats"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"

EDGE_CSV="${TMP_BASE}/descriptive_edge.csv"
EDGE_SEMI_CSV="${TMP_BASE}/descriptive_edge_semicolon.csv"
EDGE_NOHEADER_CSV="${TMP_BASE}/descriptive_edge_no_header.csv"
EDGE_RDS_PATH="${TMP_BASE}/descriptive_edge.rds"
EDGE_RDATA_PATH="${TMP_BASE}/descriptive_edge.RData"
EDGE_RDATA_DF="descriptive_edge"
EDGE_SAV_PATH="${TMP_BASE}/descriptive_edge.sav"
EDGE_LABEL="$(basename "${EDGE_CSV}")"
EDGE_LABEL="${EDGE_LABEL%.*}"
EDGE_DIR="${WORKSPACE_DIR}/${EDGE_LABEL}"
EDGE_LOG_PATH="${EDGE_DIR}/analysis_log.jsonl"
EDGE_REPORT_PATH="${EDGE_DIR}/report_canonical.md"

EDGE_SEMI_LABEL="$(basename "${EDGE_SEMI_CSV}")"
EDGE_SEMI_LABEL="${EDGE_SEMI_LABEL%.*}"
EDGE_SEMI_LOG_PATH="${WORKSPACE_DIR}/${EDGE_SEMI_LABEL}/analysis_log.jsonl"

EDGE_NOHEADER_LABEL="$(basename "${EDGE_NOHEADER_CSV}")"
EDGE_NOHEADER_LABEL="${EDGE_NOHEADER_LABEL%.*}"
EDGE_NOHEADER_LOG_PATH="${WORKSPACE_DIR}/${EDGE_NOHEADER_LABEL}/analysis_log.jsonl"

NONNUMERIC_CSV="${TMP_BASE}/descriptive_non_numeric.csv"
NONNUMERIC_LABEL="$(basename "${NONNUMERIC_CSV}")"
NONNUMERIC_LABEL="${NONNUMERIC_LABEL%.*}"
NONNUMERIC_LOG_PATH="${WORKSPACE_DIR}/${NONNUMERIC_LABEL}/analysis_log.jsonl"

INTERACTIVE_INPUT="${TMP_BASE}/descriptive_edge_interactive.txt"
TEMPLATE_OVERRIDE_PATH="${TMP_BASE}/descriptive_stats_template_override.md"
TEMPLATE_OVERRIDE_MARKER="TEMPLATE_OVERRIDE_PATH"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

log() {
  echo "$@" | tee -a "${LOG_FILE}"
}

run_ok() {
  local label="$1"; shift
  if "$@" >>"${LOG_FILE}" 2>&1; then
    log "[PASS] ${label}"
  else
    log "[FAIL] ${label}"
    exit 1
  fi
}

run_expect_fail() {
  local label="$1"; shift
  if "$@" >>"${LOG_FILE}" 2>&1; then
    log "[FAIL] ${label} (unexpected success)"
    exit 1
  else
    log "[PASS] ${label}"
  fi
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
  if [ "${before}" -ne "${after}" ]; then
    log "[FAIL] ${label} (log changed)"
    exit 1
  fi
  log "[PASS] ${label}"
}

assert_contains() {
  local label="$1"
  local file="$2"
  local needle="$3"
  if "${PYTHON_BIN}" - "$file" "$needle" <<'PY'
import sys
from pathlib import Path
path = Path(sys.argv[1])
needle = sys.argv[2]
if not path.exists():
    sys.exit(1)
text = path.read_text(encoding="utf-8")
if needle not in text:
    sys.exit(1)
PY
  then
    log "[PASS] ${label}"
  else
    log "[FAIL] ${label}"
    exit 1
  fi
}

if ! command -v Rscript >/dev/null 2>&1; then
  log "[FAIL] Rscript not found. Install R and ensure Rscript is on PATH."
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  log "[FAIL] missing dataset: ${DATA_GOLDEN}"
  exit 1
fi

rm -f "${NLSS_REPORT_PATH}" "${ANALYSIS_LOG_PATH}"

cd "${WORKSPACE_DIR}"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "descriptive stats robust" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" \
  --vars outcome_anova,x1,x2 \
  --group group2 \
  --digits 3 \
  --trim 0.2 \
  --iqr-multiplier 2 \
  --outlier-z 2.5 \
  --template robust \
  --user-prompt "descriptive stats test"

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${ANALYSIS_LOG_PATH}" "${start_count}" \
  "vars=outcome_anova,x1,x2" \
  "group=group2" \
  "trim=0.2" \
  "iqr_multiplier=2" \
  "outlier_z=2.5" \
  "min_rows=3" \
  "nonnull=iqr,mad,trimmed_mean,p10,p90,outliers_tukey,outliers_z,cv"

assert_contains "robust template title" "${NLSS_REPORT_PATH}" "Descriptive Statistics (Robust)"

start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "descriptive stats distribution" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" \
  --vars outcome_anova,x1,x2 \
  --digits 3 \
  --template distribution \
  --user-prompt "descriptive stats distribution"

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${ANALYSIS_LOG_PATH}" "${start_count}" \
  "vars=outcome_anova,x1,x2" \
  "group=-" \
  "user_prompt=descriptive stats distribution"

assert_contains "distribution template title" "${NLSS_REPORT_PATH}" "Descriptive Statistics (Distribution)"

cp "${ROOT_DIR}/assets/descriptive-stats/robust-template.md" "${TEMPLATE_OVERRIDE_PATH}"
printf "\n\n%s\n" "${TEMPLATE_OVERRIDE_MARKER}" >> "${TEMPLATE_OVERRIDE_PATH}"

run_ok "descriptive stats template path" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" \
  --vars outcome_anova \
  --template "${TEMPLATE_OVERRIDE_PATH}"

assert_contains "template path marker" "${NLSS_REPORT_PATH}" "${TEMPLATE_OVERRIDE_MARKER}"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "descriptive stats log false" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" \
  --vars x1 \
  --log FALSE
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "log false"

start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "descriptive stats workspace implicit" bash -c "cd \"${DATASET_DIR}\" && Rscript \"${R_SCRIPT_DIR}/descriptive_stats.R\" --vars x2"

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${ANALYSIS_LOG_PATH}" "${start_count}" \
  "vars=x2" \
  "group=-" \
  "require_vars=x2"

run_ok "prepare edge datasets" Rscript - "${EDGE_CSV}" "${EDGE_SEMI_CSV}" "${EDGE_NOHEADER_CSV}" \
  "${EDGE_RDS_PATH}" "${EDGE_RDATA_PATH}" "${EDGE_RDATA_DF}" "${NONNUMERIC_CSV}" "${INTERACTIVE_INPUT}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
edge_csv <- args[1]
edge_semi <- args[2]
edge_noheader <- args[3]
edge_rds <- args[4]
edge_rdata <- args[5]
edge_df_name <- args[6]
non_numeric_csv <- args[7]
interactive_input <- args[8]

df <- data.frame(
  group = c("A", "A", "B", NA),
  val_const = c(5, 5, 5, 5),
  zero_mean = c(-1, 1, -1, 1),
  tie_mode = c(1, 1, 2, 2),
  all_na = c(NA, NA, NA, NA),
  outlier = c(1, 1, 1, 10),
  single_val = c(5, NA, NA, NA),
  stringsAsFactors = FALSE
)

write.csv(df, edge_csv, row.names = FALSE)
write.table(df, edge_semi, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(df[, c("val_const", "zero_mean")], edge_noheader, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
saveRDS(df, edge_rds)
assign(edge_df_name, df)
save(list = edge_df_name, file = edge_rdata)

non_numeric <- data.frame(group = c("A", "B"), cat1 = c("x", "y"), cat2 = c("m", "n"), stringsAsFactors = FALSE)
write.csv(non_numeric, non_numeric_csv, row.names = FALSE)

prompt_lines <- c(
  "csv",
  edge_csv,
  ",",
  "TRUE",
  "val_const,zero_mean",
  "",
  "2",
  "0.25",
  "1.5",
  "1.4",
  "distribution",
  "interactive prompt",
  "TRUE"
)
writeLines(prompt_lines, interactive_input)
RS

start_count="$(log_count "${EDGE_LOG_PATH}")"
run_ok "descriptive stats edge csv" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --csv "${EDGE_CSV}" \
  --vars val_const,zero_mean,tie_mode,all_na,outlier,single_val \
  --digits 2 \
  --trim 0.25 \
  --iqr-multiplier 1.5 \
  --outlier-z 1.4 \
  --user-prompt "edge csv"

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
  "vars=val_const,zero_mean,tie_mode,all_na,outlier,single_val" \
  "group=-" \
  "trim=0.25" \
  "outlier_z=1.4" \
  "expect_1=all_na|-|n|0" \
  "expect_2=all_na|-|missing_n|4" \
  "expect_3=all_na|-|missing_pct|100" \
  "expect_4=zero_mean|-|cv|NA" \
  "expect_5=tie_mode|-|mode|NA" \
  "expect_6=outlier|-|outliers_tukey|1" \
  "expect_7=outlier|-|outliers_z|1" \
  "expect_8=single_val|-|n|1" \
  "expect_9=zero_mean|-|trimmed_mean|0" \
  "expect_10=val_const|-|iqr|0"

start_count="$(log_count "${EDGE_LOG_PATH}")"
run_ok "descriptive stats edge group" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --csv "${EDGE_CSV}" \
  --vars val_const \
  --group group \
  --digits 2

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
  "vars=val_const" \
  "group=group" \
  "expect_1=val_const|NA|n|1"

start_count="$(log_count "${EDGE_SEMI_LOG_PATH}")"
run_ok "descriptive stats edge semicolon" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --csv "${EDGE_SEMI_CSV}" \
  --sep ";" \
  --vars val_const,zero_mean

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_SEMI_LOG_PATH}" "${start_count}" \
  "vars=val_const,zero_mean" \
  "group=-"

start_count="$(log_count "${EDGE_NOHEADER_LOG_PATH}")"
run_ok "descriptive stats edge no header" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --csv "${EDGE_NOHEADER_CSV}" \
  --header FALSE \
  --vars V1,V2

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_NOHEADER_LOG_PATH}" "${start_count}" \
  "vars=V1,V2" \
  "group=-"

start_count="$(log_count "${EDGE_LOG_PATH}")"
run_ok "descriptive stats edge rds" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --rds "${EDGE_RDS_PATH}" \
  --vars val_const,zero_mean

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
  "vars=val_const,zero_mean" \
  "group=-"

start_count="$(log_count "${EDGE_LOG_PATH}")"
run_ok "descriptive stats edge rdata" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --rdata "${EDGE_RDATA_PATH}" \
  --df "${EDGE_RDATA_DF}" \
  --vars val_const,zero_mean

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
  "vars=val_const,zero_mean" \
  "group=-"

if Rscript -e "quit(status = if (requireNamespace('haven', quietly=TRUE)) 0 else 1)" >/dev/null 2>&1; then
  run_ok "prepare edge sav" Rscript -e "library(haven); df <- read.csv(\"${EDGE_CSV}\", stringsAsFactors = FALSE); write_sav(df, \"${EDGE_SAV_PATH}\")"
  start_count="$(log_count "${EDGE_LOG_PATH}")"
  run_ok "descriptive stats edge sav" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
    --sav "${EDGE_SAV_PATH}" \
    --vars val_const,zero_mean

  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
    "vars=val_const,zero_mean" \
    "group=-"
else
  log "[WARN] skipping sav input (haven not installed)"
fi

start_count="$(log_count "${EDGE_LOG_PATH}")"
run_ok "descriptive stats interactive" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --interactive

"${PYTHON_BIN}" "${CHECK_SCRIPT}" "${EDGE_LOG_PATH}" "${start_count}" \
  "vars=val_const,zero_mean" \
  "group=-" \
  "trim=0.25" \
  "iqr_multiplier=1.5" \
  "outlier_z=1.4" \
  "user_prompt=interactive prompt"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "unknown vars" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars not_a_var
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "unknown vars"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "invalid group var" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars x1 --group not_a_group
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "invalid group var"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "non-numeric vars" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars gender
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "non-numeric vars"

before_count="$(log_count "${NONNUMERIC_LOG_PATH}")"
run_expect_fail "no numeric vars" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --csv "${NONNUMERIC_CSV}"
after_count="$(log_count "${NONNUMERIC_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "no numeric vars"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "trim out of range" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars outcome_anova --trim 0.6
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "trim out of range"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "iqr multiplier invalid" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars outcome_anova --iqr-multiplier 0
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "iqr multiplier invalid"

before_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_expect_fail "outlier z invalid" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" \
  --parquet "${PARQUET_PATH}" --vars outcome_anova --outlier-z 0
after_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${before_count}" "${after_count}" "outlier z invalid"

log "[DONE] descriptive_stats tests finished"
