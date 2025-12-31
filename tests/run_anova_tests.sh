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
CHECK_SCRIPT="${ROOT_DIR}/tests/check_anova_log.py"
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
DATA_GOLDEN_CFG="$(get_tests_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"
DATA_EDGE="${DATA_DIR}/anova_edge_dataset.csv"
DATA_MISSING="${DATA_DIR}/anova_all_missing.csv"

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

WORKSPACE_DIR="${RUN_ROOT}/anova_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/anova"
LOG_FILE="${RUN_ROOT}/anova_test.log"

DATASET_GOLDEN_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_GOLDEN_LABEL="${DATASET_GOLDEN_LABEL%.*}"
DATASET_EDGE_LABEL="$(basename "${DATA_EDGE}")"
DATASET_EDGE_LABEL="${DATASET_EDGE_LABEL%.*}"
DATASET_MISSING_LABEL="$(basename "${DATA_MISSING}")"
DATASET_MISSING_LABEL="${DATASET_MISSING_LABEL%.*}"

DATASET_GOLDEN_DIR="${WORKSPACE_DIR}/${DATASET_GOLDEN_LABEL}"
DATASET_EDGE_DIR="${WORKSPACE_DIR}/${DATASET_EDGE_LABEL}"
DATASET_MISSING_DIR="${WORKSPACE_DIR}/${DATASET_MISSING_LABEL}"

NLSS_REPORT_PATH="${DATASET_GOLDEN_DIR}/report_canonical.md"
SCRATCHPAD_PATH="${DATASET_GOLDEN_DIR}/scratchpad.md"
LOG_PATH="${DATASET_GOLDEN_DIR}/analysis_log.jsonl"
LOG_PATH_EDGE="${DATASET_EDGE_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_GOLDEN_DIR}/${DATASET_GOLDEN_LABEL}.parquet"
PARQUET_EDGE="${DATASET_EDGE_DIR}/${DATASET_EDGE_LABEL}.parquet"
PARQUET_MISSING="${DATASET_MISSING_DIR}/${DATASET_MISSING_LABEL}.parquet"
RDS_PATH="${TMP_BASE}/anova_rds_input.rds"
RDATA_PATH="${TMP_BASE}/anova_rdata_input.RData"
RDATA_DF="anova_rdata_df"
SAV_PATH="${TMP_BASE}/anova_sav_input.sav"
CSV_SEMI_PATH="${TMP_BASE}/anova_semicolon.csv"
CSV_NO_HEADER_PATH="${TMP_BASE}/anova_no_header.csv"
INTERACTIVE_INPUT="${TMP_BASE}/anova_interactive.txt"
HELP_PATH="${TMP_BASE}/anova_help.txt"
CONTRAST_JSON_PATH="${TMP_BASE}/anova_contrasts.json"

DATASET_RDS_LABEL="anova_rds_input"
DATASET_RDS_DIR="${WORKSPACE_DIR}/${DATASET_RDS_LABEL}"
LOG_PATH_RDS="${DATASET_RDS_DIR}/analysis_log.jsonl"

DATASET_RDATA_LABEL="${RDATA_DF}"
DATASET_RDATA_DIR="${WORKSPACE_DIR}/${DATASET_RDATA_LABEL}"
LOG_PATH_RDATA="${DATASET_RDATA_DIR}/analysis_log.jsonl"

DATASET_SAV_LABEL="anova_sav_input"
DATASET_SAV_DIR="${WORKSPACE_DIR}/${DATASET_SAV_LABEL}"
LOG_PATH_SAV="${DATASET_SAV_DIR}/analysis_log.jsonl"

DATASET_CSV_SEMI_LABEL="anova_semicolon"
DATASET_CSV_SEMI_DIR="${WORKSPACE_DIR}/${DATASET_CSV_SEMI_LABEL}"
LOG_PATH_CSV_SEMI="${DATASET_CSV_SEMI_DIR}/analysis_log.jsonl"

DATASET_CSV_NO_HEADER_LABEL="anova_no_header"
DATASET_CSV_NO_HEADER_DIR="${WORKSPACE_DIR}/${DATASET_CSV_NO_HEADER_LABEL}"
LOG_PATH_CSV_NO_HEADER="${DATASET_CSV_NO_HEADER_DIR}/analysis_log.jsonl"

mkdir -p "${RUN_ROOT}"

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

: > "${LOG_FILE}"

cleanup() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}"
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_EDGE}" ]; then
  echo "[FAIL] missing dataset: ${DATA_EDGE}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_MISSING}" ]; then
  echo "[FAIL] missing dataset: ${DATA_MISSING}" | tee -a "${LOG_FILE}"
  exit 1
fi

mkdir -p "${WORKSPACE_DIR}" "${DATASET_GOLDEN_DIR}" "${DATASET_EDGE_DIR}" "${DATASET_MISSING_DIR}"
mkdir -p "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
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

check_anova_log() {
  local start_count="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${LOG_PATH}" "${start_count}" "$@"
}

check_anova_log_path() {
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
  if check_anova_log "${start_count}" "-" "-" "-" "-" "-" "${status}"; then
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

cleanup_runs() {
  local keep="${NLSS_KEEP_RUNS:-10}"
  local base="${RUNS_BASE}"
  if ! [[ "${keep}" =~ ^[0-9]+$ ]]; then
    keep=10
  fi
  if [ ! -d "${base}" ]; then
    return 0
  fi
  local runs=()
  while IFS= read -r name; do
    if [[ "${name}" =~ ^[0-9]{14}$ ]]; then
      runs+=("${name}")
    fi
  done < <(find "${base}" -maxdepth 1 -mindepth 1 -type d -printf "%f\n" | sort)
  local count="${#runs[@]}"
  if [ "${count}" -le "${keep}" ]; then
    return 0
  fi
  local remove_count=$((count - keep))
  for ((i = 0; i < remove_count; i++)); do
    rm -rf "${base}/${runs[$i]}"
  done
}

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
HAS_CAR=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" car >/dev/null 2>&1; then
  HAS_CAR=1
fi
HAS_EMMEANS=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" emmeans >/dev/null 2>&1; then
  HAS_EMMEANS=1
fi

EXPECTED_TYPE_II="II"
EXPECTED_TYPE_III="III"
if [ "${HAS_CAR}" -ne 1 ]; then
  EXPECTED_TYPE_II="I"
  EXPECTED_TYPE_III="I"
fi

run_ok "prepare rds/rdata/csv inputs" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write.table(df, \"${CSV_SEMI_PATH}\", sep = \";\", row.names = FALSE, col.names = TRUE, quote = FALSE); write.table(df[, c(\"outcome_anova\", \"group3\")], \"${CSV_NO_HEADER_PATH}\", sep = \",\", row.names = FALSE, col.names = FALSE, quote = FALSE); saveRDS(df, \"${RDS_PATH}\"); ${RDATA_DF} <- df; save(${RDATA_DF}, file = \"${RDATA_PATH}\")"

cat >"${CONTRAST_JSON_PATH}" <<'EOF'
{
  "term": "group3",
  "contrasts": {
    "A_vs_B": {"A": 1, "B": -1, "C": 0},
    "A_vs_C": [1, 0, -1]
  }
}
EOF

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav input" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${INTERACTIVE_INPUT}" <<EOF
csv
${CSV_SEMI_PATH}
;
TRUE
between
outcome_anova
group3

II
partial_eta
tukey
none
none
holm
0.95
auto
FALSE
200
2

interactive anova
TRUE
EOF

run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/anova.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "ANOVA (base R)"

run_ok "init workspace (golden dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
run_ok "init workspace (edge dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_EDGE}"
run_ok "init workspace (all missing dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_MISSING}"

# Positive tests
start=$(log_count "${LOG_PATH}")
run_ok "anova between default" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" p_adjust=holm conf_level=0.95 assumptions=Normality,Homogeneity assumption_tests="Shapiro-Wilk,Levene (median),Bartlett,Fligner-Killeen" posthoc_p_adj=present || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between factorial covariate type 3" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3,gender --covariates age --type 3 --effect-size eta_sq --posthoc pairwise --p-adjust bonferroni --conf-level 0.9 --user-prompt "anova factorial test"
check_anova_log "${start}" "between" "pairwise" "gt0" "eta_sq" "-" "-" type="${EXPECTED_TYPE_III}" p_adjust=bonferroni conf_level=0.9 user_prompt="anova factorial test" covariates=age posthoc_p_adj=present || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between omega" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --effect-size omega_sq --posthoc none
check_anova_log "${start}" "between" "none" "0" "omega_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova between posthoc no" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --posthoc no
check_anova_log "${start}" "between" "none" "0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH}")
  run_ok "anova planned contrasts custom" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --emmeans group3 --contrasts custom --contrast-file "${CONTRAST_JSON_PATH}"
  check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" contrasts=custom contrast_rows=gt0 || exit 1

  start=$(log_count "${LOG_PATH}")
  run_ok "anova planned contrasts method" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --emmeans group3 --contrasts trt.vs.ctrl
  check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" contrasts=trt.vs.ctrl contrast_rows=gt0 || exit 1
else
  echo "[SKIP] planned contrasts (emmeans not installed)" | tee -a "${LOG_FILE}"
fi

start=$(log_count "${LOG_PATH}")
run_ok "anova between bootstrap" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --bootstrap TRUE --bootstrap-samples 200
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" "present" bootstrap_samples=200 || exit 1

start=$(log_count "${LOG_PATH_RDS}")
run_ok "anova rds input options" Rscript "${R_SCRIPT_DIR}/anova.R" --rds "${RDS_PATH}" --dv outcome_anova --between group3 --type I --effect-size eta2 --posthoc pairs --p-adjust none --conf-level 0.9 --digits 3 --user-prompt "anova rds test"
check_anova_log_path "${LOG_PATH_RDS}" "${start}" "between" "pairwise" "gt0" "eta_sq" "-" "-" type=I p_adjust=none conf_level=0.9 digits=3 user_prompt="anova rds test" posthoc_p_adj=present assumptions=Normality,Homogeneity || exit 1

start=$(log_count "${LOG_PATH_RDATA}")
run_ok "anova rdata input type 3" Rscript "${R_SCRIPT_DIR}/anova.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --dv outcome_anova --between group3 --type 3 --effect-size partialeta --posthoc tukey
check_anova_log_path "${LOG_PATH_RDATA}" "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_III}" assumption_tests="Shapiro-Wilk,Levene (median),Bartlett,Fligner-Killeen" || exit 1

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH_SAV}")
  run_ok "anova sav input" Rscript "${R_SCRIPT_DIR}/anova.R" --sav "${SAV_PATH}" --dv outcome_anova --between group3 --posthoc tukey
  check_anova_log_path "${LOG_PATH_SAV}" "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1
fi

start=$(log_count "${LOG_PATH_CSV_SEMI}")
run_ok "anova csv custom sep" Rscript "${R_SCRIPT_DIR}/anova.R" --csv "${CSV_SEMI_PATH}" --sep ";" --header TRUE --dv outcome_anova --between group3 --posthoc tukey
check_anova_log_path "${LOG_PATH_CSV_SEMI}" "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1

start=$(log_count "${LOG_PATH_CSV_NO_HEADER}")
run_ok "anova csv header false" Rscript "${R_SCRIPT_DIR}/anova.R" --csv "${CSV_NO_HEADER_PATH}" --header FALSE --dv V1 --between V2 --posthoc none
check_anova_log_path "${LOG_PATH_CSV_NO_HEADER}" "${start}" "between" "none" "0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1

start=$(log_count "${LOG_PATH_EDGE}")
run_ok "anova CSV input (edge dataset)" Rscript "${R_SCRIPT_DIR}/anova.R" --csv "${DATA_EDGE}" --dv dv_num --between group --posthoc tukey
check_anova_log_path "${LOG_PATH_EDGE}" "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within sphericity auto" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --posthoc pairwise
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "present" "-" assumptions=Normality,Sphericity || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within sphericity no" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --sphericity no --posthoc pairwise
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "absent" "-" assumptions=Normality || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within bootstrap eta" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --effect-size eta2 --bootstrap TRUE --bootstrap-samples 20 --posthoc pairwise
check_anova_log "${start}" "within" "pairwise" "gt0" "eta_sq" "present" "-" "present" bootstrap_samples=20 assumptions=Normality,Sphericity || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within posthoc none" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --posthoc none
check_anova_log "${start}" "within" "none" "0" "partial_eta_sq" "absent" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within partial omega" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --effect-size partial_omega --posthoc none
check_anova_log "${start}" "within" "none" "0" "partial_omega_sq" "absent" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova within posthoc tukey requested" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --posthoc tukey
check_anova_log "${start}" "within" "pairwise" "gt0" "partial_eta_sq" "-" "-" || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova mixed with covariate" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --between group3 --subject-id id --covariates age --posthoc pairwise --p-adjust BH --sphericity none
check_anova_log "${start}" "mixed" "pairwise" "gt0" "partial_eta_sq" "absent" "-" p_adjust=BH covariates=age posthoc_groups=grouped assumptions=Normality,Homogeneity || exit 1

start=$(log_count "${LOG_PATH}")
run_ok "anova mixed sphericity auto grouped posthoc" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score,post_score --between group3 --subject-id id --posthoc pairwise
check_anova_log "${start}" "mixed" "pairwise" "gt0" "partial_eta_sq" "present" "-" posthoc_groups=grouped assumptions=Normality,Homogeneity,Sphericity || exit 1

start=$(log_count "${LOG_PATH_CSV_SEMI}")
run_ok "anova interactive between" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/anova.R" --interactive
check_anova_log_path "${LOG_PATH_CSV_SEMI}" "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" p_adjust=holm user_prompt="interactive anova" || exit 1

start=$(log_count "${LOG_PATH}")
pushd "${DATASET_GOLDEN_DIR}" >/dev/null
run_ok "anova workspace (no input flags)" Rscript "${R_SCRIPT_DIR}/anova.R" --dv outcome_anova --between group3
popd >/dev/null
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1

TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.anova.default)"
if [ -z "${TEMPLATE_DEFAULT_ORIG}" ]; then
  TEMPLATE_DEFAULT_ORIG="anova/default-template.md"
fi
TEMPLATE_POSTHOC_ORIG="$(get_config_value templates.anova.posthoc)"
if [ -z "${TEMPLATE_POSTHOC_ORIG}" ]; then
  TEMPLATE_POSTHOC_ORIG="anova/posthoc-template.md"
fi
TEMPLATE_CONTRASTS_ORIG="$(get_config_value templates.anova.contrasts)"
if [ -z "${TEMPLATE_CONTRASTS_ORIG}" ]; then
  TEMPLATE_CONTRASTS_ORIG="anova/contrasts-template.md"
fi

TEMPLATE_DEFAULT_TMP="${TMP_BASE}/anova_default_template.md"
TEMPLATE_POSTHOC_TMP="${TMP_BASE}/anova_posthoc_template.md"
TEMPLATE_CONTRASTS_TMP="${TMP_BASE}/anova_contrasts_template.md"

cp "$(resolve_template_source "${TEMPLATE_DEFAULT_ORIG}")" "${TEMPLATE_DEFAULT_TMP}"
cp "$(resolve_template_source "${TEMPLATE_POSTHOC_ORIG}")" "${TEMPLATE_POSTHOC_TMP}"
cp "$(resolve_template_source "${TEMPLATE_CONTRASTS_ORIG}")" "${TEMPLATE_CONTRASTS_TMP}"

printf "\n\nANOVA_TEMPLATE_DEFAULT\n" >>"${TEMPLATE_DEFAULT_TMP}"
printf "\n\nANOVA_TEMPLATE_POSTHOC\n" >>"${TEMPLATE_POSTHOC_TMP}"
printf "\n\nANOVA_TEMPLATE_CONTRASTS\n" >>"${TEMPLATE_CONTRASTS_TMP}"

set_config_value templates.anova.default "${TEMPLATE_DEFAULT_TMP}"
set_config_value templates.anova.posthoc "${TEMPLATE_POSTHOC_TMP}"
set_config_value templates.anova.contrasts "${TEMPLATE_CONTRASTS_TMP}"

start=$(log_count "${LOG_PATH}")
run_ok "template override default" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --posthoc none
check_anova_log "${start}" "between" "none" "0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1
assert_contains "${NLSS_REPORT_PATH}" "ANOVA_TEMPLATE_DEFAULT"

start=$(log_count "${LOG_PATH}")
run_ok "template override posthoc" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --posthoc tukey
check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" type="${EXPECTED_TYPE_II}" || exit 1
assert_contains "${NLSS_REPORT_PATH}" "ANOVA_TEMPLATE_POSTHOC"

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH}")
  run_ok "template override contrasts" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --emmeans group3 --contrasts custom --contrast-file "${CONTRAST_JSON_PATH}"
  check_anova_log "${start}" "between" "tukey" "gt0" "partial_eta_sq" "-" "-" contrasts=custom contrast_rows=gt0 || exit 1
  assert_contains "${NLSS_REPORT_PATH}" "ANOVA_TEMPLATE_CONTRASTS"
else
  echo "[WARN] skipping contrasts template tests (emmeans not installed)" | tee -a "${LOG_FILE}"
fi

set_config_value templates.anova.default "${TEMPLATE_DEFAULT_ORIG}"
set_config_value templates.anova.posthoc "${TEMPLATE_POSTHOC_ORIG}"
set_config_value templates.anova.contrasts "${TEMPLATE_CONTRASTS_ORIG}"

start=$(log_count "${LOG_PATH}")
run_ok "anova log false" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --log FALSE
end=$(log_count "${LOG_PATH}")
assert_log_unchanged "${start}" "${end}" "anova log false"

# Negative tests (invalid input with logged status)
run_expect_invalid "anova missing subject-id" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score
run_expect_invalid "anova within length <2" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score --subject-id id
run_expect_invalid "anova missing dv" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --between group3
run_expect_invalid "anova missing between/within" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova
run_expect_invalid "anova overlap within and dv" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,mid_score --subject-id id --dv pre_score
run_expect_invalid "anova overlap within and between" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --between pre_score
run_expect_invalid "anova overlap within and covariates" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,post_score --subject-id id --covariates pre_score
run_expect_invalid "anova contrasts missing file" "invalid_input" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --emmeans group3 --contrasts custom

# Negative tests (expected failures without logged status)
run_expect_fail "anova nonnumeric dv" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv dv_char --between group
run_expect_fail "anova nonnumeric within" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --within pre_text,post --subject-id id
run_expect_fail "anova nonnumeric covariate" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv dv_num --between group --covariates covar_char
run_expect_fail "anova unknown dv" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_EDGE}" --dv not_a_var --between group
run_expect_fail "anova unknown between" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between not_a_var
run_expect_fail "anova unknown covariate" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --dv outcome_anova --between group3 --covariates not_a_var
run_expect_fail "anova unknown within" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_GOLDEN}" --within pre_score,not_a_var --subject-id id
run_expect_fail "anova rdata missing df" Rscript "${R_SCRIPT_DIR}/anova.R" --rdata "${RDATA_PATH}" --dv outcome_anova --between group3
run_expect_fail "anova no complete cases" Rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_MISSING}" --dv dv_num --between group

echo "ANOVA tests: OK" | tee -a "${LOG_FILE}"
cleanup_runs
