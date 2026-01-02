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
CHECK_SCRIPT="${ROOT_DIR}/tests/smoke/check_nonparametric_log.py"
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"

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
DATA_MAIN="${DATA_DIR}/t_test_main.csv"
DATA_GOLDEN_CFG="$(get_tests_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"

DATA_MAIN_LABEL="$(basename "${DATA_MAIN}")"
DATA_MAIN_LABEL="${DATA_MAIN_LABEL%.*}"
DATA_GOLDEN_LABEL="$(basename "${DATA_GOLDEN}")"
DATA_GOLDEN_LABEL="${DATA_GOLDEN_LABEL%.*}"

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

WORKSPACE_DIR="${RUN_ROOT}/nonparametric_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/nonparametric"
LOG_FILE="${RUN_ROOT}/nonparametric_test.log"

HELP_PATH="${TMP_BASE}/nonparametric_help.txt"
INTERACTIVE_INPUT="${TMP_BASE}/nonparametric_interactive.txt"
RDS_PATH="${TMP_BASE}/nonparametric_rds_input.rds"
RDATA_PATH="${TMP_BASE}/nonparametric_rdata_input.RData"
RDATA_BAD_PATH="${TMP_BASE}/nonparametric_rdata_bad.RData"
RDATA_DF="np_df"
SAV_PATH="${TMP_BASE}/nonparametric_sav_input.sav"
CSV_SEMI_PATH="${TMP_BASE}/nonparametric_semicolon.csv"
CSV_NOHEADER_PATH="${TMP_BASE}/nonparametric_noheader.csv"
CSV_NO_NUMERIC_PATH="${TMP_BASE}/nonparametric_no_numeric.csv"
PARQUET_PATH="${TMP_BASE}/nonparametric_parquet_input.parquet"
TEMPLATE_TMP="${TMP_BASE}/nonparametric_template_override.md"

RDS_LABEL="$(basename "${RDS_PATH}")"
RDS_LABEL="${RDS_LABEL%.*}"
RDATA_LABEL="${RDATA_DF}"
SAV_LABEL="$(basename "${SAV_PATH}")"
SAV_LABEL="${SAV_LABEL%.*}"
CSV_SEMI_LABEL="$(basename "${CSV_SEMI_PATH}")"
CSV_SEMI_LABEL="${CSV_SEMI_LABEL%.*}"
CSV_NOHEADER_LABEL="$(basename "${CSV_NOHEADER_PATH}")"
CSV_NOHEADER_LABEL="${CSV_NOHEADER_LABEL%.*}"
PARQUET_LABEL="$(basename "${PARQUET_PATH}")"
PARQUET_LABEL="${PARQUET_LABEL%.*}"

MAIN_DATASET_DIR="${WORKSPACE_DIR}/${DATA_MAIN_LABEL}"
GOLDEN_DATASET_DIR="${WORKSPACE_DIR}/${DATA_GOLDEN_LABEL}"
MAIN_LOG_PATH="${MAIN_DATASET_DIR}/analysis_log.jsonl"
GOLDEN_LOG_PATH="${GOLDEN_DATASET_DIR}/analysis_log.jsonl"
NLSS_MAIN_REPORT_PATH="${MAIN_DATASET_DIR}/report_canonical.md"
NLSS_GOLDEN_REPORT_PATH="${GOLDEN_DATASET_DIR}/report_canonical.md"
LOG_PATH_RDS="${WORKSPACE_DIR}/${RDS_LABEL}/analysis_log.jsonl"
LOG_PATH_RDATA="${WORKSPACE_DIR}/${RDATA_LABEL}/analysis_log.jsonl"
LOG_PATH_SAV="${WORKSPACE_DIR}/${SAV_LABEL}/analysis_log.jsonl"
LOG_PATH_SEMI="${WORKSPACE_DIR}/${CSV_SEMI_LABEL}/analysis_log.jsonl"
LOG_PATH_NOHEADER="${WORKSPACE_DIR}/${CSV_NOHEADER_LABEL}/analysis_log.jsonl"
LOG_PATH_PARQUET="${WORKSPACE_DIR}/${PARQUET_LABEL}/analysis_log.jsonl"

mkdir -p "${RUN_ROOT}"
: > "${LOG_FILE}"

cleanup() {
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}
trap cleanup EXIT

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

if [ ! -f "${DATA_MAIN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_MAIN}" | tee -a "${LOG_FILE}"
  exit 1
fi
if [ ! -f "${DATA_GOLDEN}" ]; then
  echo "[FAIL] missing dataset: ${DATA_GOLDEN}" | tee -a "${LOG_FILE}"
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

assert_contains_count() {
  local file="$1"
  local expected="$2"
  local min_count="$3"
  local count
  count="$("${PYTHON_BIN}" - "$file" "$expected" <<'PY'
import sys
path = sys.argv[1]
needle = sys.argv[2]
text = open(path, "r", encoding="utf-8").read()
print(text.count(needle))
PY
)"
  if [ "${count}" -lt "${min_count}" ]; then
    echo "Expected at least ${min_count} occurrences of: ${expected}" | tee -a "${LOG_FILE}"
    echo "Found ${count} occurrences in: ${file}" | tee -a "${LOG_FILE}"
    exit 1
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

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
HAS_ARROW=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" arrow >/dev/null 2>&1; then
  HAS_ARROW=1
fi

# Preparation
run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/nonparametric.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Nonparametric tests"

run_ok "prepare inputs" Rscript -e "df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write.table(df, \"${CSV_SEMI_PATH}\", sep = \";\", row.names = FALSE, col.names = TRUE, quote = FALSE); write.table(df, \"${CSV_NOHEADER_PATH}\", sep = \",\", row.names = FALSE, col.names = FALSE, quote = FALSE); write.csv(df[, c(\"group\", \"group3\", \"nonnumeric\")], \"${CSV_NO_NUMERIC_PATH}\", row.names = FALSE); saveRDS(df, \"${RDS_PATH}\"); ${RDATA_DF} <- df; save(${RDATA_DF}, file = \"${RDATA_PATH}\"); bad_obj <- 1; save(bad_obj, file = \"${RDATA_BAD_PATH}\")"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav input" Rscript -e "library(haven); df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

if [ "${HAS_ARROW}" -eq 1 ]; then
  run_ok "prepare parquet input" Rscript -e "library(arrow); df <- read.csv(\"${DATA_MAIN}\", stringsAsFactors = FALSE); write_parquet(df, \"${PARQUET_PATH}\")"
else
  echo "[SKIP] parquet input (arrow not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${INTERACTIVE_INPUT}" <<EOF
csv
${CSV_SEMI_PATH}
;
TRUE
wilcoxon
score





1
greater
TRUE
FALSE
0.9
none
holm
rb
3

interactive nonparametric
TRUE
EOF

run_ok "init workspace (main dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_MAIN}"
run_ok "init workspace (golden dataset)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

# Positive tests
start=$(log_count "${MAIN_LOG_PATH}")
run_ok "wilcoxon one-sample options" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --mu 1 --test wilcoxon --alternative greater --exact TRUE --continuity FALSE --conf-level 0.9 --effect-size rb --digits 3 --user-prompt "one-sample prompt"
check_log "${MAIN_LOG_PATH}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score mu=1 alternative=greater exact=true continuity=false conf_level=0.9 effect_size=rb digits=3 user_prompt="one-sample prompt"

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "wilcoxon paired multi" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --x pre,score --y post,score2 --alternative less --exact FALSE --continuity TRUE
check_log "${MAIN_LOG_PATH}" "${start}" "-" "wilcoxon_paired" test=wilcoxon x=pre,score y=post,score2 alternative=less exact=false continuity=true effect_size=r digits=2

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "mann_whitney auto default vars" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --group group
check_log "${MAIN_LOG_PATH}" "${start}" "-" "mann_whitney" test=mann_whitney group=group vars=score alternative=two.sided effect_size=r conf_level=0.95

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "kruskal posthoc bonferroni" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score,score2 --group group3 --posthoc pairwise --p-adjust bonferroni --effect-size r
check_log "${MAIN_LOG_PATH}" "${start}" "-" "kruskal" test=kruskal group=group3 vars=score,score2 posthoc=pairwise p_adjust=bonferroni effect_size=epsilon_sq
assert_contains "${NLSS_MAIN_REPORT_PATH}" "Nonparametric Post Hoc"

start=$(log_count "${GOLDEN_LOG_PATH}")
run_ok "friedman posthoc" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_GOLDEN}" --within pre_score,mid_score,post_score --subject-id id --posthoc pairwise --p-adjust BH --effect-size r
check_log "${GOLDEN_LOG_PATH}" "${start}" "-" "friedman" test=friedman within=pre_score,mid_score,post_score subject_id=id posthoc=pairwise p_adjust=BH effect_size=kendall_w
assert_contains "${NLSS_GOLDEN_REPORT_PATH}" "Nonparametric Post Hoc"

start=$(log_count "${LOG_PATH_SEMI}")
run_ok "csv semicolon" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${CSV_SEMI_PATH}" --sep ";" --header TRUE --vars score --test wilcoxon
check_log "${LOG_PATH_SEMI}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score

start=$(log_count "${LOG_PATH_NOHEADER}")
run_ok "csv header false" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${CSV_NOHEADER_PATH}" --header FALSE --vars V3 --group V1 --test mann_whitney
check_log "${LOG_PATH_NOHEADER}" "${start}" "-" "mann_whitney" test=mann_whitney vars=V3 group=V1

start=$(log_count "${LOG_PATH_RDS}")
run_ok "rds input" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --rds "${RDS_PATH}" --vars score
check_log "${LOG_PATH_RDS}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score

start=$(log_count "${LOG_PATH_RDATA}")
run_ok "rdata input" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF}" --vars score --group group --test mann_whitney
check_log "${LOG_PATH_RDATA}" "${start}" "-" "mann_whitney" test=mann_whitney group=group vars=score

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH_SAV}")
  run_ok "sav input" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --sav "${SAV_PATH}" --vars score --test wilcoxon
  check_log "${LOG_PATH_SAV}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score
fi

if [ "${HAS_ARROW}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH_PARQUET}")
  run_ok "parquet input" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH}" --vars score --group group --test mann_whitney
  check_log "${LOG_PATH_PARQUET}" "${start}" "-" "mann_whitney" test=mann_whitney group=group vars=score
fi

# Template override (post-hoc uses the same template path)
cp "${ROOT_DIR}/assets/nonparametric/default-template.md" "${TEMPLATE_TMP}"
TEMPLATE_MARKER="NONPARAMETRIC_TEMPLATE_OVERRIDE"
printf "\n\n%s\n" "${TEMPLATE_MARKER}" >>"${TEMPLATE_TMP}"
start=$(log_count "${MAIN_LOG_PATH}")
run_ok "template override posthoc" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --group group3 --test kruskal --posthoc pairwise --template "${TEMPLATE_TMP}"
check_log "${MAIN_LOG_PATH}" "${start}" "-" "kruskal" test=kruskal posthoc=pairwise effect_size=epsilon_sq
assert_contains_count "${NLSS_MAIN_REPORT_PATH}" "${TEMPLATE_MARKER}" 2

start=$(log_count "${LOG_PATH_SEMI}")
run_ok "interactive wilcoxon" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --interactive
check_log "${LOG_PATH_SEMI}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score mu=1 alternative=greater exact=true continuity=false conf_level=0.9 effect_size=rb digits=3 user_prompt="interactive nonparametric"

start=$(log_count "${MAIN_LOG_PATH}")
pushd "${MAIN_DATASET_DIR}" >/dev/null
run_ok "workspace run (no input flags)" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --vars score --test wilcoxon
popd >/dev/null
check_log "${MAIN_LOG_PATH}" "${start}" "-" "wilcoxon_one_sample" test=wilcoxon vars=score

start=$(log_count "${MAIN_LOG_PATH}")
run_ok "log disabled" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --test wilcoxon --log FALSE
end=$(log_count "${MAIN_LOG_PATH}")
assert_log_unchanged "${start}" "${end}" "log disabled"

# Invalid input tests (logged)
run_expect_invalid "paired with group" "invalid_input" "${MAIN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --x pre --y post --group group

run_expect_invalid "paired mismatched lengths" "invalid_input" "${MAIN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --x pre,score --y post

run_expect_invalid "friedman missing subject-id" "invalid_input" "${GOLDEN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_GOLDEN}" --within pre_score,mid_score --test friedman

run_expect_invalid "friedman with vars" "invalid_input" "${GOLDEN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_GOLDEN}" --within pre_score,mid_score --subject-id id --vars score

run_expect_invalid "kruskal missing group" "invalid_input" "${MAIN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --test kruskal

run_expect_invalid "wilcoxon with group" "invalid_input" "${MAIN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --group group --test wilcoxon

run_expect_invalid "mann_whitney invalid group levels" "invalid_input" "${MAIN_LOG_PATH}" "-" -- \
  Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars score --group group3 --test mann_whitney

# Expected failures (no structured invalid_input log)
run_expect_fail "unknown variable" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars missing_var
run_expect_fail "non-numeric variable" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${DATA_MAIN}" --vars nonnumeric
run_expect_fail "no numeric variables" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --csv "${CSV_NO_NUMERIC_PATH}"
run_expect_fail "rdata missing df" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --rdata "${RDATA_PATH}" --vars score
run_expect_fail "rdata non-dataframe object" Rscript "${R_SCRIPT_DIR}/nonparametric.R" --rdata "${RDATA_BAD_PATH}" --df bad_obj --vars score

echo "[DONE] nonparametric tests finished" | tee -a "${LOG_FILE}"
