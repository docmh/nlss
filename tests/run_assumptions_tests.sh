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
ASSUMPTIONS_SCRIPT="${R_SCRIPT_DIR}/assumptions.R"
INIT_SCRIPT="${R_SCRIPT_DIR}/init_workspace.R"
CHECK_SCRIPT="${ROOT_DIR}/tests/check_assumptions_log.py"
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

WORKSPACE_DIR="${RUN_ROOT}/assumptions_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/assumptions"
LOG_FILE="${RUN_ROOT}/assumptions_test.log"

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
  if [ "${before}" -ne "${after}" ]; then
    echo "[FAIL] ${label} (expected log count unchanged)" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

run_expect_fail() {
  local label="$1"; shift
  local log_path="$1"; shift
  echo "[RUN-EXPECT-FAIL] ${label}" | tee -a "${LOG_FILE}"
  local start_count=""
  if [ -n "${log_path}" ] && [ "${log_path}" != "-" ]; then
    start_count="$(log_count "${log_path}")"
  fi
  set +e
  "$@" >>"${LOG_FILE}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -eq 0 ]; then
    echo "[FAIL] ${label} (unexpected success)" | tee -a "${LOG_FILE}"
    exit 1
  fi
  if [ -n "${log_path}" ] && [ "${log_path}" != "-" ]; then
    local end_count
    end_count="$(log_count "${log_path}")"
    assert_log_unchanged "${start_count}" "${end_count}" "${label}"
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

check_log() {
  local log_path="$1"; shift
  "${PYTHON_BIN}" "${CHECK_SCRIPT}" "${log_path}" "$@"
}

check_log_main() {
  local start_count="$1"; shift
  check_log "${LOG_PATH}" "${start_count}" "$@"
}

check_log_mixed() {
  local start_count="$1"; shift
  check_log "${MIXED_LOG_PATH}" "${start_count}" "$@"
}

if ! Rscript "${CHECK_PKG_SCRIPT}" lme4 >/dev/null 2>&1; then
  echo "[FAIL] assumptions mixed_models checks require the lme4 package." | tee -a "${LOG_FILE}"
  exit 1
fi

if ! Rscript "${CHECK_PKG_SCRIPT}" lavaan >/dev/null 2>&1; then
  echo "[FAIL] assumptions sem checks require the lavaan package." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_HAVEN=0
if Rscript "${CHECK_PKG_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi

HAS_MVN=0
if Rscript "${CHECK_PKG_SCRIPT}" MVN >/dev/null 2>&1; then
  HAS_MVN=1
fi

HAS_PERFORMANCE=0
if Rscript "${CHECK_PKG_SCRIPT}" performance >/dev/null 2>&1; then
  HAS_PERFORMANCE=1
fi

HAS_DHARMA=0
if Rscript "${CHECK_PKG_SCRIPT}" DHARMa >/dev/null 2>&1; then
  HAS_DHARMA=1
fi

HAS_INFLUENCE=0
if Rscript "${CHECK_PKG_SCRIPT}" influence.ME >/dev/null 2>&1; then
  HAS_INFLUENCE=1
fi

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"

RDS_PATH="${TMP_BASE}/${DATASET_LABEL}.rds"
RDATA_PATH="${TMP_BASE}/${DATASET_LABEL}.RData"
RDATA_DF="golden_dataset"
SAV_PATH="${TMP_BASE}/${DATASET_LABEL}.sav"
CSV_SEMI_PATH="${TMP_BASE}/${DATASET_LABEL}.csv"
INTERACTIVE_INPUT="${TMP_BASE}/interactive_assumptions.txt"
HELP_PATH="${TMP_BASE}/assumptions_help.txt"

MIXED_LABEL="mixed_models_long"
MIXED_DATA_PATH="${TMP_BASE}/${MIXED_LABEL}.csv"
MIXED_DATASET_DIR="${WORKSPACE_DIR}/${MIXED_LABEL}"
MIXED_PARQUET_PATH="${MIXED_DATASET_DIR}/${MIXED_LABEL}.parquet"
MIXED_LOG_PATH="${MIXED_DATASET_DIR}/analysis_log.jsonl"
MIXED_NLSS_REPORT_PATH="${MIXED_DATASET_DIR}/report_canonical.md"

MAXFUN_DEFAULT="$(get_config_value modules.mixed_models.maxfun)"
if [ -z "${MAXFUN_DEFAULT}" ]; then
  MAXFUN_DEFAULT="100000"
fi

rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}" "${MIXED_NLSS_REPORT_PATH}" "${MIXED_LOG_PATH}"
cd "${WORKSPACE_DIR}"

run_ok "help text" bash -c "Rscript \"${ASSUMPTIONS_SCRIPT}\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Assumptions checks"

run_ok "prepare rds/rdata" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); saveRDS(df, \"${RDS_PATH}\"); golden_dataset <- df; save(golden_dataset, file = \"${RDATA_PATH}\")"

run_ok "prepare csv semicolon" Rscript -e "df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write.table(df, \"${CSV_SEMI_PATH}\", sep = ';', row.names = FALSE, col.names = TRUE)"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript -e "library(haven); df <- read.csv(\"${DATA_GOLDEN}\", stringsAsFactors = FALSE); write_sav(df, \"${SAV_PATH}\")"
else
  echo "[WARN] skipping SAV input tests (haven not installed)" | tee -a "${LOG_FILE}"
fi

cat >"${INTERACTIVE_INPUT}" <<EOF
csv
${DATA_GOLDEN}
,
TRUE
ttest
one-sample
x1
shapiro
levene
0.05
2

interactive assumptions prompt
TRUE
EOF

run_ok "init workspace (golden)" Rscript "${INIT_SCRIPT}" --csv "${DATA_GOLDEN}"
if [ ! -f "${PARQUET_PATH}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

run_ok "prepare mixed models data" Rscript "${PREP_SCRIPT}" "${DATA_GOLDEN}" "${MIXED_DATA_PATH}"
run_ok "init workspace (mixed models)" Rscript "${INIT_SCRIPT}" --csv "${MIXED_DATA_PATH}"
if [ ! -f "${MIXED_PARQUET_PATH}" ]; then
  echo "[FAIL] missing parquet copy: ${MIXED_PARQUET_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

start=$(log_count "${LOG_PATH}")
run_ok "ttest csv input" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --csv "${DATA_GOLDEN}" \
  --analysis ttest \
  --vars x1
check_log_main "${start}" ttest one_sample gt0 assumptions=Normality tests="Shapiro-Wilk" normality=shapiro

start=$(log_count "${LOG_PATH}")
run_ok "ttest csv semicolon" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --csv "${CSV_SEMI_PATH}" \
  --sep ";" \
  --header TRUE \
  --analysis ttest \
  --vars x1
check_log_main "${start}" ttest one_sample gt0 assumptions=Normality tests="Shapiro-Wilk"

start=$(log_count "${LOG_PATH}")
run_ok "ttest rds input" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --rds "${RDS_PATH}" \
  --analysis ttest \
  --vars x1
check_log_main "${start}" ttest one_sample gt0 assumptions=Normality tests="Shapiro-Wilk"

start=$(log_count "${LOG_PATH}")
run_ok "ttest rdata input" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --rdata "${RDATA_PATH}" \
  --df "${RDATA_DF}" \
  --analysis ttest \
  --vars x1
check_log_main "${start}" ttest one_sample gt0 assumptions=Normality tests="Shapiro-Wilk"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${LOG_PATH}")
  run_ok "ttest sav input" \
    Rscript "${ASSUMPTIONS_SCRIPT}" \
    --sav "${SAV_PATH}" \
    --analysis ttest \
    --vars x1
  check_log_main "${start}" ttest one_sample gt0 assumptions=Normality tests="Shapiro-Wilk"
fi

start=$(log_count "${LOG_PATH}")
run_ok "ttest independent homogeneity all" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --vars x1 \
  --group group2 \
  --homogeneity all
check_log_main "${start}" ttest independent gt0 \
  assumptions=Normality,Homogeneity \
  tests="Shapiro-Wilk,Levene (median),Bartlett,Fligner-Killeen,F-test" \
  homogeneity=levene,bartlett,fligner,f

start=$(log_count "${LOG_PATH}")
run_ok "ttest independent normality none" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --vars x1 \
  --group group2 \
  --normality none \
  --homogeneity levene
check_log_main "${start}" ttest independent gt0 \
  assumptions=Homogeneity \
  assumptions_absent=Normality \
  tests="Levene (median)" \
  tests_absent="Shapiro-Wilk" \
  normality=none

start=$(log_count "${LOG_PATH}")
run_ok "ttest paired" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --x pre_score \
  --y post_score
check_log_main "${start}" ttest paired gt0 \
  assumptions=Normality \
  tests="Shapiro-Wilk" \
  models=Paired

start=$(log_count "${LOG_PATH}")
run_ok "interactive ttest" \
  env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${ASSUMPTIONS_SCRIPT}" --interactive
check_log_main "${start}" ttest one_sample gt0 \
  assumptions=Normality tests="Shapiro-Wilk" user_prompt="interactive assumptions prompt"

start=$(log_count "${LOG_PATH}")
run_ok "anova between" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --dv outcome_anova \
  --between group3
check_log_main "${start}" anova between gt0 \
  assumptions=Normality,Homogeneity \
  tests="Shapiro-Wilk,Levene (median)" \
  models=Between

start=$(log_count "${LOG_PATH}")
run_ok "anova within" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --within pre_score,mid_score,post_score \
  --subject-id id
check_log_main "${start}" anova within gt0 \
  assumptions=Normality,Sphericity \
  tests="Shapiro-Wilk,Mauchly" \
  models=Within

start=$(log_count "${LOG_PATH}")
run_ok "anova mixed" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --within pre_score,mid_score,post_score \
  --between group3 \
  --subject-id id
check_log_main "${start}" anova mixed gt0 \
  assumptions=Normality,Homogeneity,Sphericity \
  tests="Shapiro-Wilk,Levene (median),Mauchly" \
  models=Mixed

start=$(log_count "${LOG_PATH}")
run_ok "anova within (no sphericity)" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --within pre_score,post_score
check_log_main "${start}" anova within gt0 \
  assumptions=Normality \
  assumptions_absent=Sphericity \
  tests="Shapiro-Wilk" \
  tests_absent="Mauchly"

start=$(log_count "${LOG_PATH}")
run_ok "regression ivs" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --ivs x1,x2,x3
check_log_main "${start}" regression - gt0 \
  assumptions=Normality,Linearity,Homoscedasticity,Independence,Outliers,Influence,Multicollinearity \
  tests="Shapiro-Wilk,Residual correlation,Breusch-Pagan,Durbin-Watson,Std. residuals,Cook's distance,VIF"

start=$(log_count "${LOG_PATH}")
run_ok "regression blocks" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --blocks "x1,x2;x3"
check_log_main "${start}" regression - gt0 \
  models="Block 2"

start=$(log_count "${LOG_PATH}")
run_ok "regression toggles off" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --ivs x1,x2 \
  --linearity FALSE \
  --homoscedasticity FALSE \
  --vif FALSE \
  --durbin-watson FALSE \
  --outliers FALSE \
  --influence FALSE
check_log_main "${start}" regression - gt0 \
  assumptions=Normality \
  assumptions_absent=Linearity,Homoscedasticity,Independence,Outliers,Influence,Multicollinearity \
  tests="Shapiro-Wilk" \
  tests_absent="Residual correlation,Breusch-Pagan,Durbin-Watson,Std. residuals,Cook's distance,VIF" \
  linearity=false homoscedasticity=false vif=false durbin_watson=false outliers=false influence=false

start=$(log_count "${LOG_PATH}")
run_ok "regression thresholds" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --ivs x1,x2 \
  --vif-warn 4 \
  --vif-high 8 \
  --outlier-z 2.5 \
  --cook-multiplier 5 \
  --alpha 0.01 \
  --digits 3 \
  --max-shapiro-n 10 \
  --user-prompt "assumptions regression prompt"
check_log_main "${start}" regression - gt0 \
  alpha=0.01 digits=3 max_shapiro_n=10 \
  vif_warn=4 vif_high=8 outlier_z=2.5 cook_multiplier=5 \
  user_prompt="assumptions regression prompt"

start=$(log_count "${LOG_PATH}")
run_ok "auto analysis (regression)" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --dv outcome_reg \
  --ivs x1,x2
check_log_main "${start}" regression - gt0

start=$(log_count "${MIXED_LOG_PATH}")
run_ok "mixed models formula" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${MIXED_PARQUET_PATH}" \
  --analysis mixed_models \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --reml TRUE \
  --maxfun -10
check_log_mixed "${start}" mixed_models - gt0 \
  assumptions="Convergence,Singularity,Normality,Random-effects normality,Homoscedasticity,Outliers" \
  reml=true maxfun="${MAXFUN_DEFAULT}"

start=$(log_count "${MIXED_LOG_PATH}")
run_ok "mixed models toggles off" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${MIXED_PARQUET_PATH}" \
  --analysis mixed_models \
  --dv score \
  --fixed time,group3,x1 \
  --random "1|id" \
  --random-effects FALSE \
  --singular FALSE \
  --convergence FALSE \
  --homoscedasticity FALSE \
  --outliers FALSE \
  --influence FALSE \
  --dharma FALSE \
  --performance FALSE \
  --reml FALSE \
  --optimizer nloptwrap \
  --maxfun 5000
check_log_mixed "${start}" mixed_models - gt0 \
  assumptions=Normality \
  assumptions_absent="Convergence,Singularity,Random-effects normality,Homoscedasticity,Outliers,Influence" \
  random_effects=false singular=false convergence=false homoscedasticity=false outliers=false influence=false \
  reml=false optimizer=nloptwrap maxfun=5000

start=$(log_count "${LOG_PATH}")
run_ok "sem cfa factors" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis cfa \
  --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev" \
  --ordered f1_1,f1_2 \
  --estimator WLSMV \
  --missing listwise \
  --se standard \
  --ci standard \
  --std std.all
check_log_main "${start}" sem - gt0 \
  assumptions=Normality,Multicollinearity,Convergence,Heywood,Outliers \
  sem_type=cfa estimator=WLSMV missing=listwise se=standard ci=standard std=std.all

if [ "${HAS_MVN}" -eq 1 ]; then
  check_log_main "${start}" sem - gt0 assumptions="Multivariate normality"
else
  check_log_main "${start}" sem - gt0 assumptions_absent="Multivariate normality"
fi

start=$(log_count "${LOG_PATH}")
run_ok "sem path bootstrap" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis path \
  --dv outcome_reg \
  --ivs x1,x2 \
  --estimator ML \
  --bootstrap TRUE \
  --bootstrap-samples 30 \
  --ci bca \
  --std none
check_log_main "${start}" sem - gt0 \
  assumptions=Normality,Convergence \
  sem_type=path estimator=ML bootstrap=true bootstrap_samples=30 se=bootstrap ci=bca std=none

start=$(log_count "${LOG_PATH}")
run_ok "sem mediation serial" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis mediation \
  --x x1 \
  --m mediator,x2 \
  --y outcome_reg \
  --serial TRUE \
  --covariates age
check_log_main "${start}" sem - gt0 \
  sem_type=mediation

start=$(log_count "${LOG_PATH}")
run_ok "sem invariance model" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis invariance \
  --model "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4"
check_log_main "${start}" sem - gt0 \
  sem_type=invariance

MODEL_FILE="${TMP_BASE}/assumptions_model.txt"
cat >"${MODEL_FILE}" <<'EOF'
outcome_reg ~ x1 + x2
EOF

start=$(log_count "${LOG_PATH}")
run_ok "sem model file" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis sem \
  --model-file "${MODEL_FILE}" \
  --group group2
check_log_main "${start}" sem - gt0 \
  sem_type=sem assumptions=Normality

start=$(log_count "${LOG_PATH}")
run_ok "sem collinearity off" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis sem \
  --model "outcome_reg ~ x1 + x2" \
  --collinearity FALSE \
  --mahalanobis FALSE \
  --mardia FALSE \
  --heywood FALSE \
  --convergence FALSE
check_log_main "${start}" sem - gt0 \
  assumptions=Normality \
  assumptions_absent="Multicollinearity,Outliers,Heywood,Convergence,Multivariate normality" \
  collinearity=false mahalanobis=false mardia=false heywood=false convergence=false

before_log=$(log_count "${LOG_PATH}")
run_ok "log disabled" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --vars x1 \
  --log FALSE
after_log=$(log_count "${LOG_PATH}")
assert_log_unchanged "${before_log}" "${after_log}" "log disabled"

TEMPLATE_TTEST_ORIG="$(get_config_value templates.assumptions.ttest)"
TEMPLATE_ANOVA_ORIG="$(get_config_value templates.assumptions.anova)"
TEMPLATE_REG_ORIG="$(get_config_value templates.assumptions.regression)"
TEMPLATE_MM_ORIG="$(get_config_value templates.assumptions.mixed_models)"
TEMPLATE_SEM_ORIG="$(get_config_value templates.assumptions.sem)"

TEMPLATE_TTEST_TMP="${TMP_BASE}/assumptions_ttest_template.md"
TEMPLATE_ANOVA_TMP="${TMP_BASE}/assumptions_anova_template.md"
TEMPLATE_REG_TMP="${TMP_BASE}/assumptions_reg_template.md"
TEMPLATE_MM_TMP="${TMP_BASE}/assumptions_mm_template.md"
TEMPLATE_SEM_TMP="${TMP_BASE}/assumptions_sem_template.md"

cp "$(resolve_template_source "${TEMPLATE_TTEST_ORIG}")" "${TEMPLATE_TTEST_TMP}"
cp "$(resolve_template_source "${TEMPLATE_ANOVA_ORIG}")" "${TEMPLATE_ANOVA_TMP}"
cp "$(resolve_template_source "${TEMPLATE_REG_ORIG}")" "${TEMPLATE_REG_TMP}"
cp "$(resolve_template_source "${TEMPLATE_MM_ORIG}")" "${TEMPLATE_MM_TMP}"
cp "$(resolve_template_source "${TEMPLATE_SEM_ORIG}")" "${TEMPLATE_SEM_TMP}"

printf "\n\nASSUMPTIONS_TEMPLATE_TTEST\n" >>"${TEMPLATE_TTEST_TMP}"
printf "\n\nASSUMPTIONS_TEMPLATE_ANOVA\n" >>"${TEMPLATE_ANOVA_TMP}"
printf "\n\nASSUMPTIONS_TEMPLATE_REGRESSION\n" >>"${TEMPLATE_REG_TMP}"
printf "\n\nASSUMPTIONS_TEMPLATE_MIXED\n" >>"${TEMPLATE_MM_TMP}"
printf "\n\nASSUMPTIONS_TEMPLATE_SEM\n" >>"${TEMPLATE_SEM_TMP}"

set_config_value templates.assumptions.ttest "${TEMPLATE_TTEST_TMP}"
set_config_value templates.assumptions.anova "${TEMPLATE_ANOVA_TMP}"
set_config_value templates.assumptions.regression "${TEMPLATE_REG_TMP}"
set_config_value templates.assumptions.mixed_models "${TEMPLATE_MM_TMP}"
set_config_value templates.assumptions.sem "${TEMPLATE_SEM_TMP}"

rm -f "${NLSS_REPORT_PATH}"
run_ok "template override ttest" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --vars x1
assert_contains "${NLSS_REPORT_PATH}" "ASSUMPTIONS_TEMPLATE_TTEST"

rm -f "${NLSS_REPORT_PATH}"
run_ok "template override anova" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --dv outcome_anova \
  --between group3
assert_contains "${NLSS_REPORT_PATH}" "ASSUMPTIONS_TEMPLATE_ANOVA"

rm -f "${NLSS_REPORT_PATH}"
run_ok "template override regression" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --ivs x1,x2
assert_contains "${NLSS_REPORT_PATH}" "ASSUMPTIONS_TEMPLATE_REGRESSION"

rm -f "${MIXED_NLSS_REPORT_PATH}"
run_ok "template override mixed models" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${MIXED_PARQUET_PATH}" \
  --analysis mixed_models \
  --formula "score ~ time + group3 + x1 + (1|id)"
assert_contains "${MIXED_NLSS_REPORT_PATH}" "ASSUMPTIONS_TEMPLATE_MIXED"

rm -f "${NLSS_REPORT_PATH}"
run_ok "template override sem" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis cfa \
  --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev"
assert_contains "${NLSS_REPORT_PATH}" "ASSUMPTIONS_TEMPLATE_SEM"

run_expect_fail "ttest paired with group" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --x pre_score \
  --y post_score \
  --group group2

run_expect_fail "ttest paired mismatched lengths" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --x pre_score \
  --y post_score,mid_score

run_expect_fail "ttest group not two levels" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis ttest \
  --vars x1 \
  --group group3

run_expect_fail "anova nonnumeric dv" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --dv gender \
  --between group3

run_expect_fail "anova unknown within" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis anova \
  --within missing_var

run_expect_fail "regression missing ivs" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg

run_expect_fail "regression unknown variable" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis regression \
  --dv outcome_reg \
  --ivs not_a_var

run_expect_fail "mixed models missing random" "${MIXED_LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${MIXED_PARQUET_PATH}" \
  --analysis mixed_models \
  --dv score \
  --fixed time

run_expect_fail "mixed models invalid formula" "${MIXED_LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${MIXED_PARQUET_PATH}" \
  --analysis mixed_models \
  --formula "score ~ time + (1|id"

run_expect_fail "sem missing model" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis sem

run_expect_fail "sem unknown ordered var" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis cfa \
  --factors "F1=f1_1,f1_2,f1_3_rev,f1_4" \
  --ordered not_a_var

run_expect_fail "sem unknown model var" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis sem \
  --model "outcome_reg ~ not_a_var"

run_expect_fail "sem serial needs two mediators" "${LOG_PATH}" \
  Rscript "${ASSUMPTIONS_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --analysis mediation \
  --x x1 \
  --m mediator \
  --y outcome_reg \
  --serial TRUE

echo "[DONE] assumptions deliberate tests finished" | tee -a "${LOG_FILE}"
