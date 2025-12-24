#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
R_SCRIPT_DIR="${ROOT_DIR}/core-stats/scripts/R"
MIXED_SCRIPT="${R_SCRIPT_DIR}/mixed_models.R"
PREP_SCRIPT="${ROOT_DIR}/outputs/tests/mixed_models_prep.R"
CHECK_PKG_SCRIPT="${ROOT_DIR}/outputs/tests/check_r_package.R"
TEMPLATE_DEFAULT="${ROOT_DIR}/core-stats/assets/mixed-models/default-template.md"
TEMPLATE_EMMEANS="${ROOT_DIR}/core-stats/assets/mixed-models/emmeans-template.md"

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

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="core-stats-workspace.yml"
fi

DATA_DIR_CFG="$(get_config_value tests.data_dir)"
if [ -z "${DATA_DIR_CFG}" ]; then
  DATA_DIR_CFG="./outputs/tests"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_GOLDEN_CFG="$(get_config_value tests.golden_dataset)"
if [ -z "${DATA_GOLDEN_CFG}" ]; then
  DATA_GOLDEN_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_GOLDEN="$(to_abs_path "${DATA_GOLDEN_CFG}")"

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

cleanup_runs() {
  local keep="${CORE_STATS_KEEP_RUNS:-10}"
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

WORKSPACE_DIR="${RUN_ROOT}/mixed_models_template_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/mixed_models_templates"
LOG_FILE="${RUN_ROOT}/mixed_models_template_tests.log"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"

export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
cd "${WORKSPACE_DIR}"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R or use scripts/run_rscript.ps1 on Windows."
  exit 1
fi

if [[ ! -f "${MIXED_SCRIPT}" ]]; then
  echo "Missing script: ${MIXED_SCRIPT}"
  exit 1
fi

if [[ ! -f "${TEMPLATE_DEFAULT}" ]]; then
  echo "Missing template: ${TEMPLATE_DEFAULT}"
  exit 1
fi

if [[ ! -f "${TEMPLATE_EMMEANS}" ]]; then
  echo "Missing template: ${TEMPLATE_EMMEANS}"
  exit 1
fi

if ! Rscript "${CHECK_PKG_SCRIPT}" lme4 >/dev/null 2>&1; then
  echo "[FAIL] mixed models template tests require the lme4 package."
  exit 1
fi

HAS_EMMEANS=0
if Rscript "${CHECK_PKG_SCRIPT}" emmeans >/dev/null 2>&1; then
  HAS_EMMEANS=1
fi

DEFAULT_BAK="${TMP_BASE}/default-template-backup.md"
EMMEANS_BAK="${TMP_BASE}/emmeans-template-backup.md"

cleanup() {
  rm -f "${WORKSPACE_MANIFEST_PATH}"
  if [[ -f "${DEFAULT_BAK}" ]]; then
    mv -f "${DEFAULT_BAK}" "${TEMPLATE_DEFAULT}"
  fi
  if [[ -f "${EMMEANS_BAK}" ]]; then
    mv -f "${EMMEANS_BAK}" "${TEMPLATE_EMMEANS}"
  fi
  cleanup_runs
}
trap cleanup EXIT

MIXED_DATA_PATH="${TMP_BASE}/mixed_models_long.csv"
Rscript "${PREP_SCRIPT}" "${DATA_GOLDEN}" "${MIXED_DATA_PATH}" >>"${LOG_FILE}" 2>&1
Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${MIXED_DATA_PATH}" >>"${LOG_FILE}" 2>&1

DATASET_LABEL="mixed_models_long"
PARQUET_PATH="${WORKSPACE_DIR}/${DATASET_LABEL}/${DATASET_LABEL}.parquet"
APA_REPORT_PATH="${WORKSPACE_DIR}/${DATASET_LABEL}/apa_report.md"

reset_report() {
  rm -f "${APA_REPORT_PATH}"
}

run_mixed() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

reset_report
run_mixed "mixed_models template baseline" \
  Rscript "${MIXED_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --log FALSE

if [[ ! -f "${APA_REPORT_PATH}" ]]; then
  echo "Missing report: ${APA_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

assert_contains "${APA_REPORT_PATH}" "Mixed Models"
assert_contains "${APA_REPORT_PATH}" "Fixed effects estimates."
assert_contains "${APA_REPORT_PATH}" "**Narrative**"

cp "${TEMPLATE_DEFAULT}" "${DEFAULT_BAK}"
sed -i 's/title: "Mixed Models"/title: "Mixed Models TEMPLATE TEST"/' "${TEMPLATE_DEFAULT}"
sed -i 's/table_title: "Fixed effects estimates\."/table_title: "Fixed effects estimates TEST."/' "${TEMPLATE_DEFAULT}"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_DEFAULT}"
sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_DEFAULT}"

reset_report
run_mixed "mixed_models template modified" \
  Rscript "${MIXED_SCRIPT}" \
  --parquet "${PARQUET_PATH}" \
  --formula "score ~ time + group3 + x1 + (1|id)" \
  --log FALSE

assert_contains "${APA_REPORT_PATH}" "Mixed Models TEMPLATE TEST"
assert_contains "${APA_REPORT_PATH}" "Fixed effects estimates TEST."
assert_contains "${APA_REPORT_PATH}" "*Note-TEST.*"
assert_contains "${APA_REPORT_PATH}" "**Narrative TEST**"

mv -f "${DEFAULT_BAK}" "${TEMPLATE_DEFAULT}"

if [ "${HAS_EMMEANS}" -eq 1 ]; then
  reset_report
  run_mixed "mixed_models emmeans template baseline" \
    Rscript "${MIXED_SCRIPT}" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time * group3 + x1 + (1|id)" \
    --emmeans "time*group3" \
    --contrasts pairwise \
    --p-adjust holm \
    --log FALSE

  assert_contains "${APA_REPORT_PATH}" "Mixed Models: Marginal Means"
  assert_contains "${APA_REPORT_PATH}" "Estimated marginal means and contrasts."

  cp "${TEMPLATE_EMMEANS}" "${EMMEANS_BAK}"
  sed -i 's/title: "Mixed Models: Marginal Means"/title: "Mixed Models: Marginal Means TEMPLATE TEST"/' "${TEMPLATE_EMMEANS}"
  sed -i 's/table_title: "Estimated marginal means and contrasts\."/table_title: "Estimated marginal means TEST."/' "${TEMPLATE_EMMEANS}"
  sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "${TEMPLATE_EMMEANS}"
  sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "${TEMPLATE_EMMEANS}"

  reset_report
  run_mixed "mixed_models emmeans template modified" \
    Rscript "${MIXED_SCRIPT}" \
    --parquet "${PARQUET_PATH}" \
    --formula "score ~ time * group3 + x1 + (1|id)" \
    --emmeans "time*group3" \
    --contrasts pairwise \
    --p-adjust holm \
    --log FALSE

  assert_contains "${APA_REPORT_PATH}" "Mixed Models: Marginal Means TEMPLATE TEST"
  assert_contains "${APA_REPORT_PATH}" "Estimated marginal means TEST."
  assert_contains "${APA_REPORT_PATH}" "*Note-TEST.*"
  assert_contains "${APA_REPORT_PATH}" "**Narrative TEST**"

  mv -f "${EMMEANS_BAK}" "${TEMPLATE_EMMEANS}"
else
  echo "[WARN] skipping emmeans template tests (emmeans not installed)" | tee -a "${LOG_FILE}"
fi

echo "mixed_models template tests: OK" | tee -a "${LOG_FILE}"
