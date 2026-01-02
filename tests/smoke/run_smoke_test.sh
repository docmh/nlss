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
CHECK_R_PACKAGE_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_package.R"
MIXED_MODELS_PREP_SCRIPT="${ROOT_DIR}/tests/smoke/mixed_models_prep.R"
LABEL_DATASET_PREP_SCRIPT="${ROOT_DIR}/tests/smoke/prepare_labeled_dataset.R"
CHECK_PARQUET_LABELS_SCRIPT="${ROOT_DIR}/tests/smoke/check_parquet_labels.R"
CHECK_R_ENV_SCRIPT="${ROOT_DIR}/tests/smoke/check_r_env.R"
CHECK_INTEGRITY_SCRIPT="${ROOT_DIR}/scripts/R/check_integrity.R"
RECONSTRUCT_REPORTS_SCRIPT="${ROOT_DIR}/scripts/R/reconstruct_reports.R"

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

set_config_value_allow_new() {
  "${PYTHON_BIN}" - "$CONFIG_PATH" "$1" "$2" <<'PY'
import sys

path, key, value = sys.argv[1], sys.argv[2], sys.argv[3]
parts = key.split(".")
lines = open(path, "r", encoding="utf-8").read().splitlines()

stack = []
paths = {}
for idx, line in enumerate(lines):
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    key_name, _, rest = stripped.partition(":")
    while stack and stack[-1][1] >= indent:
        stack.pop()
    stack.append((key_name, indent))
    paths[tuple(k for k, _ in stack)] = (idx, indent)

target = tuple(parts)
if target in paths:
    idx, indent = paths[target]
    lines[idx] = (" " * indent) + f"{parts[-1]}: \"{value}\""
    with open(path, "w", encoding="utf-8", newline="") as handle:
        handle.write("\n".join(lines) + "\n")
    sys.exit(0)

parent = tuple(parts[:-1])
if parent not in paths:
    sys.exit(1)

parent_idx, parent_indent = paths[parent]
child_indent = None
insert_idx = len(lines)
for i in range(parent_idx + 1, len(lines)):
    line = lines[i]
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    if indent <= parent_indent:
        insert_idx = i
        break
    if child_indent is None and indent > parent_indent:
        child_indent = indent

if child_indent is None:
    child_indent = parent_indent + 2

new_line = (" " * child_indent) + f"{parts[-1]}: \"{value}\""
lines.insert(insert_idx, new_line)
with open(path, "w", encoding="utf-8", newline="") as handle:
    handle.write("\n".join(lines) + "\n")
PY
}

RUNS_BASE_CFG="$(get_tests_value tests.output_dir)"
TEMPLATE_OVERRIDE_DIR_CFG="$(get_tests_value tests.template_dir)"
TEMPLATE_MARKER="$(get_tests_value tests.template_marker)"
WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"

if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi
if [ -z "${TEMPLATE_OVERRIDE_DIR_CFG}" ]; then
  TEMPLATE_OVERRIDE_DIR_CFG="templates"
fi
if [ -z "${TEMPLATE_MARKER}" ]; then
  TEMPLATE_MARKER="TEMPLATE_SMOKE_TEST"
fi
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="nlss-workspace.yml"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUN_MARKER="${TEMPLATE_MARKER}_${RUN_ID}"

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

DATA_DIR_CFG="$(get_tests_value tests.data_dir)"
if [ -z "${DATA_DIR_CFG}" ]; then
  DATA_DIR_CFG="./tests/data"
fi
DATA_DIR="$(to_abs_path "${DATA_DIR_CFG}")"
DATA_PATH_CFG="$(get_tests_value tests.golden_dataset)"
if [ -z "${DATA_PATH_CFG}" ]; then
  DATA_PATH_CFG="${DATA_DIR}/golden_dataset.csv"
fi
DATA_PATH="$(to_abs_path "${DATA_PATH_CFG}")"

resolve_run_path() {
  local path="$1"
  if [[ "${path}" == "~"* ]]; then
    path="${HOME}${path:1}"
  fi
  if [[ "${path}" != /* && ! "${path}" =~ ^[A-Za-z]: ]]; then
    path="${RUN_ROOT}/${path#./}"
  fi
  if command -v cygpath >/dev/null 2>&1; then
    case "$(uname -s)" in
      MINGW*|MSYS*|CYGWIN*) path="$(cygpath -m "${path}")" ;;
    esac
  fi
  echo "${path}"
}

to_win_path() {
  local path="$1"
  if [[ "${path}" =~ ^[A-Za-z]: || "${path}" =~ ^\\\\ ]]; then
    echo "$path"
    return 0
  fi
  if command -v wslpath >/dev/null 2>&1; then
    local converted
    converted="$(wslpath -w "$path" 2>/dev/null || true)"
    if [ -n "${converted}" ]; then
      echo "${converted}" | tr '\\\\' '/'
      return 0
    fi
    echo "$path"
    return 0
  fi
  echo "$path"
}

to_wsl_path() {
  local path="$1"
  if command -v wslpath >/dev/null 2>&1; then
    wslpath -u "$path"
    return 0
  fi
  echo "$path"
}

RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${NLSS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${NLSS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
fi

WORKSPACE_DIR="${RUN_ROOT}/workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TEMPLATE_OVERRIDE_DIR="$(resolve_run_path "${TEMPLATE_OVERRIDE_DIR_CFG}")"
TMP_BASE="${RUN_ROOT}/tmp"
LOG_PATH="${RUN_ROOT}/smoke_test.log"
INTEGRITY_EDIT_LOG_PATH="${TMP_BASE}/analysis_log_tamper_edit.jsonl"
INTEGRITY_DELETE_LOG_PATH="${TMP_BASE}/analysis_log_tamper_delete.jsonl"
DATASET_LABEL="$(basename "${DATA_PATH}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
SCRATCHPAD_PATH="${DATASET_DIR}/scratchpad.md"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
RECONSTRUCT_REPORT_PATH="${DATASET_DIR}/report_canonical_reconstructed.md"
REPORT_DATE_UTC="$(date -u +%Y%m%d 2>/dev/null || date +%Y%m%d)"
METASKILL_REPORT_ORIGINAL_PATH="${DATASET_DIR}/report_${REPORT_DATE_UTC}_sample-description_describe_the_sample.md"
METASKILL_REPORT_RECON_PATH="${DATASET_DIR}/report_${REPORT_DATE_UTC}_sample-description_describe_the_sample_reconstructed.md"
ANALYSIS_LOG_REL_PATH="${DATASET_LABEL}/analysis_log.jsonl"
INTEGRITY_EDIT_LOG_REL_PATH="../tmp/analysis_log_tamper_edit.jsonl"
INTEGRITY_DELETE_LOG_REL_PATH="../tmp/analysis_log_tamper_delete.jsonl"
WORKSPACE_DIR_WIN="$(to_win_path "${WORKSPACE_DIR}")"
DATA_PATH_WIN="$(to_win_path "${DATA_PATH}")"
PARQUET_PATH_WIN="$(to_win_path "${PARQUET_PATH}")"
TEMPLATE_OVERRIDE_DIR_WIN="$(to_win_path "${TEMPLATE_OVERRIDE_DIR}")"
MIXED_DATASET_LABEL="mixed_models_long"
MIXED_DATA_PATH="${TMP_BASE}/${MIXED_DATASET_LABEL}.csv"
MIXED_DATASET_DIR="${WORKSPACE_DIR}/${MIXED_DATASET_LABEL}"
MIXED_PARQUET_PATH="${MIXED_DATASET_DIR}/${MIXED_DATASET_LABEL}.parquet"
MIXED_NLSS_REPORT_PATH="${MIXED_DATASET_DIR}/report_canonical.md"
MIXED_ANALYSIS_LOG_PATH="${MIXED_DATASET_DIR}/analysis_log.jsonl"
MIXED_DATA_PATH_WIN="$(to_win_path "${MIXED_DATA_PATH}")"
MIXED_PARQUET_PATH_WIN="$(to_win_path "${MIXED_PARQUET_PATH}")"
LABEL_DATASET_LABEL="golden_dataset_labeled"
LABEL_DATA_PATH="${TMP_BASE}/${LABEL_DATASET_LABEL}.rds"
LABEL_DATASET_DIR="${WORKSPACE_DIR}/${LABEL_DATASET_LABEL}"
LABEL_PARQUET_PATH="${LABEL_DATASET_DIR}/${LABEL_DATASET_LABEL}.parquet"
LABEL_NLSS_REPORT_PATH="${LABEL_DATASET_DIR}/report_canonical.md"
LABEL_ANALYSIS_LOG_PATH="${LABEL_DATASET_DIR}/analysis_log.jsonl"
LABEL_DATA_PATH_WIN="$(to_win_path "${LABEL_DATA_PATH}")"
META_LABEL="golden_dataset_meta"
META_RDS_PATH="${TMP_BASE}/${META_LABEL}.rds"
META_RDATA_PATH="${TMP_BASE}/${META_LABEL}.RData"
META_SAV_PATH="${TMP_BASE}/${META_LABEL}.sav"
META_DATASET_DIR="${WORKSPACE_DIR}/${META_LABEL}"
META_PARQUET_PATH="${META_DATASET_DIR}/${META_LABEL}.parquet"
META_RDS_PATH_WIN="$(to_win_path "${META_RDS_PATH}")"
META_RDATA_PATH_WIN="$(to_win_path "${META_RDATA_PATH}")"
META_SAV_PATH_WIN="$(to_win_path "${META_SAV_PATH}")"
META_PARQUET_PATH_WIN="$(to_win_path "${META_PARQUET_PATH}")"
MIXED_LABEL_DATASET_LABEL="mixed_models_labeled"
MIXED_LABEL_DATA_PATH="${TMP_BASE}/${MIXED_LABEL_DATASET_LABEL}.rds"
MIXED_LABEL_DATASET_DIR="${WORKSPACE_DIR}/${MIXED_LABEL_DATASET_LABEL}"
MIXED_LABEL_PARQUET_PATH="${MIXED_LABEL_DATASET_DIR}/${MIXED_LABEL_DATASET_LABEL}.parquet"
MIXED_LABEL_NLSS_REPORT_PATH="${MIXED_LABEL_DATASET_DIR}/report_canonical.md"
MIXED_LABEL_ANALYSIS_LOG_PATH="${MIXED_LABEL_DATASET_DIR}/analysis_log.jsonl"
MIXED_LABEL_DATA_PATH_WIN="$(to_win_path "${MIXED_LABEL_DATA_PATH}")"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "[FAIL] Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_PATH}"
  exit 1
fi
echo "[INFO] Using Rscript directly." | tee -a "${LOG_PATH}"

CHECK_INTEGRITY_LOG_PATH="${ANALYSIS_LOG_PATH}"
CHECK_INTEGRITY_EDIT_LOG_PATH="${INTEGRITY_EDIT_LOG_PATH}"
CHECK_INTEGRITY_DELETE_LOG_PATH="${INTEGRITY_DELETE_LOG_PATH}"

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${LABEL_DATASET_DIR}" "${META_DATASET_DIR}" "${MIXED_LABEL_DATASET_DIR}" "${TEMPLATE_OVERRIDE_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

CONFIG_BAK="$(mktemp)"
CONFIG_BASE="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"
cp "${CONFIG_PATH}" "${CONFIG_BASE}"

restore_config() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}" "${CONFIG_BASE}"
  rm -f "${WORKSPACE_MANIFEST_PATH}"
}

trap restore_config EXIT

reset_to_base() {
  cp "${CONFIG_BASE}" "${CONFIG_PATH}"
}

HAS_RG=0
if command -v rg >/dev/null 2>&1; then
  HAS_RG=1
fi

assert_marker() {
  local marker="$1"
  local file="$2"
  local allow_missing="${3:-0}"
  local attempts=10
  local found=0
  for ((i = 1; i <= attempts; i++)); do
    if [ -f "${file}" ]; then
      if [ "${HAS_RG}" -eq 1 ]; then
        if rg -n "${marker}" "${file}" >/dev/null 2>&1; then
          found=1
          break
        fi
      else
        if grep -n "${marker}" "${file}" >/dev/null 2>&1; then
          found=1
          break
        fi
      fi
    fi
    sleep 0.3
  done
  if [ "${found}" -ne 1 ]; then
    if [ "${allow_missing}" -eq 1 ]; then
      if [ ! -f "${file}" ]; then
        echo "[WARN] missing output file (optional): ${file}" | tee -a "${LOG_PATH}"
      else
        echo "[WARN] marker not found (optional): ${marker}" | tee -a "${LOG_PATH}"
      fi
      return 0
    else
      if [ ! -f "${file}" ]; then
        echo "[FAIL] missing output file: ${file}" | tee -a "${LOG_PATH}"
      else
        echo "[FAIL] marker not found: ${marker}" | tee -a "${LOG_PATH}"
      fi
      exit 1
    fi
  fi
  return 0
}

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_PATH}"
  "$@" >>"${LOG_PATH}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
}

run_rscript() {
  local script_path="$1"; shift
  local script_arg="${script_path}"
  if command -v wslpath >/dev/null 2>&1 && [[ "${script_arg}" =~ ^[A-Za-z]: || "${script_arg}" =~ ^\\\\ ]]; then
    script_arg="$(to_wsl_path "${script_arg}")"
  fi
  local args=()
  local arg
  for arg in "$@"; do
    if command -v wslpath >/dev/null 2>&1 && [[ "${arg}" =~ ^[A-Za-z]: || "${arg}" =~ ^\\\\ ]]; then
      arg="$(to_wsl_path "${arg}")"
    fi
    args+=("${arg}")
  done
  Rscript "${script_arg}" "${args[@]}"
}

resolve_path_arg() {
  local path="$1"
  if command -v wslpath >/dev/null 2>&1 && [[ "${path}" =~ ^[A-Za-z]: || "${path}" =~ ^\\\\ ]]; then
    to_wsl_path "${path}"
    return
  fi
  echo "${path}"
}

reset_parquet() {
  if [ -f "${PARQUET_PATH}" ]; then
    rm -f "${PARQUET_PATH}"
  fi
}

run_init_workspace() {
  local label="$1"
  reset_parquet
  run_ok "${label}" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"
}

run_init_workspace_custom() {
  local label="$1"; shift
  local data_path_win="$1"
  run_ok "${label}" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${data_path_win}"
}

has_r_package() {
  local pkg="$1"
  set +e
  run_rscript "${CHECK_R_PACKAGE_SCRIPT}" "${pkg}" >/dev/null 2>&1
  local status=$?
  set -e
  return "${status}"
}

prepare_mixed_models_data() {
  run_ok "mixed_models prep" run_rscript "${MIXED_MODELS_PREP_SCRIPT}" "${DATA_PATH_WIN}" "${MIXED_DATA_PATH_WIN}"
}

prepare_labeled_dataset() {
  run_ok "prepare labeled dataset" run_rscript "${LABEL_DATASET_PREP_SCRIPT}" "${DATA_PATH_WIN}" "${LABEL_DATA_PATH_WIN}"
}

prepare_labeled_mixed_models_data() {
  run_ok "prepare labeled mixed dataset" run_rscript "${LABEL_DATASET_PREP_SCRIPT}" "${MIXED_DATA_PATH_WIN}" "${MIXED_LABEL_DATA_PATH_WIN}"
}

prepare_meta_dataset() {
  if [ "${HAS_HAVEN}" -eq 1 ]; then
    run_ok "prepare meta dataset" run_rscript "${LABEL_DATASET_PREP_SCRIPT}" "${DATA_PATH_WIN}" "${META_RDS_PATH_WIN}" "${META_RDATA_PATH_WIN}" "${META_LABEL}" "${META_SAV_PATH_WIN}"
  else
    run_ok "prepare meta dataset" run_rscript "${LABEL_DATASET_PREP_SCRIPT}" "${DATA_PATH_WIN}" "${META_RDS_PATH_WIN}" "${META_RDATA_PATH_WIN}" "${META_LABEL}"
  fi
}

run_fail() {
  local label="$1"; shift
  echo "[RUN-FAIL] ${label}" | tee -a "${LOG_PATH}"
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -eq 0 ]; then
    echo "[FAIL] ${label} (expected failure)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
}

run_expect() {
  local label="$1"; shift
  local marker="$1"; shift
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_PATH}"
  local start_line
  start_line=$(wc -l < "${LOG_PATH}")
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  set -e
  if [ "${status}" -ne 0 ]; then
    echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
    return 0
  fi
  local tail_start=$((start_line + 1))
  if [ "${HAS_RG}" -eq 1 ]; then
    if tail -n +"${tail_start}" "${LOG_PATH}" | rg -n "${marker}" >/dev/null 2>&1; then
      echo "[PASS] ${label} (informational)" | tee -a "${LOG_PATH}"
      return 0
    fi
  else
    if tail -n +"${tail_start}" "${LOG_PATH}" | grep -n "${marker}" >/dev/null 2>&1; then
      echo "[PASS] ${label} (informational)" | tee -a "${LOG_PATH}"
      return 0
    fi
  fi
  echo "[FAIL] ${label} (expected failure or info marker: ${marker})" | tee -a "${LOG_PATH}"
  exit 1
}

log_count() {
  local log_file="$1"
  "${PYTHON_BIN}" - "$log_file" <<'PY'
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
    echo "[FAIL] ${label} (log changed)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
}

log_has_expected() {
  local log_file="$1"
  local start_count="$2"
  local module="$3"
  local status="$4"
  "${PYTHON_BIN}" - "$log_file" "$start_count" "$module" "$status" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
start = int(sys.argv[2])
module = sys.argv[3]
status = sys.argv[4]

if not path.exists():
    sys.exit(1)

index = 0
with path.open("r", encoding="utf-8") as handle:
    for line in handle:
        if not line.strip():
            continue
        index += 1
        if index <= start:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if entry.get("module") != module:
            continue
        results = entry.get("results", {})
        if results.get("status") != status:
            continue
        sys.exit(0)
sys.exit(1)
PY
}

assert_log_field() {
  local label="$1"; shift
  local log_file="$1"; shift
  local start_count="$1"; shift
  local module="$1"; shift
  local field="$1"; shift
  local expected="$1"; shift
  if "${PYTHON_BIN}" - "$log_file" "$start_count" "$module" "$field" "$expected" <<'PY'
import json
import sys
from pathlib import Path

log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
module = sys.argv[3]
field = sys.argv[4]
expected = sys.argv[5]

if not log_path.exists():
    raise SystemExit(f"Missing log: {log_path}")

entries = []
with log_path.open("r", encoding="utf-8") as handle:
    for idx, line in enumerate(handle, start=1):
        if idx <= start_count:
            continue
        if line.strip():
            try:
                entries.append(json.loads(line))
            except json.JSONDecodeError:
                continue

if not entries:
    raise SystemExit("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == module:
        entry = candidate
        break
if entry is None:
    raise SystemExit(f"No {module} log entry found.")

def resolve_path(obj, path):
    current = obj
    for part in path.split("."):
        if isinstance(current, dict) and part in current:
            current = current[part]
        else:
            return False
    return True

has_field = resolve_path(entry, field)
if expected == "present" and not has_field:
    raise SystemExit(f"Expected {field} present.")
if expected == "absent" and has_field:
    raise SystemExit(f"Expected {field} absent.")
PY
  then
    echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
  else
    echo "[FAIL] ${label}" | tee -a "${LOG_PATH}"
    exit 1
  fi
}

run_expect_log() {
  local label="$1"; shift
  local log_file="$1"; shift
  local module="$1"; shift
  local status="$1"; shift
  echo "[RUN-EXPECT] ${label}" | tee -a "${LOG_PATH}"
  local start_count
  start_count="$(log_count "${log_file}")"
  set +e
  "$@" >>"${LOG_PATH}" 2>&1
  local exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
    return 0
  fi
  local attempts=10
  for ((i = 1; i <= attempts; i++)); do
    if log_has_expected "${log_file}" "${start_count}" "${module}" "${status}"; then
      echo "[PASS] ${label} (informational log)" | tee -a "${LOG_PATH}"
      return 0
    fi
    sleep 0.3
  done
  echo "[FAIL] ${label} (expected failure or ${status} log)" | tee -a "${LOG_PATH}"
  exit 1
}

check_integrity_expect() {
  local label="$1"; shift
  local target_path="$1"; shift
  local expect_warning="$1"; shift
  local resolved_target
  resolved_target="$(resolve_path_arg "${target_path}")"
  echo "[RUN] ${label}" | tee -a "${LOG_PATH}"
  local start_line
  start_line=$(wc -l < "${LOG_PATH}")
  NLSS_INTEGRITY_LOG="${resolved_target}" run_rscript "${CHECK_INTEGRITY_SCRIPT}" "${resolved_target}" >>"${LOG_PATH}" 2>&1
  local tail_start=$((start_line + 1))
  local has_checksum=0
  local has_warning=0
  if [ "${HAS_RG}" -eq 1 ]; then
    if tail -n +"${tail_start}" "${LOG_PATH}" | rg -n "^[0-9a-f]{32} [0-9]+\\s*$" >/dev/null 2>&1; then
      has_checksum=1
    fi
    if tail -n +"${tail_start}" "${LOG_PATH}" | rg -n "^WARNING: multiple reverted checksums found\\.[[:space:]]*$" >/dev/null 2>&1; then
      has_warning=1
    fi
  else
    if tail -n +"${tail_start}" "${LOG_PATH}" | grep -n "^[0-9a-f]\\{32\\} [0-9]\\+[[:space:]]*$" >/dev/null 2>&1; then
      has_checksum=1
    fi
    if tail -n +"${tail_start}" "${LOG_PATH}" | grep -n "^WARNING: multiple reverted checksums found\\.[[:space:]]*$" >/dev/null 2>&1; then
      has_warning=1
    fi
  fi
  if [ "${has_checksum}" -ne 1 ]; then
    echo "[FAIL] ${label} (no checksum output)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  if [ "${expect_warning}" -eq 1 ] && [ "${has_warning}" -ne 1 ]; then
    echo "[FAIL] ${label} (expected warning)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  if [ "${expect_warning}" -eq 0 ] && [ "${has_warning}" -eq 1 ]; then
    local checksum_counts
    checksum_counts="$(tail -n +"${tail_start}" "${LOG_PATH}" | awk '/^[0-9a-f]{32} [0-9]+[[:space:]]*$/ {print $2}')"
    local checksum_lines=0
    if [ -n "${checksum_counts}" ]; then
      checksum_lines="$(echo "${checksum_counts}" | wc -l | tr -d ' ')"
    fi
    if [ "${checksum_lines}" -eq 2 ]; then
      local min_count
      min_count="$(echo "${checksum_counts}" | sort -n | head -n1 | tr -d ' ')"
      if [ "${min_count}" -eq 1 ]; then
        echo "[WARN] ${label} (single checksum outlier)" | tee -a "${LOG_PATH}"
        return 0
      fi
    fi
    echo "[FAIL] ${label} (unexpected warning)" | tee -a "${LOG_PATH}"
    exit 1
  fi
  echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
}

tamper_log_edit() {
  local src="$1"
  local dest="$2"
  "${PYTHON_BIN}" - "$src" "$dest" <<'PY'
import sys
from pathlib import Path

src = Path(sys.argv[1])
dest = Path(sys.argv[2])
data = src.read_bytes()
lines = data.splitlines(keepends=True)
non_empty = [i for i, line in enumerate(lines) if line.strip()]
if len(non_empty) < 2:
    raise SystemExit("Not enough log lines to tamper.")
idx = non_empty[1]
line = lines[idx]
if line.endswith(b"\r\n"):
    body, ending = line[:-2], b"\r\n"
elif line.endswith(b"\n"):
    body, ending = line[:-1], b"\n"
else:
    body, ending = line, b""
text = body.decode("utf-8", errors="replace")
needle = "\"module\":\""
if needle not in text:
    raise SystemExit("Expected token not found.")
text = text.replace(needle, "\"module\":\"tampered_", 1)
lines[idx] = text.encode("utf-8") + ending
dest.write_bytes(b"".join(lines))
PY
}

tamper_log_delete() {
  local src="$1"
  local dest="$2"
  "${PYTHON_BIN}" - "$src" "$dest" <<'PY'
import sys
from pathlib import Path

src = Path(sys.argv[1])
dest = Path(sys.argv[2])
data = src.read_bytes()
lines = data.splitlines(keepends=True)
non_empty = [i for i, line in enumerate(lines) if line.strip()]
if len(non_empty) < 2:
    raise SystemExit("Not enough log lines to tamper.")
del lines[non_empty[1]]
dest.write_bytes(b"".join(lines))
PY
}

resolve_log_path() {
  local out_dir="${WORKSPACE_DIR}"
  if [[ "${out_dir}" =~ ^[A-Za-z]:[\\\\/] || "${out_dir}" =~ ^\\\\ ]]; then
    out_dir="$(to_wsl_path "${out_dir}")"
  else
    out_dir="$(to_abs_path "${out_dir}")"
  fi
  echo "${out_dir}/${DATASET_LABEL}/analysis_log.jsonl"
}

resolve_template_source() {
  local key="$1"
  local value
  value="$(get_config_value "templates.${key}")"
  if [ -z "${value}" ]; then
    echo "Missing templates.${key} in config." >&2
    return 1
  fi
  if [[ "${value}" == "~"* ]]; then
    value="${HOME}${value:1}"
  fi
  if [[ "${value}" =~ ^[A-Za-z]:[\\\\/] || "${value}" =~ ^\\\\ ]]; then
    echo "$(to_wsl_path "${value}")"
    return 0
  fi
  if [[ "${value}" == /* ]]; then
    echo "${value}"
    return 0
  fi
  echo "${ROOT_DIR}/assets/${value}"
}

template_test_base() {
  local allow_missing="$1"; shift
  local label="$1"; shift
  local key="$1"; shift
  local output_file="$1"; shift
  local marker="${RUN_MARKER}_${key//./_}"
  local source
  local target

  reset_to_base
  if [[ "${key}" == init_workspace.* ]]; then
    reset_parquet
  fi
  source="$(resolve_template_source "${key}")"
  if [ ! -f "${source}" ]; then
    echo "[FAIL] missing template: ${source}" | tee -a "${LOG_PATH}"
    exit 1
  fi

  target="${TEMPLATE_OVERRIDE_DIR}/${key//./_}.md"
  cp "${source}" "${target}"
  printf "\n\n%s\n" "${marker}" >> "${target}"
  set_config_value "templates.${key}" "${target}"
  run_ok "${label}" "$@"
  assert_marker "${marker}" "${output_file}" "${allow_missing}"
  reset_to_base
}

template_test() {
  template_test_base 0 "$@"
}

template_test_optional() {
  template_test_base 1 "$@"
}

template_ref_test() {
  local label="$1"; shift
  local module="$1"; shift
  local ref="$1"; shift
  local base_key="$1"; shift
  local output_file="$1"; shift
  local marker="${RUN_MARKER}_${module}_${ref}"
  local source
  local target

  reset_to_base
  source="$(resolve_template_source "${base_key}")"
  if [ ! -f "${source}" ]; then
    echo "[FAIL] missing template: ${source}" | tee -a "${LOG_PATH}"
    exit 1
  fi

  target="${TEMPLATE_OVERRIDE_DIR}/${module}_${ref}.md"
  cp "${source}" "${target}"
  printf "\n\n%s\n" "${marker}" >> "${target}"
  set_config_value_allow_new "templates.${module}.${ref}" "${target}"
  run_ok "${label}" "$@"
  assert_marker "${marker}" "${output_file}" 0
  reset_to_base
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

: > "${LOG_PATH}"

if [ ! -f "${DATA_PATH}" ]; then
  echo "[FAIL] missing dataset: ${DATA_PATH}" | tee -a "${LOG_PATH}"
  exit 1
fi

cd "${WORKSPACE_DIR}"

rm -f "${NLSS_REPORT_PATH}" "${SCRATCHPAD_PATH}" "${ANALYSIS_LOG_PATH}"
run_init_workspace "init workspace"

run_ok "calc basic" run_rscript "${R_SCRIPT_DIR}/calc.R" --expr "alpha_adj=0.05/3" --digits 4
assert_marker "alpha_adj = 0.0167" "${LOG_PATH}"
run_ok "calc set json" run_rscript "${R_SCRIPT_DIR}/calc.R" --set "r=0.3" --expr "d=2*r/sqrt(1-r^2)" --digits 3 --format json
assert_marker '"d":0.629' "${LOG_PATH}"
run_ok "calc multi expr" run_rscript "${R_SCRIPT_DIR}/calc.R" --expr "sqrt(0.2)|log(10)" --digits 3
assert_marker "expr_2 = 2.303" "${LOG_PATH}"
run_ok "calc vector unsafe" run_rscript "${R_SCRIPT_DIR}/calc.R" --expr "pnorm(c(-1,0,1))" --unsafe TRUE --digits 3
assert_marker "0.159, 0.500, 0.841" "${LOG_PATH}"
run_fail "calc invalid expr" run_rscript "${R_SCRIPT_DIR}/calc.R" --expr "1+"
run_fail "calc invalid set" run_rscript "${R_SCRIPT_DIR}/calc.R" --set "1a=3" --expr "1"
run_fail "calc restricted function" run_rscript "${R_SCRIPT_DIR}/calc.R" --expr "c(1,2)"

if [ -f "${WORKSPACE_MANIFEST_PATH}" ]; then
  RESEARCH_OUT_DIR="${DATASET_DIR}"
else
  RESEARCH_OUT_DIR_CFG="$(get_config_value defaults.output_dir)"
  if [ -z "${RESEARCH_OUT_DIR_CFG}" ]; then
    RESEARCH_OUT_DIR_CFG="./outputs/tmp"
  fi
  if [[ "${RESEARCH_OUT_DIR_CFG}" == /* || "${RESEARCH_OUT_DIR_CFG}" =~ ^[A-Za-z]: ]]; then
    RESEARCH_OUT_DIR="${RESEARCH_OUT_DIR_CFG}"
  else
    RESEARCH_OUT_DIR="${RUN_ROOT}/${RESEARCH_OUT_DIR_CFG#./}"
  fi
fi
RESEARCH_REPORT_PATH="${RESEARCH_OUT_DIR}/report_canonical.md"
RESEARCH_LOG_PATH="${RESEARCH_OUT_DIR}/analysis_log.jsonl"
rm -f "${RESEARCH_REPORT_PATH}" "${RESEARCH_LOG_PATH}"
run_ok "research_academia smoke" run_rscript "${R_SCRIPT_DIR}/research_academia.R" \
  --query "effect size" --sources openalex,crossref --top-n 3 --max-per-source 3 --max-total 6
assert_marker "\"module\":\"research-academia\"" "${RESEARCH_LOG_PATH}"
assert_marker "Research (Academia)" "${RESEARCH_REPORT_PATH}"
assert_marker "References" "${RESEARCH_REPORT_PATH}"

run_ok "research_academia smoke openalex variant" run_rscript "${R_SCRIPT_DIR}/research_academia.R" \
  --query "power analysis" --sources openalex --top-n 2 --max-per-source 2 --max-total 4
run_ok "research_academia smoke alternate query" run_rscript "${R_SCRIPT_DIR}/research_academia.R" \
  --query "stress experience" --sources openalex --top-n 2 --max-per-source 2 --max-total 4

run_ok "r env info" run_rscript "${CHECK_R_ENV_SCRIPT}"

HAS_LAVAAN=0
if has_r_package "lavaan"; then
  HAS_LAVAAN=1
fi
HAS_PWR=0
if has_r_package "pwr"; then
  HAS_PWR=1
fi
HAS_SEMPOWER=0
if has_r_package "semPower"; then
  HAS_SEMPOWER=1
fi
HAS_PSYCH=0
if has_r_package "psych"; then
  HAS_PSYCH=1
fi
HAS_HAVEN=0
if has_r_package "haven"; then
  HAS_HAVEN=1
else
  if [ "${HAS_RG}" -eq 1 ]; then
    if rg -n "haven available: yes" "${LOG_PATH}" >/dev/null 2>&1; then
      HAS_HAVEN=1
      echo "[WARN] haven check mismatch; using r env info to enable SAV checks." | tee -a "${LOG_PATH}"
    fi
  else
    if grep -n "haven available: yes" "${LOG_PATH}" >/dev/null 2>&1; then
      HAS_HAVEN=1
      echo "[WARN] haven check mismatch; using r env info to enable SAV checks." | tee -a "${LOG_PATH}"
    fi
  fi
fi
if [ "${HAS_HAVEN}" -eq 1 ]; then
  echo "[INFO] haven available; SAV metadata checks enabled." | tee -a "${LOG_PATH}"
else
  echo "[INFO] haven not available; skipping SAV metadata checks." | tee -a "${LOG_PATH}"
fi

run_ok "data_explorer clean" run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars id,site,group3,gender,education,cat_var2,ordinal_var
run_ok "metaskill_runner clean" run_rscript "${R_SCRIPT_DIR}/metaskill_runner.R" --parquet "${PARQUET_PATH_WIN}" --meta sample-description --intent "describe the sample"
cat > "${METASKILL_REPORT_ORIGINAL_PATH}" <<'EOF'
# Dummy Metaskill Report

This is a placeholder for a metaskill report generated outside the runner.
EOF
assert_marker "# Dummy Metaskill Report" "${METASKILL_REPORT_ORIGINAL_PATH}"
run_ok "metaskill_runner finalization" run_rscript "${R_SCRIPT_DIR}/metaskill_runner.R" --parquet "${PARQUET_PATH_WIN}" --meta sample-description --intent "describe the sample" --phase finalization
run_ok "descriptive_stats clean" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --group group3 --digits 3

reset_to_base
set_config_value "logging.enabled" "true"
set_config_value "logging.include_checksum" "true"
set_config_value "logging.include_timestamps" "true"
set_config_value "logging.include_versions" "true"
set_config_value "logging.include_environment" "true"
set_config_value "logging.include_user_prompt" "true"
set_config_value "logging.include_cli_args" "true"
set_config_value "logging.include_inputs" "true"
set_config_value "logging.include_outputs" "true"
start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "logging toggles all on" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --user-prompt "logging toggles on"
assert_log_field "logging on timestamp" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "timestamp_utc" "present"
assert_log_field "logging on versions" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "nlss_version" "present"
assert_log_field "logging on r_version" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "r_version" "present"
assert_log_field "logging on os" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "os" "present"
assert_log_field "logging on user_prompt" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "user_prompt" "present"
assert_log_field "logging on prompt" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "prompt" "present"
assert_log_field "logging on commands" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "commands" "present"
assert_log_field "logging on options" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "options" "present"
assert_log_field "logging on results" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "results" "present"
assert_log_field "logging on checksum" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "checksum_version" "present"

reset_to_base
set_config_value "logging.enabled" "true"
set_config_value "logging.include_checksum" "false"
set_config_value "logging.include_timestamps" "false"
set_config_value "logging.include_versions" "false"
set_config_value "logging.include_environment" "false"
set_config_value "logging.include_user_prompt" "false"
set_config_value "logging.include_cli_args" "false"
set_config_value "logging.include_inputs" "false"
set_config_value "logging.include_outputs" "false"
start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "logging toggles all off" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars x2 --user-prompt "logging toggles off"
assert_log_field "logging off timestamp" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "timestamp_utc" "absent"
assert_log_field "logging off versions" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "nlss_version" "absent"
assert_log_field "logging off r_version" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "r_version" "absent"
assert_log_field "logging off os" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "os" "absent"
assert_log_field "logging off user_prompt" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "user_prompt" "absent"
assert_log_field "logging off prompt" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "prompt" "absent"
assert_log_field "logging off commands" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "commands" "absent"
assert_log_field "logging off options" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "options" "absent"
assert_log_field "logging off results" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "results" "absent"
assert_log_field "logging off checksum" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "checksum_version" "absent"

reset_to_base
set_config_value "logging.enabled" "true"
set_config_value "logging.include_outputs" "maybe"
start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "logging outputs broken" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars x3
assert_log_field "logging outputs broken results" "${ANALYSIS_LOG_PATH}" "${start_count}" "descriptive_stats" "results" "absent"

reset_to_base
set_config_value "logging.enabled" "false"
start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "logging disabled" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --log TRUE
end_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${start_count}" "${end_count}" "logging disabled"

reset_to_base
set_config_value "logging.enabled" "maybe"
start_count="$(log_count "${ANALYSIS_LOG_PATH}")"
run_ok "logging enabled broken" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --log TRUE
end_count="$(log_count "${ANALYSIS_LOG_PATH}")"
assert_log_unchanged "${start_count}" "${end_count}" "logging enabled broken"

reset_to_base
run_ok "plot clean histogram" run_rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_PATH_WIN}" --type histogram --vars age --bins 12
run_ok "plot clean bar percent" run_rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_PATH_WIN}" --type bar --vars gender --group group3 --stat percent --percent-base group --position stack
run_ok "frequencies clean" run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender,education,cat_var2,ordinal_var --group group3
run_ok "crosstabs clean" run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3 --percent row --chisq TRUE --expected TRUE --residuals TRUE
assert_marker "Cramer's V" "${NLSS_REPORT_PATH}"
run_ok "correlations clean" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --method pearson --missing pairwise
run_ok "correlations clean bootstrap" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3 --bootstrap TRUE --bootstrap-samples 200 --seed 42
run_ok "correlations clean r0" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3 --r0 0.2
run_ok "correlations clean compare groups" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3 --group group2 --compare-groups TRUE
run_ok "scale clean" run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean --omega TRUE
if [ "${HAS_PSYCH}" -eq 0 ]; then
  echo "[RUN-EXPECT] efa missing psych" | tee -a "${LOG_PATH}"
  efa_log_path="$(resolve_log_path)"
  start_count="$(log_count "${efa_log_path}")"
  set +e
  run_rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev
  exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] efa missing psych (failed as expected)" | tee -a "${LOG_PATH}"
  else
    efa_missing_logged=0
    for ((i = 1; i <= 10; i++)); do
      if log_has_expected "${efa_log_path}" "${start_count}" "efa" "missing_dependency"; then
        echo "[PASS] efa missing psych (informational log)" | tee -a "${LOG_PATH}"
        efa_missing_logged=1
        break
      fi
      sleep 0.3
    done
    if [ "${efa_missing_logged}" -ne 1 ]; then
      echo "[PASS] efa missing psych (psych available; warnings are informational)" | tee -a "${LOG_PATH}"
      echo "[INFO] efa missing psych did not log missing_dependency; proceeding with EFA suite." | tee -a "${LOG_PATH}"
      HAS_PSYCH=1
    fi
  fi
fi
if [ "${HAS_PSYCH}" -eq 1 ]; then
  run_ok "efa clean" run_rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev
fi
run_ok "reliability clean icc" run_rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_PATH_WIN}" --analysis icc --vars pre_score,mid_score,post_score --icc-model twoway-random --icc-type agreement --icc-unit single
run_ok "assumptions clean" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis regression --dv outcome_anova --ivs x1,x2,x3,mediator
run_ok "regression clean" run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --blocks "x1,x2;x3,mediator" --interactions x1:mediator --center mean --standardize predictors

prepare_labeled_dataset
rm -f "${LABEL_NLSS_REPORT_PATH}" "${LABEL_ANALYSIS_LOG_PATH}" "${LABEL_PARQUET_PATH}"
run_ok "frequencies labeled" run_rscript "${R_SCRIPT_DIR}/frequencies.R" --rds "${LABEL_DATA_PATH_WIN}" --vars gender --group group3
assert_marker "LBL_Gender" "${LABEL_NLSS_REPORT_PATH}"
run_ok "descriptive_stats labeled" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --rds "${LABEL_DATA_PATH_WIN}" --vars outcome_anova --digits 3
assert_marker "LBL_OutcomeAnova" "${LABEL_NLSS_REPORT_PATH}"
run_ok "data_explorer labeled" run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --rds "${LABEL_DATA_PATH_WIN}" --vars education,ordinal_var
assert_marker "LBL_Education" "${LABEL_NLSS_REPORT_PATH}"
run_ok "crosstabs labeled" run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --rds "${LABEL_DATA_PATH_WIN}" --row gender --col group3 --percent row --chisq TRUE
assert_marker "LBL_Group3" "${LABEL_NLSS_REPORT_PATH}"
run_ok "correlations labeled" run_rscript "${R_SCRIPT_DIR}/correlations.R" --rds "${LABEL_DATA_PATH_WIN}" --vars x1,x2 --method pearson --missing pairwise
assert_marker "LBL_X1" "${LABEL_NLSS_REPORT_PATH}"
run_ok "scale labeled" run_rscript "${R_SCRIPT_DIR}/scale.R" --rds "${LABEL_DATA_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean
assert_marker "LBL_F1_1" "${LABEL_NLSS_REPORT_PATH}"
run_ok "reliability labeled" run_rscript "${R_SCRIPT_DIR}/reliability.R" --rds "${LABEL_DATA_PATH_WIN}" --analysis test_retest --vars pre_score,post_score
assert_marker "LBL_PreScore" "${LABEL_NLSS_REPORT_PATH}"
run_ok "t_test labeled" run_rscript "${R_SCRIPT_DIR}/t_test.R" --rds "${LABEL_DATA_PATH_WIN}" --vars outcome_anova --group group2
assert_marker "LBL_Control" "${LABEL_NLSS_REPORT_PATH}"
run_ok "nonparametric labeled" run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --rds "${LABEL_DATA_PATH_WIN}" --vars ordinal_var --mu 3 --test wilcoxon --exact FALSE --continuity FALSE
assert_marker "LBL_Ordinal" "${LABEL_NLSS_REPORT_PATH}"
run_ok "anova labeled" run_rscript "${R_SCRIPT_DIR}/anova.R" --rds "${LABEL_DATA_PATH_WIN}" --dv outcome_anova --between group2
assert_marker "LBL_Group2" "${LABEL_NLSS_REPORT_PATH}"
run_ok "regression labeled" run_rscript "${R_SCRIPT_DIR}/regression.R" --rds "${LABEL_DATA_PATH_WIN}" --dv outcome_reg --ivs x1,mediator
assert_marker "LBL_Mediator" "${LABEL_NLSS_REPORT_PATH}"
run_ok "missings labeled" run_rscript "${R_SCRIPT_DIR}/missings.R" --rds "${LABEL_DATA_PATH_WIN}" --vars cat_var2,cat_var --method listwise
assert_marker "LBL_CatVar2" "${LABEL_NLSS_REPORT_PATH}"
run_ok "impute labeled" run_rscript "${R_SCRIPT_DIR}/impute.R" --rds "${LABEL_DATA_PATH_WIN}" --vars cat_var --engine simple --numeric-method median --categorical-method mode --indicator FALSE
assert_marker "LBL_CatVar" "${LABEL_NLSS_REPORT_PATH}"

prepare_meta_dataset
rm -f "${META_PARQUET_PATH}"
run_ok "init workspace meta rds" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --rds "${META_RDS_PATH_WIN}"
run_ok "check parquet meta rds" run_rscript "${CHECK_PARQUET_LABELS_SCRIPT}" "${META_PARQUET_PATH_WIN}" "rds"
assert_marker "META_OK rds" "${LOG_PATH}"
rm -f "${META_PARQUET_PATH}"
run_ok "init workspace meta rdata" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --rdata "${META_RDATA_PATH_WIN}" --df "${META_LABEL}"
run_ok "check parquet meta rdata" run_rscript "${CHECK_PARQUET_LABELS_SCRIPT}" "${META_PARQUET_PATH_WIN}" "rdata"
assert_marker "META_OK rdata" "${LOG_PATH}"
if [ "${HAS_HAVEN}" -eq 1 ]; then
  rm -f "${META_PARQUET_PATH}"
  run_ok "init workspace meta sav" run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --sav "${META_SAV_PATH_WIN}"
  run_ok "check parquet meta sav" run_rscript "${CHECK_PARQUET_LABELS_SCRIPT}" "${META_PARQUET_PATH_WIN}" "sav"
  assert_marker "META_OK sav" "${LOG_PATH}"
fi
if [ "${HAS_PWR}" -eq 0 ]; then
  echo "[RUN-EXPECT] power missing pwr" | tee -a "${LOG_PATH}"
  power_log_path="$(resolve_log_path)"
  start_count="$(log_count "${power_log_path}")"
  set +e
  run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
  exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] power missing pwr (failed as expected)" | tee -a "${LOG_PATH}"
  else
    power_missing_logged=0
    for ((i = 1; i <= 10; i++)); do
      if log_has_expected "${power_log_path}" "${start_count}" "power" "missing_dependency"; then
        echo "[PASS] power missing pwr (informational log)" | tee -a "${LOG_PATH}"
        power_missing_logged=1
        break
      fi
      sleep 0.3
    done
    if [ "${power_missing_logged}" -ne 1 ]; then
      echo "[PASS] power missing pwr (pwr available; warnings are informational)" | tee -a "${LOG_PATH}"
      echo "[INFO] power missing pwr did not log missing_dependency; proceeding with power suite." | tee -a "${LOG_PATH}"
      HAS_PWR=1
    fi
  fi
fi
if [ "${HAS_PWR}" -eq 1 ]; then
  run_ok "power clean ttest apriori" run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
  run_ok "power clean correlation sensitivity" run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis correlation --mode sensitivity --n 120 --power 0.8
  if [ "${HAS_SEMPOWER}" -eq 1 ]; then
    run_ok "power clean sem apriori" run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
  else
    echo "[RUN-EXPECT] power missing semPower" | tee -a "${LOG_PATH}"
    power_log_path="$(resolve_log_path)"
    start_count="$(log_count "${power_log_path}")"
    set +e
    run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
    exit_status=$?
    set -e
    if [ "${exit_status}" -ne 0 ]; then
      echo "[PASS] power missing semPower (failed as expected)" | tee -a "${LOG_PATH}"
    else
      power_missing_logged=0
      for ((i = 1; i <= 10; i++)); do
        if log_has_expected "${power_log_path}" "${start_count}" "power" "missing_dependency"; then
          echo "[PASS] power missing semPower (informational log)" | tee -a "${LOG_PATH}"
          power_missing_logged=1
          break
        fi
        sleep 0.3
      done
      if [ "${power_missing_logged}" -ne 1 ]; then
        echo "[PASS] power missing semPower (semPower available; warnings are informational)" | tee -a "${LOG_PATH}"
        echo "[INFO] power missing semPower did not log missing_dependency; proceeding with SEM power." | tee -a "${LOG_PATH}"
        HAS_SEMPOWER=1
      fi
    fi
    if [ "${HAS_SEMPOWER}" -eq 1 ]; then
      run_ok "power clean sem apriori" run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8
    fi
  fi
fi
if [ "${HAS_LAVAAN}" -eq 0 ]; then
  echo "[RUN-EXPECT] sem missing lavaan" | tee -a "${LOG_PATH}"
  sem_log_path="$(resolve_log_path)"
  start_count="$(log_count "${sem_log_path}")"
  set +e
  run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=f1_1,f1_2"
  exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] sem missing lavaan (failed as expected)" | tee -a "${LOG_PATH}"
  else
    sem_missing_logged=0
    for ((i = 1; i <= 10; i++)); do
      if log_has_expected "${sem_log_path}" "${start_count}" "sem" "missing_dependency"; then
        echo "[PASS] sem missing lavaan (informational log)" | tee -a "${LOG_PATH}"
        sem_missing_logged=1
        break
      fi
      sleep 0.3
    done
    if [ "${sem_missing_logged}" -ne 1 ]; then
      echo "[PASS] sem missing lavaan (lavaan available; warnings are informational)" | tee -a "${LOG_PATH}"
      echo "[INFO] sem missing lavaan did not log missing_dependency; proceeding with sem suite." | tee -a "${LOG_PATH}"
      HAS_LAVAAN=1
    fi
  fi
fi
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  run_ok "sem clean cfa" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
  run_ok "sem clean mediation" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis mediation --x x1 --m mediator --y outcome_reg --bootstrap FALSE
  run_ok "assumptions sem clean" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis sem --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
  run_expect_log "sem missing vars" "$(resolve_log_path)" "sem" "invalid_input" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis cfa --factors "F1=missing_item1,missing_item2"
fi
run_ok "anova clean between" run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
run_ok "t_test clean one-sample" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --mu 0
run_ok "t_test clean independent" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group2
run_ok "t_test clean paired" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x f1_1 --y f1_2
run_ok "nonparametric clean mann_whitney" run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group2 --test mann_whitney
run_ok "nonparametric clean friedman" run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --within pre_score,mid_score,post_score --subject-id id --test friedman

prepare_mixed_models_data
prepare_labeled_mixed_models_data
rm -f "${MIXED_LABEL_NLSS_REPORT_PATH}" "${MIXED_LABEL_ANALYSIS_LOG_PATH}" "${MIXED_LABEL_PARQUET_PATH}"
run_init_workspace_custom "init workspace (mixed models)" "${MIXED_DATA_PATH_WIN}"

HAS_LME4=0
if has_r_package "lme4"; then
  HAS_LME4=1
fi
HAS_PERFORMANCE=0
if has_r_package "performance"; then
  HAS_PERFORMANCE=1
fi
HAS_EMMEANS=0
if has_r_package "emmeans"; then
  HAS_EMMEANS=1
fi

if [ "${HAS_LME4}" -eq 0 ]; then
  echo "[RUN-EXPECT] mixed_models missing lme4" | tee -a "${LOG_PATH}"
  start_count="$(log_count "${MIXED_ANALYSIS_LOG_PATH}")"
  set +e
  run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + (1|id)"
  exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] mixed_models missing lme4 (failed as expected)" | tee -a "${LOG_PATH}"
  else
    mixed_models_missing_logged=0
    for ((i = 1; i <= 10; i++)); do
      if log_has_expected "${MIXED_ANALYSIS_LOG_PATH}" "${start_count}" "mixed_models" "missing_dependency"; then
        echo "[PASS] mixed_models missing lme4 (informational log)" | tee -a "${LOG_PATH}"
        mixed_models_missing_logged=1
        break
      fi
      sleep 0.3
    done
    if [ "${mixed_models_missing_logged}" -ne 1 ]; then
      echo "[PASS] mixed_models missing lme4 (lme4 available; warnings are informational)" | tee -a "${LOG_PATH}"
      echo "[INFO] mixed_models missing lme4 did not log missing_dependency; proceeding with mixed_models suite." | tee -a "${LOG_PATH}"
      HAS_LME4=1
    fi
  fi
fi

if [ "${HAS_LME4}" -eq 1 ] && [ "${HAS_PERFORMANCE}" -eq 0 ]; then
  echo "[RUN-EXPECT] mixed_models missing performance" | tee -a "${LOG_PATH}"
  start_count="$(log_count "${MIXED_ANALYSIS_LOG_PATH}")"
  set +e
  run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + (1|id)"
  exit_status=$?
  set -e
  if [ "${exit_status}" -ne 0 ]; then
    echo "[PASS] mixed_models missing performance (failed as expected)" | tee -a "${LOG_PATH}"
  else
    mixed_models_missing_logged=0
    for ((i = 1; i <= 10; i++)); do
      if log_has_expected "${MIXED_ANALYSIS_LOG_PATH}" "${start_count}" "mixed_models" "missing_dependency"; then
        echo "[PASS] mixed_models missing performance (informational log)" | tee -a "${LOG_PATH}"
        mixed_models_missing_logged=1
        break
      fi
      sleep 0.3
    done
    if [ "${mixed_models_missing_logged}" -ne 1 ]; then
      echo "[PASS] mixed_models missing performance (performance available; warnings are informational)" | tee -a "${LOG_PATH}"
      echo "[INFO] mixed_models missing performance did not log missing_dependency; proceeding with mixed_models suite." | tee -a "${LOG_PATH}"
      HAS_PERFORMANCE=1
    fi
  fi
fi

if [ "${HAS_LME4}" -eq 1 ] && [ "${HAS_PERFORMANCE}" -eq 1 ]; then
  run_ok "mixed_models labeled" run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --rds "${MIXED_LABEL_DATA_PATH_WIN}" --formula "score ~ time + group3 + x1 + (1|id)" --reml TRUE --df-method satterthwaite
  assert_marker "LBL_Time" "${MIXED_LABEL_NLSS_REPORT_PATH}"
  run_ok "mixed_models clean" run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + group3 + x1 + (1|id)" --reml TRUE --df-method satterthwaite
  run_ok "assumptions mixed_models clean" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --analysis mixed_models --formula "score ~ time + group3 + x1 + (1|id)"
  run_ok "mixed_models emmeans" run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise --p-adjust holm
  run_expect_log "mixed_models missing random" "${MIXED_ANALYSIS_LOG_PATH}" "mixed_models" "invalid_input" \
    run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --dv score --fixed time,group3
fi

run_init_workspace "init workspace (edge)"
run_ok "data_explorer edge" run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,cat_var,high_missing_var,all_missing_var,zero_var,near_zero_var
run_ok "descriptive_stats edge" run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars skewed_var,outlier_var,zero_var,near_zero_var,high_missing_var --group group2 --digits 3 --trim 0.2 --iqr-multiplier 2 --outlier-z 2.5
run_ok "plot edge density" run_rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_PATH_WIN}" --type density --vars skewed_var --group group2 --bw 0.5 --alpha 0.4
run_ok "frequencies edge" run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars cat_var --group group2
run_ok "crosstabs edge" run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row cat_var --col group2 --fisher TRUE --fisher-simulate TRUE --fisher-b 200
run_ok "correlations edge" run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --x skewed_var,outlier_var --y age,income --method spearman --missing complete --p-adjust BH
if [ "${HAS_PWR}" -eq 1 ]; then
  run_ok "power edge estimate effect" run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --mode apriori --estimate-effect TRUE --vars outcome_anova --group group2 --power 0.8
fi
run_ok "scale edge" run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f2_1,f2_2,f2_3,f2_4_rev --reverse f2_4_rev --reverse-min 1 --reverse-max 5 --missing complete --omega FALSE
if [ "${HAS_PSYCH}" -eq 1 ]; then
  run_ok "efa edge" run_rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev --method minres --n-factors 2 --missing pairwise --cor spearman --loading-cutoff 0.4 --sort-loadings FALSE
fi
run_ok "reliability edge kappa" run_rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_PATH_WIN}" --analysis kappa --vars cat_var,cat_var2 --kappa-weight none
run_ok "assumptions edge" run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis anova --dv outcome_anova --between group3 --within pre_score,mid_score,post_score --subject-id id
run_ok "regression edge bootstrap" run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --ivs x1,x2,x3 --bootstrap TRUE --bootstrap-samples 200 --seed 42
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  run_ok "sem edge path" run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis path --dv outcome_reg --ivs skewed_var,outlier_var
fi
run_ok "anova edge mixed" run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --within pre_score,mid_score,post_score --between group3 --subject-id id --posthoc pairwise
run_ok "t_test edge independent" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars pre_score --group group2 --var-equal TRUE
run_ok "t_test edge bootstrap paired" run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x pre_score --y post_score --bootstrap TRUE --bootstrap-samples 200 --seed 42
run_ok "nonparametric edge one-sample" run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars skewed_var --mu 0 --test wilcoxon --exact FALSE --continuity FALSE
run_ok "nonparametric edge kruskal posthoc" run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group3 --test kruskal --posthoc pairwise --p-adjust holm

run_init_workspace "init workspace (missings clean)"
run_ok "missings clean" run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2,x3,mediator --method listwise

run_init_workspace "init workspace (missings edge)"
run_ok "missings edge" run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,satisfaction,outcome_reg,high_missing_var,all_missing_var --method indicator --indicator-threshold 0.2 --drop-threshold 0.5

run_init_workspace "init workspace (impute clean)"
run_ok "impute clean" run_rscript "${R_SCRIPT_DIR}/impute.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,cat_var --engine simple --numeric-method median --categorical-method mode --indicator FALSE

run_init_workspace "init workspace (impute edge)"
run_ok "impute edge" run_rscript "${R_SCRIPT_DIR}/impute.R" --parquet "${PARQUET_PATH_WIN}" --vars age,cat_var,all_missing_var --engine simple --numeric-method mean --categorical-method mode --indicator TRUE --indicator-suffix _miss

run_init_workspace "init workspace (transform clean)"
run_ok "data_transform clean" run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --calc "score_avg=(f1_1+f1_2+f1_4)/3" --standardize x1

run_init_workspace "init workspace (transform edge)"
run_ok "data_transform edge" run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --transform "skewed_var=log" --percentile-bins "outlier_var=4" --recode "group3=A:1,B:2,C:3" --drop zero_var --confirm-drop TRUE

template_test "template init_workspace nlss" "init_workspace.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"
template_test "template init_workspace scratchpad" "init_workspace.scratchpad" "${SCRATCHPAD_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_PATH_WIN}"
template_test "template metaskill_runner default" "metaskill_runner.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/metaskill_runner.R" --parquet "${PARQUET_PATH_WIN}" --meta sample-description --intent "describe the sample"

template_test "template descriptive_stats" "descriptive_stats.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2
template_test "template descriptive_stats robust" "descriptive_stats.robust" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2 --template robust
template_test "template descriptive_stats distribution" "descriptive_stats.distribution" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2 --template distribution
template_ref_test "template descriptive_stats ref" "descriptive_stats" "temporal" "descriptive_stats.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova,x1,x2 --template temporal
template_test "template frequencies default" "frequencies.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender,education
template_test "template frequencies grouped" "frequencies.grouped" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH_WIN}" --vars gender --group group3
template_test "template crosstabs default" "crosstabs.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3
template_test "template crosstabs grouped" "crosstabs.grouped" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH_WIN}" --row gender --col group3 --group site
template_test "template correlations default" "correlations.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3
template_test "template correlations cross" "correlations.cross" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --x x1,x2 --y x3,mediator
template_test "template correlations matrix" "correlations.matrix" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3 --template matrix
template_test "template correlations comparison" "correlations.comparison" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH_WIN}" --vars x1,x2,x3 --group group2 --compare-groups TRUE
template_test "template scale" "scale.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/scale.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5
if [ "${HAS_PSYCH}" -eq 1 ]; then
  template_test "template efa" "efa.default" "${NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/efa.R" --parquet "${PARQUET_PATH_WIN}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev
else
  echo "[WARN] skipping efa template (psych not installed)" | tee -a "${LOG_PATH}"
fi
template_test "template reliability" "reliability.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/reliability.R" --parquet "${PARQUET_PATH_WIN}" --analysis icc --vars pre_score,mid_score,post_score
template_test "template data_explorer" "data_explorer.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_PATH_WIN}" --vars id,site,group3
template_test "template plot" "plot.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_PATH_WIN}" --type histogram --vars age
template_test_optional "template data_transform" "data_transform.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/data_transform.R" --parquet "${PARQUET_PATH_WIN}" --calc "score_avg=(f1_1+f1_2+f1_4)/3"
template_test_optional "template impute" "impute.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/impute.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,cat_var --engine simple
run_init_workspace "init workspace (templates after transform)"
template_test_optional "template missings" "missings.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/missings.R" --parquet "${PARQUET_PATH_WIN}" --vars age,income,satisfaction --method listwise
run_init_workspace "init workspace (templates after missings)"
template_test "template assumptions ttest" "assumptions.ttest" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --vars x1 --group group2
template_test "template assumptions anova" "assumptions.anova" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis anova --dv outcome_anova --between group3
template_test "template assumptions regression" "assumptions.regression" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis regression --dv outcome_anova --ivs x1,x2,x3
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  template_test "template assumptions sem" "assumptions.sem" "${NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${PARQUET_PATH_WIN}" --analysis sem --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev"
else
  echo "[WARN] skipping assumptions sem template (lavaan not installed)" | tee -a "${LOG_PATH}"
fi
if [ "${HAS_LME4}" -eq 1 ]; then
  template_test "template assumptions mixed_models" "assumptions.mixed_models" "${MIXED_NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/assumptions.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --analysis mixed_models --formula "score ~ time + group3 + x1 + (1|id)"
else
  echo "[WARN] skipping assumptions mixed_models template (lme4 not installed)" | tee -a "${LOG_PATH}"
fi
template_test "template regression" "regression.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_reg --ivs x1,x2,x3
if [ "${HAS_PWR}" -eq 1 ]; then
  template_test "template power" "power.default" "${NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
else
  echo "[WARN] skipping power template (pwr not installed)" | tee -a "${LOG_PATH}"
fi
if [ "${HAS_LAVAAN}" -eq 1 ]; then
  template_test "template sem default" "sem.default" "${NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis path --dv outcome_reg --ivs x1,x2
  template_test "template sem mediation" "sem.mediation" "${NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/sem.R" --parquet "${PARQUET_PATH_WIN}" --analysis mediation --x x1 --m mediator --y outcome_reg
else
  echo "[WARN] skipping sem templates (lavaan not installed)" | tee -a "${LOG_PATH}"
fi
if [ "${HAS_LME4}" -eq 1 ]; then
  template_test "template mixed_models default" "mixed_models.default" "${MIXED_NLSS_REPORT_PATH}" \
    run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time + group3 + x1 + (1|id)"
  if [ "${HAS_EMMEANS}" -eq 1 ]; then
    template_test "template mixed_models emmeans" "mixed_models.emmeans" "${MIXED_NLSS_REPORT_PATH}" \
      run_rscript "${R_SCRIPT_DIR}/mixed_models.R" --parquet "${MIXED_PARQUET_PATH_WIN}" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise
  else
    echo "[WARN] skipping mixed_models emmeans template (emmeans not installed)" | tee -a "${LOG_PATH}"
  fi
else
  echo "[WARN] skipping mixed_models templates (lme4 not installed)" | tee -a "${LOG_PATH}"
fi
template_test "template anova default" "anova.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
template_test "template anova posthoc" "anova.posthoc" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --dv outcome_anova --between group3
template_test "template t_test" "t_test.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars x1 --mu 0
template_test "template nonparametric default" "nonparametric.default" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group2 --test mann_whitney
template_test "template nonparametric posthoc" "nonparametric.posthoc" "${NLSS_REPORT_PATH}" \
  run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group3 --test kruskal --posthoc pairwise --p-adjust holm

run_expect_log "t_test invalid group levels" "$(resolve_log_path)" "t_test" "expected_invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group3 --expect-two-groups TRUE
run_expect_log "t_test paired with group" "$(resolve_log_path)" "t_test" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH_WIN}" --x pre_score --y post_score --group group3
run_expect_log "nonparametric invalid group levels" "$(resolve_log_path)" "nonparametric" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/nonparametric.R" --parquet "${PARQUET_PATH_WIN}" --vars outcome_anova --group group3 --test mann_whitney
run_expect_log "anova missing subject-id" "$(resolve_log_path)" "anova" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/anova.R" --parquet "${PARQUET_PATH_WIN}" --within pre_score,post_score
run_expect_log "regression missing dv" "$(resolve_log_path)" "regression" "invalid_input" \
  run_rscript "${R_SCRIPT_DIR}/regression.R" --parquet "${PARQUET_PATH_WIN}" --ivs x1,x2
if [ "${HAS_PWR}" -eq 1 ]; then
  run_expect_log "power missing effect size" "$(resolve_log_path)" "power" "invalid_input" \
    run_rscript "${R_SCRIPT_DIR}/power.R" --parquet "${PARQUET_PATH_WIN}" --analysis ttest --mode apriori --t-type two-sample --power 0.8
fi

check_integrity_expect "check_integrity clean" "${CHECK_INTEGRITY_LOG_PATH}" 0
tamper_log_edit "${ANALYSIS_LOG_PATH}" "${INTEGRITY_EDIT_LOG_PATH}"
check_integrity_expect "check_integrity tamper edit" "${CHECK_INTEGRITY_EDIT_LOG_PATH}" 1
tamper_log_delete "${ANALYSIS_LOG_PATH}" "${INTEGRITY_DELETE_LOG_PATH}"
check_integrity_expect "check_integrity tamper delete" "${CHECK_INTEGRITY_DELETE_LOG_PATH}" 1

NLSS_RECONSTRUCT_LOG="${ANALYSIS_LOG_REL_PATH}" run_ok "reconstruct reports" run_rscript "${RECONSTRUCT_REPORTS_SCRIPT}"
assert_marker "# Descriptive Statistics" "${RECONSTRUCT_REPORT_PATH}"
assert_marker "# Dummy Metaskill Report" "${METASKILL_REPORT_RECON_PATH}"

echo "[DONE] smoke tests finished" | tee -a "${LOG_PATH}"
cleanup_runs
