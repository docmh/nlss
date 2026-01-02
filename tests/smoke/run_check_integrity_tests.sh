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
CHECK_INTEGRITY_SCRIPT="${ROOT_DIR}/scripts/R/check_integrity.R"

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

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
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

WORKSPACE_DIR="${RUN_ROOT}/check_integrity_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
LOG_FILE="${RUN_ROOT}/check_integrity_test.log"
TMP_BASE="${RUN_ROOT}/tmp/check_integrity"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PARQUET_PATH="${DATASET_DIR}/${DATASET_LABEL}.parquet"
ANALYSIS_LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"

BASE_LOG_PATH="${TMP_BASE}/analysis_log_base.jsonl"
EDIT_LOG_PATH="${TMP_BASE}/analysis_log_edit.jsonl"
DELETE_LOG_PATH="${TMP_BASE}/analysis_log_delete.jsonl"
DUP_LOG_PATH="${TMP_BASE}/analysis_log_duplicate.jsonl"
CODE_CHANGE_FILE="${ROOT_DIR}/scripts/integrity_test_marker.tmp"

mkdir -p "${RUN_ROOT}" "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
: > "${LOG_FILE}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"

cd "${WORKSPACE_DIR}"

cleanup_code_change() {
  rm -f "${CODE_CHANGE_FILE}"
}
trap cleanup_code_change EXIT

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R and ensure Rscript is on PATH." | tee -a "${LOG_FILE}"
  exit 1
fi

HAS_RG=0
if command -v rg >/dev/null 2>&1; then
  HAS_RG=1
fi

log() {
  echo "$@" | tee -a "${LOG_FILE}"
}

run_ok() {
  local label="$1"; shift
  log "[RUN] ${label}"
  "$@" >>"${LOG_FILE}" 2>&1
  log "[PASS] ${label}"
}

assert_tail_matches() {
  local label="$1"; shift
  local pattern="$1"; shift
  if [ "${HAS_RG}" -eq 1 ]; then
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | rg -n "${pattern}" >/dev/null 2>&1; then
      return 0
    fi
  else
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | grep -n "${pattern}" >/dev/null 2>&1; then
      return 0
    fi
  fi
  log "[FAIL] ${label} (missing pattern: ${pattern})"
  exit 1
}

check_integrity_expect() {
  local label="$1"; shift
  local target_path="$1"; shift
  local expect_warning="$1"; shift
  local expect_diag="$1"; shift
  log "[RUN] ${label}"
  local start_line
  start_line=$(wc -l < "${LOG_FILE}")
  Rscript "${CHECK_INTEGRITY_SCRIPT}" "${target_path}" --diagnose TRUE >>"${LOG_FILE}" 2>&1
  LAST_TAIL_START=$((start_line + 1))
  local has_checksum=0
  local has_warning=0
  local has_diag=0
  if [ "${HAS_RG}" -eq 1 ]; then
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | rg -n "^[0-9a-f]{32} [0-9]+\\s*$" >/dev/null 2>&1; then
      has_checksum=1
    fi
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | rg -n "^WARNING: multiple reverted checksums found\\.[[:space:]]*$" >/dev/null 2>&1; then
      has_warning=1
    fi
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | rg -n "^DIAG line=" >/dev/null 2>&1; then
      has_diag=1
    fi
  else
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | grep -n "^[0-9a-f]\\{32\\} [0-9]\\+[[:space:]]*$" >/dev/null 2>&1; then
      has_checksum=1
    fi
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | grep -n "^WARNING: multiple reverted checksums found\\.[[:space:]]*$" >/dev/null 2>&1; then
      has_warning=1
    fi
    if tail -n +"${LAST_TAIL_START}" "${LOG_FILE}" | grep -n "^DIAG line=" >/dev/null 2>&1; then
      has_diag=1
    fi
  fi
  if [ "${has_checksum}" -ne 1 ]; then
    log "[FAIL] ${label} (no checksum output)"
    exit 1
  fi
  if [ "${expect_warning}" -eq 1 ] && [ "${has_warning}" -ne 1 ]; then
    log "[FAIL] ${label} (expected warning)"
    exit 1
  fi
  if [ "${expect_warning}" -eq 0 ] && [ "${has_warning}" -eq 1 ]; then
    log "[FAIL] ${label} (unexpected warning)"
    exit 1
  fi
  if [ "${expect_diag}" -eq 1 ] && [ "${has_diag}" -ne 1 ]; then
    log "[FAIL] ${label} (missing diagnostics)"
    exit 1
  fi
  log "[PASS] ${label}"
}

tamper_edit() {
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

tamper_delete() {
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

tamper_duplicate() {
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
lines.insert(idx + 1, lines[idx])
dest.write_bytes(b"".join(lines))
PY
}

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"
run_ok "descriptive_stats" Rscript "${R_SCRIPT_DIR}/descriptive_stats.R" --parquet "${PARQUET_PATH}" --vars x1,x2
run_ok "frequencies" Rscript "${R_SCRIPT_DIR}/frequencies.R" --parquet "${PARQUET_PATH}" --vars group3
run_ok "t_test" Rscript "${R_SCRIPT_DIR}/t_test.R" --parquet "${PARQUET_PATH}" --vars outcome_anova --group group2

if [ ! -f "${ANALYSIS_LOG_PATH}" ]; then
  log "[FAIL] missing analysis_log.jsonl"
  exit 1
fi
if [ "${HAS_RG}" -eq 1 ]; then
  if ! rg -n "analysis_log_seq:[[:space:]]*[0-9]+" "${WORKSPACE_MANIFEST_PATH}" >/dev/null 2>&1; then
    log "[FAIL] missing analysis_log_seq in manifest"
    exit 1
  fi
else
  if ! grep -n "analysis_log_seq:[[:space:]]*[0-9]\\+" "${WORKSPACE_MANIFEST_PATH}" >/dev/null 2>&1; then
    log "[FAIL] missing analysis_log_seq in manifest"
    exit 1
  fi
fi

check_integrity_expect "integrity clean" "${ANALYSIS_LOG_PATH}" 0 1
assert_tail_matches "integrity clean diagnostic" "DIAG line="

cp "${ANALYSIS_LOG_PATH}" "${BASE_LOG_PATH}"

run_ok "codebase change marker" bash -c "printf \"\" > \"${CODE_CHANGE_FILE}\""
run_ok "correlations (codebase changed)" Rscript "${R_SCRIPT_DIR}/correlations.R" --parquet "${PARQUET_PATH}" --vars x1,x2
rm -f "${CODE_CHANGE_FILE}"
run_ok "crosstabs (codebase restored)" Rscript "${R_SCRIPT_DIR}/crosstabs.R" --parquet "${PARQUET_PATH}" --row group2 --col group3 --percent row
check_integrity_expect "integrity codebase change" "${ANALYSIS_LOG_PATH}" 1 1
assert_tail_matches "integrity codebase mismatch" "status=mismatch seq=ok"

tamper_edit "${BASE_LOG_PATH}" "${EDIT_LOG_PATH}"
check_integrity_expect "integrity tamper edit" "${EDIT_LOG_PATH}" 1 1
assert_tail_matches "integrity edit detect" "inferred=edited_candidate"
assert_tail_matches "integrity edit chain" "inferred=post_edit_chain"

tamper_delete "${BASE_LOG_PATH}" "${DELETE_LOG_PATH}"
check_integrity_expect "integrity tamper delete" "${DELETE_LOG_PATH}" 1 1
assert_tail_matches "integrity delete detect" "inferred=post_delete_gap"
assert_tail_matches "integrity delete gap" "missing_seq="

tamper_duplicate "${BASE_LOG_PATH}" "${DUP_LOG_PATH}"
check_integrity_expect "integrity tamper duplicate" "${DUP_LOG_PATH}" 1 1
assert_tail_matches "integrity duplicate detect" "inferred=seq_out_of_order"

log "[DONE] check_integrity tests finished"
