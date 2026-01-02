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

WORKSPACE_DIR="${RUN_ROOT}/data_explorer_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/data_explorer"
LOG_FILE="${RUN_ROOT}/data_explorer_test.log"

mkdir -p "${RUN_ROOT}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"

CUSTOM_CSV="${TMP_BASE}/data_explorer_custom.csv"
CUSTOM_LABEL="$(basename "${CUSTOM_CSV}")"
CUSTOM_LABEL="${CUSTOM_LABEL%.*}"
CUSTOM_DIR="${WORKSPACE_DIR}/${CUSTOM_LABEL}"
CUSTOM_LOG_PATH="${CUSTOM_DIR}/analysis_log.jsonl"
CUSTOM_REPORT_PATH="${CUSTOM_DIR}/report_canonical.md"
INTERACTIVE_CSV_INPUT="${TMP_BASE}/interactive_csv_input.txt"
INTERACTIVE_RDATA_INPUT="${TMP_BASE}/interactive_rdata_input.txt"
INTERACTIVE_BAD_INPUT="${TMP_BASE}/interactive_bad_input.txt"
HELP_PATH="${TMP_BASE}/data_explorer_help.txt"
TYPED_BASE="data_explorer_typed"
TYPED_RDS="${TMP_BASE}/${TYPED_BASE}.rds"
TYPED_RDATA="${TMP_BASE}/${TYPED_BASE}.RData"
TYPED_DF_NAME="typed_df"
TYPED_RDS_DIR="${WORKSPACE_DIR}/${TYPED_BASE}"
TYPED_RDS_LOG_PATH="${TYPED_RDS_DIR}/analysis_log.jsonl"
TYPED_RDS_REPORT_PATH="${TYPED_RDS_DIR}/report_canonical.md"
TYPED_RDATA_DIR="${WORKSPACE_DIR}/${TYPED_DF_NAME}"
TYPED_RDATA_LOG_PATH="${TYPED_RDATA_DIR}/analysis_log.jsonl"
TYPED_RDATA_REPORT_PATH="${TYPED_RDATA_DIR}/report_canonical.md"
SEMI_CSV="${TMP_BASE}/data_explorer_semicolon.csv"
SEMI_LABEL="data_explorer_semicolon"
SEMI_DIR="${WORKSPACE_DIR}/${SEMI_LABEL}"
SEMI_LOG_PATH="${SEMI_DIR}/analysis_log.jsonl"
SEMI_REPORT_PATH="${SEMI_DIR}/report_canonical.md"
NO_HEADER_CSV="${TMP_BASE}/data_explorer_no_header.csv"
NO_HEADER_LABEL="data_explorer_no_header"
NO_HEADER_DIR="${WORKSPACE_DIR}/${NO_HEADER_LABEL}"
NO_HEADER_LOG_PATH="${NO_HEADER_DIR}/analysis_log.jsonl"
NO_HEADER_REPORT_PATH="${NO_HEADER_DIR}/report_canonical.md"
ROUNDING1_CSV="${TMP_BASE}/data_explorer_rounding_1.csv"
ROUNDING1_LABEL="data_explorer_rounding_1"
ROUNDING1_DIR="${WORKSPACE_DIR}/${ROUNDING1_LABEL}"
ROUNDING1_LOG_PATH="${ROUNDING1_DIR}/analysis_log.jsonl"
ROUNDING1_REPORT_PATH="${ROUNDING1_DIR}/report_canonical.md"
ROUNDING2_CSV="${TMP_BASE}/data_explorer_rounding_2.csv"
ROUNDING2_LABEL="data_explorer_rounding_2"
ROUNDING2_DIR="${WORKSPACE_DIR}/${ROUNDING2_LABEL}"
ROUNDING2_LOG_PATH="${ROUNDING2_DIR}/analysis_log.jsonl"
ROUNDING2_REPORT_PATH="${ROUNDING2_DIR}/report_canonical.md"
BAD_RDS="${TMP_BASE}/data_explorer_bad.rds"
BAD_RDS_LABEL="data_explorer_bad"
BAD_RDS_DIR="${WORKSPACE_DIR}/${BAD_RDS_LABEL}"
BAD_RDS_LOG_PATH="${BAD_RDS_DIR}/analysis_log.jsonl"
BAD_RDATA="${TMP_BASE}/data_explorer_bad.RData"
BAD_RDATA_DF="not_df"
BAD_RDATA_LABEL="${BAD_RDATA_DF}"
BAD_RDATA_DIR="${WORKSPACE_DIR}/${BAD_RDATA_LABEL}"
BAD_RDATA_LOG_PATH="${BAD_RDATA_DIR}/analysis_log.jsonl"
SAV_PATH="${TMP_BASE}/data_explorer_typed_sav.sav"
SAV_LABEL="data_explorer_typed_sav"
SAV_DIR="${WORKSPACE_DIR}/${SAV_LABEL}"
SAV_LOG_PATH="${SAV_DIR}/analysis_log.jsonl"
SAV_REPORT_PATH="${SAV_DIR}/report_canonical.md"

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

mkdir -p "${WORKSPACE_DIR}" "${DATASET_DIR}" "${TMP_BASE}"
: > "${WORKSPACE_MANIFEST_PATH}"
export TMPDIR="${TMP_BASE}"
export TMP="${TMP_BASE}"
export TEMP="${TMP_BASE}"
rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}"
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

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_FILE}"
  "$@" >>"${LOG_FILE}" 2>&1
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

assert_not_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if rg -q --fixed-strings "$expected" "$file"; then
      echo "Did not expect to find: $expected" | tee -a "${LOG_FILE}"
      echo "In file: $file" | tee -a "${LOG_FILE}"
      exit 1
    fi
  else
    if grep -qF "$expected" "$file"; then
      echo "Did not expect to find: $expected" | tee -a "${LOG_FILE}"
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

run_expect_fail() {
  local label="$1"; shift
  local log_path="$1"; shift
  echo "[RUN-EXPECT-FAIL] ${label}" | tee -a "${LOG_FILE}"
  local start_count=""
  if [ -n "${log_path}" ]; then
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
  if [ -n "${log_path}" ]; then
    local end_count
    end_count="$(log_count "${log_path}")"
    assert_log_unchanged "${start_count}" "${end_count}" "${label} log unchanged"
  fi
  echo "[PASS] ${label}" | tee -a "${LOG_FILE}"
}

check_log_value() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local path="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${path}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
path = sys.argv[3]
expected = sys.argv[4]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "data_explorer":
        entry = candidate
        break
if entry is None:
    fail("No data_explorer log entry found.")

value = entry
for part in path.split("."):
    if isinstance(value, dict) and part in value:
        value = value[part]
    else:
        value = None
        break

if expected == "-":
    sys.exit(0)

if isinstance(value, bool) and expected.lower() in ("true", "false"):
    exp = expected.lower() == "true"
    if value != exp:
        fail(f"Expected {path}={exp}, got {value}")
    sys.exit(0)

try:
    exp_num = float(expected)
    if isinstance(value, (int, float)):
        if value is None or abs(value - exp_num) > 1e-6:
            fail(f"Expected {path}≈{exp_num}, got {value}")
        sys.exit(0)
except ValueError:
    pass

if isinstance(value, list):
    value_text = ",".join([str(v) for v in value])
else:
    value_text = "" if value is None else str(value)

if value_text != expected:
    fail(f"Expected {path}='{expected}', got '{value_text}'")
PY
}

check_log_list_contains() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local path="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${path}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
path = sys.argv[3]
expected = sys.argv[4]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "data_explorer":
        entry = candidate
        break
if entry is None:
    fail("No data_explorer log entry found.")

value = entry
for part in path.split("."):
    if isinstance(value, dict) and part in value:
        value = value[part]
    else:
        value = None
        break

expected_items = [item.strip() for item in expected.split(",") if item.strip()]
if not expected_items:
    sys.exit(0)

if isinstance(value, list):
    actual_items = [str(item) for item in value if str(item)]
elif value is None:
    actual_items = []
else:
    actual_items = [item.strip() for item in str(value).split(",") if item.strip()]

missing = [item for item in expected_items if item not in actual_items]
if missing:
    fail(f"Expected {path} to include {missing}, got {actual_items}")
PY
}

check_overview_value() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local variable="$1"; shift
  local column="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${variable}" "${column}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


def compare(value, expected, label):
    if expected in ("-", ""):
        return
    if expected in ("NA", "null", "NULL"):
        if value is not None:
            fail(f"Expected {label} to be null/NA, got {value}")
        return
    if isinstance(value, bool) and expected.lower() in ("true", "false"):
        exp = expected.lower() == "true"
        if value != exp:
            fail(f"Expected {label}={exp}, got {value}")
        return
    try:
        exp_num = float(expected)
        if isinstance(value, (int, float)):
            if value is None or abs(value - exp_num) > 1e-6:
                fail(f"Expected {label}≈{exp_num}, got {value}")
            return
    except ValueError:
        pass
    if isinstance(value, list):
        value_text = ",".join([str(v) for v in value])
    else:
        value_text = "" if value is None else str(value)
    if value_text != expected:
        fail(f"Expected {label}='{expected}', got '{value_text}'")


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
variable = sys.argv[3]
column = sys.argv[4]
expected = sys.argv[5]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "data_explorer":
        entry = candidate
        break
if entry is None:
    fail("No data_explorer log entry found.")

overview = (entry.get("results") or {}).get("overview_df") or []
if not isinstance(overview, list):
    fail("overview_df is not a list.")

row = None
for candidate in overview:
    if candidate.get("variable") == variable:
        row = candidate
        break
if row is None:
    fail(f"Variable not found in overview_df: {variable}")

compare(row.get(column), expected, f"{variable}.{column}")
PY
}

check_levels_value() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local variable="$1"; shift
  local level="$1"; shift
  local column="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${variable}" "${level}" "${column}" "${expected}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


def compare(value, expected, label):
    if expected in ("-", ""):
        return
    if expected in ("NA", "null", "NULL"):
        if value is not None:
            fail(f"Expected {label} to be null/NA, got {value}")
        return
    if isinstance(value, bool) and expected.lower() in ("true", "false"):
        exp = expected.lower() == "true"
        if value != exp:
            fail(f"Expected {label}={exp}, got {value}")
        return
    try:
        exp_num = float(expected)
        if isinstance(value, (int, float)):
            if value is None or abs(value - exp_num) > 1e-6:
                fail(f"Expected {label}≈{exp_num}, got {value}")
            return
    except ValueError:
        pass
    if isinstance(value, list):
        value_text = ",".join([str(v) for v in value])
    else:
        value_text = "" if value is None else str(value)
    if value_text != expected:
        fail(f"Expected {label}='{expected}', got '{value_text}'")


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
variable = sys.argv[3]
level = sys.argv[4]
column = sys.argv[5]
expected = sys.argv[6]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "data_explorer":
        entry = candidate
        break
if entry is None:
    fail("No data_explorer log entry found.")

levels = (entry.get("results") or {}).get("levels_df") or []
if not isinstance(levels, list):
    fail("levels_df is not a list.")

row = None
for candidate in levels:
    if candidate.get("variable") == variable and candidate.get("level") == level:
        row = candidate
        break
if row is None:
    fail(f"Level not found in levels_df: {variable} / {level}")

compare(row.get(column), expected, f"{variable}.{level}.{column}")
PY
}

check_levels_absent() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local variable="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${variable}" <<'PY'
import json
import sys
from pathlib import Path


def load_entries(path, start_count):
    entries = []
    with path.open("r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle, start=1):
            if idx <= start_count:
                continue
            if line.strip():
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return entries


def fail(message):
    print(message)
    sys.exit(1)


log_path = Path(sys.argv[1])
start_count = int(sys.argv[2])
variable = sys.argv[3]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "data_explorer":
        entry = candidate
        break
if entry is None:
    fail("No data_explorer log entry found.")

levels = (entry.get("results") or {}).get("levels_df") or []
if not isinstance(levels, list):
    sys.exit(0)

for candidate in levels:
    if candidate.get("variable") == variable:
        fail(f"Did not expect levels_df rows for variable: {variable}")
PY
}

cat >"${CUSTOM_CSV}" <<'DATA_CSV'
age,gender,segment
23,M,A
25,F,B
22,F,C
26,M,D
24,M,A
DATA_CSV

run_ok "prepare test datasets" Rscript - \
  "${TYPED_RDS}" \
  "${TYPED_RDATA}" \
  "${TYPED_DF_NAME}" \
  "${SEMI_CSV}" \
  "${NO_HEADER_CSV}" \
  "${ROUNDING1_CSV}" \
  "${ROUNDING2_CSV}" \
  "${BAD_RDS}" \
  "${BAD_RDATA}" \
  "${BAD_RDATA_DF}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
typed_rds <- args[1]
typed_rdata <- args[2]
typed_df_name <- args[3]
semicolon_csv <- args[4]
no_header_csv <- args[5]
rounding1_csv <- args[6]
rounding2_csv <- args[7]
bad_rds <- args[8]
bad_rdata <- args[9]
bad_rdata_df <- args[10]

typed_df <- data.frame(
  date_var = as.Date("2023-01-01") + 0:4,
  ordered_var = ordered(c("low", "med", "high", "low", "med"), levels = c("low", "med", "high")),
  integer_like = c(1, 2, 3, 1, 2),
  numeric_high = c(0.1, 0.2, 0.3, 0.4, 0.5),
  logical_var = c(TRUE, FALSE, NA, TRUE, FALSE),
  all_missing = as.numeric(c(NA, NA, NA, NA, NA)),
  numeric_missing = c(1.11, 1.13, NA, 1.15, 1.17),
  stringsAsFactors = FALSE
)
saveRDS(typed_df, typed_rds)
assign(typed_df_name, typed_df)
save(list = typed_df_name, file = typed_rdata)

semicolon_df <- data.frame(alpha = c("A", "B", "C"), score = c(10, 20, 30), stringsAsFactors = FALSE)
write.table(semicolon_df, semicolon_csv, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)

no_header_df <- data.frame(num = c(1, 2, 3), grp = c("G1", "G2", "G1"), stringsAsFactors = FALSE)
write.table(no_header_df, no_header_csv, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)

rounding_df <- data.frame(score = c(1.11, 1.13, 1.11, 1.13), stringsAsFactors = FALSE)
write.csv(rounding_df, rounding1_csv, row.names = FALSE)
write.csv(rounding_df, rounding2_csv, row.names = FALSE)

saveRDS(c(1, 2, 3), bad_rds)

assign(bad_rdata_df, c(1, 2, 3))
save(list = bad_rdata_df, file = bad_rdata)
RS

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript - "${TYPED_RDS}" "${SAV_PATH}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
rds_path <- args[1]
sav_path <- args[2]
library(haven)
df <- readRDS(rds_path)
write_sav(df, sav_path)
RS
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/data_explorer.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Data explorer (base R)"

run_ok "init workspace" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

start=$(log_count "${LOG_PATH}")
run_ok "parquet input" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --parquet "${PARQUET_GOLDEN}" \
  --vars id,site,group3 \
  --max-levels 5 \
  --top-n 3 \
  --digits 1 \
  --user-prompt "data explorer test prompt"

if [ ! -f "${NLSS_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${NLSS_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

assert_contains "${NLSS_REPORT_PATH}" "Data Exploration"
assert_contains "${NLSS_REPORT_PATH}" "Variable Overview"
assert_contains "${NLSS_REPORT_PATH}" "Value Levels"
assert_contains "${NLSS_REPORT_PATH}" "Table 1"
assert_contains "${NLSS_REPORT_PATH}" "Table 2"

check_log_list_contains "${LOG_PATH}" "${start}" "options.vars" "id,site,group3"
check_log_value "${LOG_PATH}" "${start}" "options.max_levels" "5"
check_log_value "${LOG_PATH}" "${start}" "options.top_n" "3"
check_log_value "${LOG_PATH}" "${start}" "options.digits" "1"
check_log_value "${LOG_PATH}" "${start}" "user_prompt" "data explorer test prompt"

start=$(log_count "${LOG_PATH}")
run_ok "env user prompt" env CODEX_USER_PROMPT="env prompt value" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --parquet "${PARQUET_GOLDEN}" \
  --vars id,site \
  --log TRUE
check_log_value "${LOG_PATH}" "${start}" "user_prompt" "env prompt value"

start=$(log_count "${LOG_PATH}")
run_ok "workspace default input" bash -c "cd \"${DATASET_DIR}\" && Rscript \"${R_SCRIPT_DIR}/data_explorer.R\""
check_log_list_contains "${LOG_PATH}" "${start}" "options.vars" "id,site"

start=$(log_count "${LOG_PATH}")
run_ok "log disabled" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --parquet "${PARQUET_GOLDEN}" \
  --vars id,site \
  --log FALSE
end=$(log_count "${LOG_PATH}")
assert_log_unchanged "${start}" "${end}" "log disabled"

start=$(log_count "${CUSTOM_LOG_PATH}")
run_ok "csv truncation" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --csv "${CUSTOM_CSV}" \
  --vars age,gender,segment \
  --max-levels 2 \
  --top-n 1 \
  --digits 1

if [ ! -f "${CUSTOM_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${CUSTOM_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi

assert_contains "${CUSTOM_REPORT_PATH}" "Other (remaining)"
check_log_list_contains "${CUSTOM_LOG_PATH}" "${start}" "options.vars" "age,gender,segment"
check_log_value "${CUSTOM_LOG_PATH}" "${start}" "options.max_levels" "2"
check_log_value "${CUSTOM_LOG_PATH}" "${start}" "options.top_n" "1"
check_overview_value "${CUSTOM_LOG_PATH}" "${start}" "segment" "levels_truncated" "true"
check_overview_value "${CUSTOM_LOG_PATH}" "${start}" "segment" "levels_note" "levels truncated to top 1"

start=$(log_count "${SEMI_LOG_PATH}")
run_ok "csv semicolon" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --csv "${SEMI_CSV}" \
  --sep ";" \
  --vars alpha,score
if [ ! -f "${SEMI_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${SEMI_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
assert_contains "${SEMI_REPORT_PATH}" "alpha"
assert_contains "${SEMI_REPORT_PATH}" "score"

start=$(log_count "${NO_HEADER_LOG_PATH}")
run_ok "csv no header" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --csv "${NO_HEADER_CSV}" \
  --header FALSE \
  --vars V1,V2
if [ ! -f "${NO_HEADER_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${NO_HEADER_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
assert_contains "${NO_HEADER_REPORT_PATH}" "V1"
check_log_list_contains "${NO_HEADER_LOG_PATH}" "${start}" "options.vars" "V1,V2"

start=$(log_count "${ROUNDING1_LOG_PATH}")
run_ok "rounding digits 2" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --csv "${ROUNDING1_CSV}" \
  --vars score \
  --digits 2
if [ ! -f "${ROUNDING1_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${ROUNDING1_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
assert_contains "${ROUNDING1_REPORT_PATH}" "1.12"
check_log_value "${ROUNDING1_LOG_PATH}" "${start}" "options.digits" "2"

start=$(log_count "${ROUNDING2_LOG_PATH}")
run_ok "rounding digits 1" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --csv "${ROUNDING2_CSV}" \
  --vars score \
  --digits 1
if [ ! -f "${ROUNDING2_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${ROUNDING2_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
assert_contains "${ROUNDING2_REPORT_PATH}" "1.1"
assert_not_contains "${ROUNDING2_REPORT_PATH}" "1.12"
check_log_value "${ROUNDING2_LOG_PATH}" "${start}" "options.digits" "1"

start=$(log_count "${TYPED_RDS_LOG_PATH}")
run_ok "rds typed default vars" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --rds "${TYPED_RDS}" \
  --max-levels 3 \
  --top-n 2
if [ ! -f "${TYPED_RDS_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${TYPED_RDS_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
check_log_list_contains "${TYPED_RDS_LOG_PATH}" "${start}" "options.vars" "date_var,ordered_var,integer_like,numeric_high,logical_var,all_missing,numeric_missing"
check_log_value "${TYPED_RDS_LOG_PATH}" "${start}" "options.max_levels" "3"
check_log_value "${TYPED_RDS_LOG_PATH}" "${start}" "options.top_n" "2"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "date_var" "measurement_level" "interval"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "date_var" "measurement_note" "date/time"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "ordered_var" "measurement_level" "ordinal"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "ordered_var" "measurement_note" "ordered factor"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "integer_like" "measurement_level" "ordinal"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "integer_like" "measurement_note" "integer-like with few levels"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "numeric_high" "measurement_level" "interval/ratio"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "numeric_high" "measurement_note" "numeric"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "logical_var" "measurement_level" "nominal"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "logical_var" "measurement_note" "categorical"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "numeric_high" "levels_included" "false"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "numeric_high" "levels_note" "levels not listed (unique_n = 5)"
check_levels_absent "${TYPED_RDS_LOG_PATH}" "${start}" "numeric_high"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "all_missing" "levels_note" "no valid data"
check_levels_value "${TYPED_RDS_LOG_PATH}" "${start}" "all_missing" "(no valid data)" "n" "0"
check_overview_value "${TYPED_RDS_LOG_PATH}" "${start}" "logical_var" "missing_n" "1"

start=$(log_count "${TYPED_RDATA_LOG_PATH}")
run_ok "rdata typed df" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --rdata "${TYPED_RDATA}" \
  --df "${TYPED_DF_NAME}" \
  --vars date_var,ordered_var
if [ ! -f "${TYPED_RDATA_REPORT_PATH}" ]; then
  echo "[FAIL] missing report: ${TYPED_RDATA_REPORT_PATH}" | tee -a "${LOG_FILE}"
  exit 1
fi
check_log_list_contains "${TYPED_RDATA_LOG_PATH}" "${start}" "options.vars" "date_var,ordered_var"

cat >"${INTERACTIVE_CSV_INPUT}" <<INPUT_DATA
csv
${CUSTOM_CSV}
,
TRUE
age,gender,segment
1
2
1

data explorer interactive
TRUE
INPUT_DATA

start=$(log_count "${CUSTOM_LOG_PATH}")
run_ok "interactive csv" env NLSS_PROMPT_FILE="${INTERACTIVE_CSV_INPUT}" Rscript "${R_SCRIPT_DIR}/data_explorer.R" --interactive
check_log_value "${CUSTOM_LOG_PATH}" "${start}" "user_prompt" "data explorer interactive"

cat >"${INTERACTIVE_RDATA_INPUT}" <<INPUT_DATA
rdata
${TYPED_RDATA}
${TYPED_DF_NAME}
date_var,ordered_var
2
3
2

interactive rdata
TRUE
INPUT_DATA

start=$(log_count "${TYPED_RDATA_LOG_PATH}")
run_ok "interactive rdata" env NLSS_PROMPT_FILE="${INTERACTIVE_RDATA_INPUT}" Rscript "${R_SCRIPT_DIR}/data_explorer.R" --interactive
check_log_value "${TYPED_RDATA_LOG_PATH}" "${start}" "user_prompt" "interactive rdata"
check_log_list_contains "${TYPED_RDATA_LOG_PATH}" "${start}" "options.vars" "date_var,ordered_var"

cat >"${INTERACTIVE_BAD_INPUT}" <<INPUT_DATA
badtype
INPUT_DATA

run_expect_fail "interactive invalid input" "${LOG_PATH}" \
  env NLSS_PROMPT_FILE="${INTERACTIVE_BAD_INPUT}" Rscript "${R_SCRIPT_DIR}/data_explorer.R" --interactive

if [ "${HAS_HAVEN}" -eq 1 ]; then
  start=$(log_count "${SAV_LOG_PATH}")
  run_ok "sav input" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
    --sav "${SAV_PATH}" \
    --vars date_var,ordered_var
  if [ ! -f "${SAV_REPORT_PATH}" ]; then
    echo "[FAIL] missing report: ${SAV_REPORT_PATH}" | tee -a "${LOG_FILE}"
    exit 1
  fi
  check_log_list_contains "${SAV_LOG_PATH}" "${start}" "options.vars" "date_var,ordered_var"
fi

run_expect_fail "unknown vars" "${LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/data_explorer.R" --parquet "${PARQUET_GOLDEN}" --vars not_a_var
run_expect_fail "rdata missing df" "${TYPED_RDATA_LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/data_explorer.R" --rdata "${TYPED_RDATA}"
run_expect_fail "rdata not data frame" "${BAD_RDATA_LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/data_explorer.R" --rdata "${BAD_RDATA}" --df "${BAD_RDATA_DF}"
run_expect_fail "rds not data frame" "${BAD_RDS_LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/data_explorer.R" --rds "${BAD_RDS}"

TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.data_explorer.default)"
if [ -z "${TEMPLATE_DEFAULT_ORIG}" ]; then
  TEMPLATE_DEFAULT_ORIG="data-explorer/default-template.md"
fi
TEMPLATE_DEFAULT_TMP="${TMP_BASE}/data_explorer_template.md"

cat >"${TEMPLATE_DEFAULT_TMP}" <<'EOF'
---
tokens:
  title: "Data Explorer TEMPLATE OVERRIDE"
  overview_table_title: "Overview Override."
  levels_table_title: "Levels Override."
  note_prefix: "*Note-OVR.*"
  narrative_heading: "**Narrative Override**"
tables:
  overview:
    columns:
      - key: "variable"
        label: "Var"
      - key: "measurement_level"
        label: "Scale"
      - key: "not_a_col"
        label: "DropMe"
        drop_if_empty: true
  levels:
    columns:
      - key: "variable"
        label: "Var"
      - key: "level"
        label: "Level"
      - key: "n"
        label: "n"
      - key: "drop_level"
        label: "DropLevel"
        drop_if_empty: true
narrative:
  template: "OVERRIDE NARRATIVE: {{narrative_default}}"
---
# {{title}}

{{analysis_flags}}

**Table {{table_number}}**

{{overview_table_title}}

{{overview_table_body}}

{{note_prefix}} {{overview_note_body}}

**Table {{table_number_next}}**

{{levels_table_title}}

{{levels_table_body}}

{{note_prefix}} {{levels_note_body}}

{{narrative_heading}}

{{narrative}}
EOF

set_config_value templates.data_explorer.default "${TEMPLATE_DEFAULT_TMP}"

run_ok "template override" Rscript "${R_SCRIPT_DIR}/data_explorer.R" \
  --parquet "${PARQUET_GOLDEN}" \
  --vars id,site,group3

assert_contains "${NLSS_REPORT_PATH}" "Data Explorer TEMPLATE OVERRIDE"
assert_contains "${NLSS_REPORT_PATH}" "Overview Override."
assert_contains "${NLSS_REPORT_PATH}" "Levels Override."
assert_contains "${NLSS_REPORT_PATH}" "*Note-OVR.*"
assert_contains "${NLSS_REPORT_PATH}" "**Narrative Override**"
assert_contains "${NLSS_REPORT_PATH}" "OVERRIDE NARRATIVE:"
assert_contains "${NLSS_REPORT_PATH}" "| Var | Scale |"
assert_contains "${NLSS_REPORT_PATH}" "| Var | Level | n |"
assert_not_contains "${NLSS_REPORT_PATH}" "DropMe"
assert_not_contains "${NLSS_REPORT_PATH}" "DropLevel"

set_config_value templates.data_explorer.default "${TEMPLATE_DEFAULT_ORIG}"

echo "data_explorer tests: OK" | tee -a "${LOG_FILE}"
