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

WORKSPACE_DIR="${RUN_ROOT}/plot_workspace"
WORKSPACE_MANIFEST_PATH="${WORKSPACE_DIR}/${WORKSPACE_MANIFEST_NAME}"
TMP_BASE="${RUN_ROOT}/tmp/plot"
LOG_FILE="${RUN_ROOT}/plot_test.log"
SNAPSHOT_DIR="${RUN_ROOT}/plot_snapshots"

mkdir -p "${RUN_ROOT}"
mkdir -p "${SNAPSHOT_DIR}"

DATASET_LABEL="$(basename "${DATA_GOLDEN}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
DATASET_DIR="${WORKSPACE_DIR}/${DATASET_LABEL}"
PLOTS_DIR="${DATASET_DIR}/plots"
NLSS_REPORT_PATH="${DATASET_DIR}/report_canonical.md"
LOG_PATH="${DATASET_DIR}/analysis_log.jsonl"
PARQUET_GOLDEN="${DATASET_DIR}/${DATASET_LABEL}.parquet"

RDS_PATH="${TMP_BASE}/plot_rds.rds"
RDS_LABEL="plot_rds"
RDS_DIR="${WORKSPACE_DIR}/${RDS_LABEL}"
RDS_PLOTS_DIR="${RDS_DIR}/plots"
RDS_NLSS_REPORT_PATH="${RDS_DIR}/report_canonical.md"
RDS_LOG_PATH="${RDS_DIR}/analysis_log.jsonl"

RDATA_PATH="${TMP_BASE}/plot_rdata.RData"
RDATA_DF_NAME="plot_rdata"
RDATA_LABEL="${RDATA_DF_NAME}"
RDATA_DIR="${WORKSPACE_DIR}/${RDATA_LABEL}"
RDATA_PLOTS_DIR="${RDATA_DIR}/plots"
RDATA_NLSS_REPORT_PATH="${RDATA_DIR}/report_canonical.md"
RDATA_LOG_PATH="${RDATA_DIR}/analysis_log.jsonl"

SAV_PATH="${TMP_BASE}/plot_sav.sav"
SAV_LABEL="plot_sav"
SAV_DIR="${WORKSPACE_DIR}/${SAV_LABEL}"
SAV_PLOTS_DIR="${SAV_DIR}/plots"
SAV_NLSS_REPORT_PATH="${SAV_DIR}/report_canonical.md"
SAV_LOG_PATH="${SAV_DIR}/analysis_log.jsonl"

MISSING_CSV="${TMP_BASE}/plot_missing.csv"
MISSING_LABEL="plot_missing"
MISSING_DIR="${WORKSPACE_DIR}/${MISSING_LABEL}"
MISSING_PLOTS_DIR="${MISSING_DIR}/plots"
MISSING_NLSS_REPORT_PATH="${MISSING_DIR}/report_canonical.md"
MISSING_LOG_PATH="${MISSING_DIR}/analysis_log.jsonl"
MISSING_PARQUET="${MISSING_DIR}/${MISSING_LABEL}.parquet"

SMALL_CSV="${TMP_BASE}/plot_small.csv"
SMALL_LABEL="plot_small"
SMALL_DIR="${WORKSPACE_DIR}/${SMALL_LABEL}"
SMALL_PLOTS_DIR="${SMALL_DIR}/plots"
SMALL_NLSS_REPORT_PATH="${SMALL_DIR}/report_canonical.md"
SMALL_LOG_PATH="${SMALL_DIR}/analysis_log.jsonl"
SMALL_PARQUET="${SMALL_DIR}/${SMALL_LABEL}.parquet"

SEMI_CSV="${TMP_BASE}/plot_semicolon.csv"
SEMI_LABEL="plot_semicolon"
SEMI_DIR="${WORKSPACE_DIR}/${SEMI_LABEL}"
SEMI_PLOTS_DIR="${SEMI_DIR}/plots"
SEMI_NLSS_REPORT_PATH="${SEMI_DIR}/report_canonical.md"
SEMI_LOG_PATH="${SEMI_DIR}/analysis_log.jsonl"
SEMI_PARQUET="${SEMI_DIR}/${SEMI_LABEL}.parquet"

NO_HEADER_CSV="${TMP_BASE}/plot_no_header.csv"
NO_HEADER_LABEL="plot_no_header"
NO_HEADER_DIR="${WORKSPACE_DIR}/${NO_HEADER_LABEL}"
NO_HEADER_PLOTS_DIR="${NO_HEADER_DIR}/plots"
NO_HEADER_NLSS_REPORT_PATH="${NO_HEADER_DIR}/report_canonical.md"
NO_HEADER_LOG_PATH="${NO_HEADER_DIR}/analysis_log.jsonl"
NO_HEADER_PARQUET="${NO_HEADER_DIR}/${NO_HEADER_LABEL}.parquet"

INTERACTIVE_CSV="${TMP_BASE}/plot_interactive.csv"
INTERACTIVE_LABEL="plot_interactive"
INTERACTIVE_DIR="${WORKSPACE_DIR}/${INTERACTIVE_LABEL}"
INTERACTIVE_PLOTS_DIR="${INTERACTIVE_DIR}/plots"
INTERACTIVE_NLSS_REPORT_PATH="${INTERACTIVE_DIR}/report_canonical.md"
INTERACTIVE_LOG_PATH="${INTERACTIVE_DIR}/analysis_log.jsonl"
INTERACTIVE_INPUT="${TMP_BASE}/interactive_input.txt"

HELP_PATH="${TMP_BASE}/plot_help.txt"

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

snapshot_outputs() {
  local label="$1"
  local report_path="$2"
  local plots_dir="$3"
  local log_path="$4"
  local dest="${SNAPSHOT_DIR}/${label}"
  mkdir -p "${dest}"
  if [ -f "${report_path}" ]; then
    cp "${report_path}" "${dest}/report_canonical.md"
  fi
  if [ -f "${log_path}" ]; then
    cp "${log_path}" "${dest}/analysis_log.jsonl"
  fi
  if [ -d "${plots_dir}" ]; then
    rm -rf "${dest}/plots"
    cp -R "${plots_dir}" "${dest}/plots"
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

assert_glob() {
  local pattern="$1"
  if ! compgen -G "$pattern" >/dev/null; then
    echo "Expected files for pattern: $pattern" | tee -a "${LOG_FILE}"
    exit 1
  fi
}

assert_no_glob() {
  local pattern="$1"
  if compgen -G "$pattern" >/dev/null; then
    echo "Did not expect files for pattern: $pattern" | tee -a "${LOG_FILE}"
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
    if candidate.get("module") == "plot":
        entry = candidate
        break
if entry is None:
    fail("No plot log entry found.")

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
    if candidate.get("module") == "plot":
        entry = candidate
        break
if entry is None:
    fail("No plot log entry found.")

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

check_figure_count() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${expected}" <<'PY'
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
expected = int(sys.argv[3])

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "plot":
        entry = candidate
        break
if entry is None:
    fail("No plot log entry found.")

figures = (entry.get("results") or {}).get("figures") or []
if not isinstance(figures, list):
    fail("results.figures is not a list")

if len(figures) != expected:
    fail(f"Expected {expected} figures, got {len(figures)}")
PY
}

check_figure_value() {
  local log_path="$1"; shift
  local start_count="$1"; shift
  local index="$1"; shift
  local key="$1"; shift
  local expected="$1"; shift
  "${PYTHON_BIN}" - "${log_path}" "${start_count}" "${index}" "${key}" "${expected}" <<'PY'
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
index = int(sys.argv[3])
key = sys.argv[4]
expected = sys.argv[5]

entries = load_entries(log_path, start_count)
if not entries:
    fail("No new log entries found.")

entry = None
for candidate in reversed(entries):
    if candidate.get("module") == "plot":
        entry = candidate
        break
if entry is None:
    fail("No plot log entry found.")

figures = (entry.get("results") or {}).get("figures") or []
if not isinstance(figures, list) or not figures:
    fail("results.figures is empty")

if index < 0 or index >= len(figures):
    fail(f"Figure index {index} out of range")

value = figures[index].get(key)

if expected == "-":
    sys.exit(0)

if isinstance(value, bool) and expected.lower() in ("true", "false"):
    exp = expected.lower() == "true"
    if value != exp:
        fail(f"Expected {key}={exp}, got {value}")
    sys.exit(0)

try:
    exp_num = float(expected)
    if isinstance(value, (int, float)):
        if value is None or abs(value - exp_num) > 1e-6:
            fail(f"Expected {key}≈{exp_num}, got {value}")
        sys.exit(0)
except ValueError:
    pass

value_text = "" if value is None else str(value)
if value_text != expected:
    fail(f"Expected {key}='{expected}', got '{value_text}'")
PY
}

cat >"${MISSING_CSV}" <<'DATA_CSV'
segment,score
A,1
NA,2
B,3
NA,4
DATA_CSV

cat >"${SMALL_CSV}" <<'DATA_CSV'
num1,num2,cat
1,4,A
2,5,B
3,6,A
4,7,B
DATA_CSV

cat >"${SEMI_CSV}" <<'DATA_CSV'
category;value
A;1
B;2
A;3
DATA_CSV

cat >"${NO_HEADER_CSV}" <<'DATA_CSV'
A,1
B,2
A,3
DATA_CSV

cat >"${INTERACTIVE_CSV}" <<'DATA_CSV'
value,group
1,A
2,B
3,A
4,B
DATA_CSV

run_ok "prepare rds/rdata" Rscript - "${DATA_GOLDEN}" "${RDS_PATH}" "${RDATA_PATH}" "${RDATA_DF_NAME}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
rds_path <- args[2]
rdata_path <- args[3]
rdata_df <- args[4]

df <- read.csv(input_path, stringsAsFactors = FALSE)
saveRDS(df, rds_path)
assign(rdata_df, df)
save(list = rdata_df, file = rdata_path)
RS

HAS_HAVEN=0
if Rscript "${CHECK_R_PACKAGE_SCRIPT}" haven >/dev/null 2>&1; then
  HAS_HAVEN=1
fi
if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "prepare sav" Rscript - "${DATA_GOLDEN}" "${SAV_PATH}" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
sav_path <- args[2]
library(haven)
df <- read.csv(input_path, stringsAsFactors = FALSE)
write_sav(df, sav_path)
RS
else
  echo "[SKIP] sav input (haven not installed)" | tee -a "${LOG_FILE}"
fi

run_ok "help text" bash -c "Rscript \"${R_SCRIPT_DIR}/plot.R\" --help > \"${HELP_PATH}\" 2>&1"
assert_contains "${HELP_PATH}" "Plots (ggplot2)"
assert_contains "${HELP_PATH}" "corr-heatmap"

run_ok "init workspace (golden)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${DATA_GOLDEN}"

if [ ! -f "${PARQUET_GOLDEN}" ]; then
  echo "[FAIL] missing parquet copy: ${PARQUET_GOLDEN}" | tee -a "${LOG_FILE}"
  exit 1
fi

rm -f "${NLSS_REPORT_PATH}" "${LOG_PATH}"
rm -rf "${PLOTS_DIR}"

start=$(log_count "${LOG_PATH}")
run_ok "histogram workspace default" bash -c "cd \"${DATASET_DIR}\" && Rscript \"${R_SCRIPT_DIR}/plot.R\" --type histogram --vars age --bins 12"
assert_glob "${PLOTS_DIR}/figure-001-histogram-age*.png"
assert_contains "${NLSS_REPORT_PATH}" "Figure 1."
check_log_value "${LOG_PATH}" "${start}" "options.type" "histogram"
check_log_value "${LOG_PATH}" "${start}" "options.bins" "12"
check_log_list_contains "${LOG_PATH}" "${start}" "options.vars" "age"
check_figure_count "${LOG_PATH}" "${start}" "1"
check_figure_value "${LOG_PATH}" "${start}" "0" "plot_type" "histogram"

start=$(log_count "${LOG_PATH}")
run_ok "bar percent auto" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type bar --vars gender --group group3 --stat percent --percent-base group --position stack --na-action keep --user-prompt "plot test prompt"
assert_glob "${PLOTS_DIR}/figure-002-bar-gender-group3*.png"
assert_contains "${NLSS_REPORT_PATH}" "Figure 2."
check_log_value "${LOG_PATH}" "${start}" "options.stat" "percent"
check_log_value "${LOG_PATH}" "${start}" "options.percent_base" "group"
check_log_value "${LOG_PATH}" "${start}" "options.position" "stack"
check_log_value "${LOG_PATH}" "${start}" "options.na_action" "keep"
check_log_value "${LOG_PATH}" "${start}" "options.group" "group3"
check_log_value "${LOG_PATH}" "${start}" "user_prompt" "plot test prompt"
check_figure_value "${LOG_PATH}" "${start}" "0" "plot_type" "bar"

HAS_SVG=0
if Rscript -e "quit(status = if (requireNamespace('ggplot2', quietly=TRUE) && requireNamespace('svglite', quietly=TRUE)) 0 else 1)" >/dev/null 2>&1; then
  HAS_SVG=1
fi

SCATTER_FORMAT="png"
SCATTER_EXT="png"
if [ "${HAS_SVG}" -eq 1 ]; then
  SCATTER_FORMAT="svg"
  SCATTER_EXT="svg"
else
  echo "[SKIP] svg format (svglite not available for ggplot2)" | tee -a "${LOG_FILE}"
fi

start=$(log_count "${LOG_PATH}")
run_ok "scatter smooth" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type scatter --x x1 --y outcome_reg --group group2 --smooth loess --se FALSE --span 0.5 --palette viridis --theme bw --format "${SCATTER_FORMAT}"
assert_glob "${PLOTS_DIR}/figure-003-scatter-x1-outcome_reg-group2*.${SCATTER_EXT}"
assert_contains "${NLSS_REPORT_PATH}" "Figure 3."
check_log_value "${LOG_PATH}" "${start}" "options.smooth" "loess"
check_log_value "${LOG_PATH}" "${start}" "options.se" "false"
check_log_value "${LOG_PATH}" "${start}" "options.span" "0.5"
check_log_value "${LOG_PATH}" "${start}" "options.palette" "viridis"
check_log_value "${LOG_PATH}" "${start}" "options.theme" "bw"
check_log_value "${LOG_PATH}" "${start}" "options.format" "${SCATTER_FORMAT}"

snapshot_outputs "golden_before_line_summary" "${NLSS_REPORT_PATH}" "${PLOTS_DIR}" "${LOG_PATH}"
rm -f "${NLSS_REPORT_PATH}"
rm -rf "${PLOTS_DIR}"

start=$(log_count "${LOG_PATH}")
run_ok "line summary prefix" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type line --x pre_score --y post_score --summary mean --file-prefix linefig --file-suffix demo --figure-number 10 --format pdf --width 6 --height 4 --dpi 200
assert_glob "${PLOTS_DIR}/linefig-010-line-pre_score-post_score-demo.pdf"
assert_contains "${NLSS_REPORT_PATH}" "Figure 10."
check_log_value "${LOG_PATH}" "${start}" "options.summary" "mean"
check_log_value "${LOG_PATH}" "${start}" "options.file_prefix" "linefig"
check_log_value "${LOG_PATH}" "${start}" "options.file_suffix" "demo"
check_log_value "${LOG_PATH}" "${start}" "options.format" "pdf"
check_log_value "${LOG_PATH}" "${start}" "options.width" "6"
check_log_value "${LOG_PATH}" "${start}" "options.height" "4"
check_log_value "${LOG_PATH}" "${start}" "options.dpi" "200"
check_figure_value "${LOG_PATH}" "${start}" "0" "figure_number" "10"

start=$(log_count "${LOG_PATH}")
run_ok "overwrite base" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --figure-number 25 --file-prefix dupfig --file-suffix repeat --binwidth 1
run_ok "overwrite unique" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --figure-number 25 --file-prefix dupfig --file-suffix repeat --binwidth 1
assert_glob "${PLOTS_DIR}/dupfig-025-histogram-age-repeat-1.png"
check_log_value "${LOG_PATH}" "${start}" "options.binwidth" "1"

run_ok "init workspace (rds)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --rds "${RDS_PATH}"
rm -f "${RDS_NLSS_REPORT_PATH}" "${RDS_LOG_PATH}"
rm -rf "${RDS_PLOTS_DIR}"

start=$(log_count "${RDS_LOG_PATH}")
run_ok "density rds" Rscript "${R_SCRIPT_DIR}/plot.R" --rds "${RDS_PATH}" --type density --vars skewed_var --group group2 --bw 0.5 --alpha 0.4 --figure-number 20
assert_glob "${RDS_PLOTS_DIR}/figure-020-density-skewed_var-group2*.png"
check_log_value "${RDS_LOG_PATH}" "${start}" "options.bw" "0.5"
check_log_value "${RDS_LOG_PATH}" "${start}" "options.alpha" "0.4"
check_log_value "${RDS_LOG_PATH}" "${start}" "options.group" "group2"
check_figure_value "${RDS_LOG_PATH}" "${start}" "0" "plot_type" "density"

run_ok "init workspace (rdata)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF_NAME}"
rm -f "${RDATA_NLSS_REPORT_PATH}" "${RDATA_LOG_PATH}"
rm -rf "${RDATA_PLOTS_DIR}"

start=$(log_count "${RDATA_LOG_PATH}")
run_ok "box rdata" Rscript "${R_SCRIPT_DIR}/plot.R" --rdata "${RDATA_PATH}" --df "${RDATA_DF_NAME}" --type box --vars outcome_anova --group group2 --figure-number 30
assert_glob "${RDATA_PLOTS_DIR}/figure-030-box-*.png"
check_log_value "${RDATA_LOG_PATH}" "${start}" "options.type" "box"
check_log_value "${RDATA_LOG_PATH}" "${start}" "options.group" "group2"
check_figure_value "${RDATA_LOG_PATH}" "${start}" "0" "plot_type" "box"

if [ "${HAS_HAVEN}" -eq 1 ]; then
  run_ok "init workspace (sav)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --sav "${SAV_PATH}"
  rm -f "${SAV_NLSS_REPORT_PATH}" "${SAV_LOG_PATH}"
  rm -rf "${SAV_PLOTS_DIR}"

  start=$(log_count "${SAV_LOG_PATH}")
  run_ok "violin sav" Rscript "${R_SCRIPT_DIR}/plot.R" --sav "${SAV_PATH}" --type violin --vars outcome_anova --group group2 --palette greys --figure-number 40
  assert_glob "${SAV_PLOTS_DIR}/figure-040-violin-*.png"
  check_log_value "${SAV_LOG_PATH}" "${start}" "options.palette" "greys"
  check_figure_value "${SAV_LOG_PATH}" "${start}" "0" "plot_type" "violin"
else
  echo "[SKIP] sav plot coverage (haven not installed)" | tee -a "${LOG_FILE}"
fi

start=$(log_count "${LOG_PATH}")
run_ok "qq log disabled" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type qq --vars skewed_var --figure-number 50 --log FALSE
assert_glob "${PLOTS_DIR}/figure-050-qq-skewed_var*.png"
end=$(log_count "${LOG_PATH}")
assert_log_unchanged "${start}" "${end}" "qq log disabled"

start=$(log_count "${LOG_PATH}")
run_ok "corr-heatmap parquet" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type corr-heatmap --vars x1,x2,x3 --digits 3 --figure-number 60
assert_glob "${PLOTS_DIR}/figure-060-corr-heatmap*.png"
check_log_value "${LOG_PATH}" "${start}" "options.type" "corr-heatmap"
check_log_list_contains "${LOG_PATH}" "${start}" "options.vars" "x1,x2,x3"
check_figure_value "${LOG_PATH}" "${start}" "0" "plot_type" "corr-heatmap"

start=$(log_count "${LOG_PATH}")
run_ok "scatter smooth lm" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type scatter --x x2 --y outcome_reg --smooth lm --se TRUE --format png --figure-number 65
assert_glob "${PLOTS_DIR}/figure-065-scatter-x2-outcome_reg*.png"
check_log_value "${LOG_PATH}" "${start}" "options.smooth" "lm"
check_log_value "${LOG_PATH}" "${start}" "options.se" "true"

start=$(log_count "${LOG_PATH}")
run_ok "bar percent total fill" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type bar --vars gender --group group2 --stat percent --percent-base total --position fill --figure-number 66
assert_glob "${PLOTS_DIR}/figure-066-bar-gender-group2*.png"
check_log_value "${LOG_PATH}" "${start}" "options.percent_base" "total"
check_log_value "${LOG_PATH}" "${start}" "options.position" "fill"
check_log_value "${LOG_PATH}" "${start}" "options.group" "group2"

start=$(log_count "${LOG_PATH}")
run_ok "line median group" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type line --x pre_score --y post_score --group group2 --summary median --figure-number 67
assert_glob "${PLOTS_DIR}/figure-067-line-pre_score-post_score-group2*.png"
check_log_value "${LOG_PATH}" "${start}" "options.summary" "median"
check_log_value "${LOG_PATH}" "${start}" "options.group" "group2"

start=$(log_count "${LOG_PATH}")
run_ok "caption note override" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --title "Override Title" --subtitle "Override Subtitle" --caption "Override Caption" --note "Override Note." --figure-number 68
assert_contains "${NLSS_REPORT_PATH}" "Override Caption"
assert_contains "${NLSS_REPORT_PATH}" "Override Note."

overwrite_base="${PLOTS_DIR}/overwritefig-069-histogram-age-exact.png"
overwrite_dup="${PLOTS_DIR}/overwritefig-069-histogram-age-exact-1.png"
rm -f "${overwrite_base}" "${overwrite_dup}"
run_ok "overwrite false initial" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --figure-number 69 --file-prefix overwritefig --file-suffix exact
run_ok "overwrite true" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --figure-number 69 --file-prefix overwritefig --file-suffix exact --overwrite TRUE
assert_glob "${overwrite_base}"
assert_no_glob "${overwrite_dup}"

run_ok "init workspace (small)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${SMALL_CSV}"
rm -f "${SMALL_NLSS_REPORT_PATH}" "${SMALL_LOG_PATH}"
rm -rf "${SMALL_PLOTS_DIR}"

start=$(log_count "${SMALL_LOG_PATH}")
run_ok "auto default vars multi" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${SMALL_PARQUET}" --figure-number 5
assert_glob "${SMALL_PLOTS_DIR}/figure-005-histogram-num1*.png"
assert_glob "${SMALL_PLOTS_DIR}/figure-006-histogram-num2*.png"
check_log_value "${SMALL_LOG_PATH}" "${start}" "options.type" "histogram"
check_log_list_contains "${SMALL_LOG_PATH}" "${start}" "options.vars" "num1,num2"
check_figure_count "${SMALL_LOG_PATH}" "${start}" "2"
check_figure_value "${SMALL_LOG_PATH}" "${start}" "0" "figure_number" "5"
check_figure_value "${SMALL_LOG_PATH}" "${start}" "1" "figure_number" "6"

start=$(log_count "${SMALL_LOG_PATH}")
run_ok "multi vars explicit" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${SMALL_PARQUET}" --type histogram --vars num1,num2 --figure-number 8
assert_glob "${SMALL_PLOTS_DIR}/figure-008-histogram-num1*.png"
assert_glob "${SMALL_PLOTS_DIR}/figure-009-histogram-num2*.png"
check_figure_count "${SMALL_LOG_PATH}" "${start}" "2"

start=$(log_count "${SMALL_LOG_PATH}")
run_ok "auto box group" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${SMALL_PARQUET}" --vars num1 --group cat --figure-number 7
assert_glob "${SMALL_PLOTS_DIR}/figure-007-box-*.png"
check_log_value "${SMALL_LOG_PATH}" "${start}" "options.type" "box"
check_log_value "${SMALL_LOG_PATH}" "${start}" "options.group" "cat"
check_figure_value "${SMALL_LOG_PATH}" "${start}" "0" "plot_type" "box"

run_ok "init workspace (semicolon)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${SEMI_CSV}" --sep ";"
rm -f "${SEMI_NLSS_REPORT_PATH}" "${SEMI_LOG_PATH}"
rm -rf "${SEMI_PLOTS_DIR}"
rm -f "${SEMI_PARQUET}"

start=$(log_count "${SEMI_LOG_PATH}")
run_ok "csv semicolon" Rscript "${R_SCRIPT_DIR}/plot.R" --csv "${SEMI_CSV}" --sep ";" --type bar --vars category --stat percent --percent-base total --position fill --figure-number 15
assert_glob "${SEMI_PLOTS_DIR}/figure-015-bar-category*.png"
check_log_list_contains "${SEMI_LOG_PATH}" "${start}" "options.vars" "category"
check_log_value "${SEMI_LOG_PATH}" "${start}" "options.percent_base" "total"
check_log_value "${SEMI_LOG_PATH}" "${start}" "options.position" "fill"

run_ok "init workspace (no header)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${NO_HEADER_CSV}" --header FALSE
rm -f "${NO_HEADER_NLSS_REPORT_PATH}" "${NO_HEADER_LOG_PATH}"
rm -rf "${NO_HEADER_PLOTS_DIR}"
rm -f "${NO_HEADER_PARQUET}"

start=$(log_count "${NO_HEADER_LOG_PATH}")
run_ok "csv no header" Rscript "${R_SCRIPT_DIR}/plot.R" --csv "${NO_HEADER_CSV}" --header FALSE --type bar --vars V1 --figure-number 16
assert_glob "${NO_HEADER_PLOTS_DIR}/figure-016-bar-V1*.png"
check_log_list_contains "${NO_HEADER_LOG_PATH}" "${start}" "options.vars" "V1"

snapshot_outputs "golden_before_template_override" "${NLSS_REPORT_PATH}" "${PLOTS_DIR}" "${LOG_PATH}"
TEMPLATE_DEFAULT_ORIG="$(get_config_value templates.plot.default)"
if [ -z "${TEMPLATE_DEFAULT_ORIG}" ]; then
  TEMPLATE_DEFAULT_ORIG="plot/default-template.md"
fi
TEMPLATE_OVERRIDE_PATH="${TMP_BASE}/plot_template_override.md"
cp "$(resolve_template_source "${TEMPLATE_DEFAULT_ORIG}")" "${TEMPLATE_OVERRIDE_PATH}"
printf "\n\nPLOT_TEMPLATE_OVERRIDE\n" >> "${TEMPLATE_OVERRIDE_PATH}"

snapshot_outputs "golden_after_template_override_path" "${NLSS_REPORT_PATH}" "${PLOTS_DIR}" "${LOG_PATH}"
rm -f "${NLSS_REPORT_PATH}"
rm -rf "${PLOTS_DIR}"

start=$(log_count "${LOG_PATH}")
run_ok "template override path" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --template "${TEMPLATE_OVERRIDE_PATH}" --figure-number 1
assert_contains "${NLSS_REPORT_PATH}" "PLOT_TEMPLATE_OVERRIDE"
check_log_value "${LOG_PATH}" "${start}" "options.type" "histogram"

set_config_value templates.plot.default "${TEMPLATE_OVERRIDE_PATH}"
snapshot_outputs "golden_after_template_override_key" "${NLSS_REPORT_PATH}" "${PLOTS_DIR}" "${LOG_PATH}"
rm -f "${NLSS_REPORT_PATH}"
rm -rf "${PLOTS_DIR}"

start=$(log_count "${LOG_PATH}")
run_ok "template override key" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type histogram --vars age --template default --figure-number 2
assert_contains "${NLSS_REPORT_PATH}" "PLOT_TEMPLATE_OVERRIDE"
check_log_value "${LOG_PATH}" "${start}" "options.type" "histogram"
set_config_value templates.plot.default "${TEMPLATE_DEFAULT_ORIG}"

run_ok "init workspace (missing)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${MISSING_CSV}"
rm -f "${MISSING_NLSS_REPORT_PATH}" "${MISSING_LOG_PATH}"
rm -rf "${MISSING_PLOTS_DIR}"

start=$(log_count "${MISSING_LOG_PATH}")
run_ok "missing na_action keep" Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${MISSING_PARQUET}" --type bar --vars segment --na-action keep --figure-number 1
assert_contains "${MISSING_NLSS_REPORT_PATH}" "Missing values shown as 'Missing'"
check_log_value "${MISSING_LOG_PATH}" "${start}" "options.na_action" "keep"
check_figure_value "${MISSING_LOG_PATH}" "${start}" "0" "plot_type" "bar"

run_ok "init workspace (interactive)" Rscript "${R_SCRIPT_DIR}/init_workspace.R" --csv "${INTERACTIVE_CSV}"
rm -f "${INTERACTIVE_NLSS_REPORT_PATH}" "${INTERACTIVE_LOG_PATH}"
rm -rf "${INTERACTIVE_PLOTS_DIR}"

cat >"${INTERACTIVE_INPUT}" <<PROMPT
csv
${INTERACTIVE_CSV}
,
TRUE
histogram
value



count
total
10


none
TRUE
none
omit
classic
default
Interactive Histogram



png
7
5
300
interactive

FALSE
2

plot interactive test
TRUE
PROMPT

start=$(log_count "${INTERACTIVE_LOG_PATH}")
run_ok "interactive run" env NLSS_PROMPT_FILE="${INTERACTIVE_INPUT}" Rscript "${R_SCRIPT_DIR}/plot.R" --interactive
assert_contains "${INTERACTIVE_NLSS_REPORT_PATH}" "Interactive Histogram"
check_log_value "${INTERACTIVE_LOG_PATH}" "${start}" "user_prompt" "plot interactive test"

run_expect_fail "invalid plot type" "${LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type not_a_plot --vars age
run_expect_fail "corr-heatmap nonnumeric" "${LOG_PATH}" \
  Rscript "${R_SCRIPT_DIR}/plot.R" --parquet "${PARQUET_GOLDEN}" --type corr-heatmap --vars x1,gender

echo "plot tests: OK" | tee -a "${LOG_FILE}"
