#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_PATH="${ROOT_DIR}/core-stats/scripts/config.yml"
SCRIPT="${ROOT_DIR}/core-stats/scripts/R/t_test.R"
DATA_MAIN="${ROOT_DIR}/outputs/tests/data/t_test_main.csv"
DATA_SMALL="${ROOT_DIR}/outputs/tests/data/t_test_small.csv"

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

set_config_value() {
  python3 - "$CONFIG_PATH" "$1" "$2" <<'PY'
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
    handle.write("\\n".join(lines) + "\\n")
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

WORKSPACE_DIR="${RUN_ROOT}/t_test_workspace"
TMPDIR_PATH="${RUN_ROOT}/tmp/t_test"
LOG_PATH="${RUN_ROOT}/t_test_cases.log"

CONFIG_BAK="$(mktemp)"
cp "${CONFIG_PATH}" "${CONFIG_BAK}"

cleanup() {
  cp "${CONFIG_BAK}" "${CONFIG_PATH}"
  rm -f "${CONFIG_BAK}"
}
trap cleanup EXIT

mkdir -p "${TMPDIR_PATH}" "${WORKSPACE_DIR}"
set_config_value defaults.output_dir "${WORKSPACE_DIR}"

run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG_PATH}"
  TMPDIR="${TMPDIR_PATH}" Rscript "${SCRIPT}" "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  if [ $status -ne 0 ]; then
    echo "[FAIL] ${label} (exit ${status})" | tee -a "${LOG_PATH}"
    return 1
  fi
  echo "[PASS] ${label}" | tee -a "${LOG_PATH}"
}

run_fail() {
  local label="$1"; shift
  echo "[RUN-FAIL] ${label}" | tee -a "${LOG_PATH}"
  TMPDIR="${TMPDIR_PATH}" Rscript "${SCRIPT}" "$@" >>"${LOG_PATH}" 2>&1
  local status=$?
  if [ $status -eq 0 ]; then
    echo "[FAIL] ${label} (expected failure)" | tee -a "${LOG_PATH}"
    return 1
  fi
  echo "[PASS] ${label} (failed as expected)" | tee -a "${LOG_PATH}"
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

: > "${LOG_PATH}"

run_ok "one-sample default" --csv "${DATA_MAIN}" --vars score
run_ok "one-sample greater mu" --csv "${DATA_MAIN}" --vars score --mu 0.3 --alternative greater
run_ok "one-sample bootstrap" --csv "${DATA_MAIN}" --vars score2 --bootstrap TRUE --bootstrap-samples 1000 --seed 123
run_ok "independent welch" --csv "${DATA_MAIN}" --vars score --group group
run_ok "independent equal var" --csv "${DATA_MAIN}" --vars score2 --group group --var-equal TRUE
run_ok "paired two pairs" --csv "${DATA_MAIN}" --x pre,score --y post,score2
run_ok "one-sample tiny n" --csv "${DATA_SMALL}" --vars score

run_fail "independent with 3 levels" --csv "${DATA_MAIN}" --vars score --group group3
run_fail "non-numeric variable" --csv "${DATA_MAIN}" --vars nonnumeric
run_fail "paired mismatch lists" --csv "${DATA_MAIN}" --x pre,score --y post

cleanup_runs
