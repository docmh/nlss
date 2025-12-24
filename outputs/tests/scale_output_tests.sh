#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CONFIG_PATH="$ROOT/core-stats/scripts/config.yml"
SCRIPT_PATH="$ROOT/core-stats/scripts/R/scale.R"
TEMPLATE_PATH="$ROOT/core-stats/assets/scale/default-template.md"

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
  echo "${ROOT}/${path#./}"
}

RUNS_BASE_CFG="$(get_config_value tests.output_dir)"
if [ -z "${RUNS_BASE_CFG}" ]; then
  RUNS_BASE_CFG="outputs/test-runs"
fi

WORKSPACE_MANIFEST_NAME="$(get_config_value defaults.workspace_manifest)"
if [ -z "${WORKSPACE_MANIFEST_NAME}" ]; then
  WORKSPACE_MANIFEST_NAME="core-stats-workspace.yml"
fi

RUN_ID="$(date +%Y%m%d%H%M%S)"
RUNS_BASE="$(to_abs_path "${RUNS_BASE_CFG}")"
if [ -n "${CORE_STATS_TEST_ROOT:-}" ]; then
  RUN_ROOT="$(to_abs_path "${CORE_STATS_TEST_ROOT}")"
else
  RUN_ROOT="${RUNS_BASE}/${RUN_ID}"
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install R or use scripts/run_rscript.ps1 on Windows."
  exit 1
fi

if [[ ! -f "$SCRIPT_PATH" ]]; then
  echo "Missing script: $SCRIPT_PATH"
  exit 1
fi

if [[ ! -f "$TEMPLATE_PATH" ]]; then
  echo "Missing template: $TEMPLATE_PATH"
  exit 1
fi

mkdir -p "$RUN_ROOT"
WORKDIR="$(mktemp -d "$RUN_ROOT/scale-output-tests.XXXXXX")"
CSV_PATH="$WORKDIR/scale_output_test.csv"
DATASET_LABEL="$(basename "${CSV_PATH}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
BASE_OUT="$WORKDIR/out-baseline"
APPEND_OUT="$WORKDIR/out-append"
MOD_OUT="$WORKDIR/out-modified"
BACKUP_PATH="$WORKDIR/template-backup.md"
TMPDIR_PATH="$WORKDIR/tmp"
RESULTS_PATH="$WORKDIR/results.txt"
SUMMARY_PATH="$RUN_ROOT/scale-output-tests-results.txt"

mkdir -p "$BASE_OUT" "$APPEND_OUT" "$MOD_OUT" "$TMPDIR_PATH"

cleanup() {
  if [[ -f "$BACKUP_PATH" ]]; then
    mv -f "$BACKUP_PATH" "$TEMPLATE_PATH"
  fi
}
trap cleanup EXIT

assert_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if ! rg -q --fixed-strings "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "$RESULTS_PATH"
      echo "In file: $file" | tee -a "$RESULTS_PATH"
      exit 1
    fi
  else
    if ! grep -qF "$expected" "$file"; then
      echo "Expected to find: $expected" | tee -a "$RESULTS_PATH"
      echo "In file: $file" | tee -a "$RESULTS_PATH"
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

run_scale() {
  local out_dir="$1"
  local manifest_path="${out_dir}/${WORKSPACE_MANIFEST_NAME}"
  : > "${manifest_path}"
  set +e
  (
    trap 'rm -f "${manifest_path}"' EXIT
    cd "${out_dir}" || exit 1
    TMPDIR="$TMPDIR_PATH" Rscript "$SCRIPT_PATH" \
      --csv "$CSV_PATH" \
      --vars item1,item2,item3 \
      --digits 2 \
      --log TRUE \
      >/dev/null
  )
  local status=$?
  set -e
  return "${status}"
}

cat >"$CSV_PATH" <<'EOF_DATA'
item1,item2,item3,group
1,2,1,A
2,3,2,A
3,4,3,B
4,5,3,B
5,6,4,B
6,7,5,A
7,8,6,A
8,9,7,B
9,10,8,B
10,12,9,A
EOF_DATA

# Baseline output generation
run_scale "$BASE_OUT"
BASE_REPORT="$BASE_OUT/$DATASET_LABEL/apa_report.md"
if [[ ! -f "$BASE_REPORT" ]]; then
  echo "Missing report: $BASE_REPORT" | tee -a "$RESULTS_PATH"
  exit 1
fi

assert_contains "$BASE_REPORT" "Scale Analysis"
assert_contains "$BASE_REPORT" "Item analysis and reliability."
assert_contains "$BASE_REPORT" "Table 1"
assert_contains "$BASE_REPORT" "r_it"
assert_contains "$BASE_REPORT" "r_drop"
assert_contains "$BASE_REPORT" "alpha_if_deleted"
assert_contains "$BASE_REPORT" "**Narrative**"
assert_contains "$BASE_REPORT" "r_it = item-total correlation"
assert_contains "$BASE_REPORT" "Variables: item1, item2, item3"

LOG_PATH="$BASE_OUT/$DATASET_LABEL/analysis_log.jsonl"
if [[ ! -f "$LOG_PATH" ]]; then
  echo "Missing log: $LOG_PATH" | tee -a "$RESULTS_PATH"
  exit 1
fi

# Append test: run twice and confirm Table 1/2
run_scale "$APPEND_OUT"
run_scale "$APPEND_OUT"
APPEND_REPORT="$APPEND_OUT/$DATASET_LABEL/apa_report.md"
if [[ ! -f "$APPEND_REPORT" ]]; then
  echo "Missing report: $APPEND_REPORT" | tee -a "$RESULTS_PATH"
  exit 1
fi
assert_contains "$APPEND_REPORT" "Table 1"
assert_contains "$APPEND_REPORT" "Table 2"

LOG_PATH_APPEND="$APPEND_OUT/$DATASET_LABEL/analysis_log.jsonl"
if [[ ! -f "$LOG_PATH_APPEND" ]]; then
  echo "Missing log: $LOG_PATH_APPEND" | tee -a "$RESULTS_PATH"
  exit 1
fi
LOG_LINES=$(wc -l <"$LOG_PATH_APPEND")
if [[ "$LOG_LINES" -lt 2 ]]; then
  echo "Expected at least 2 log lines, found $LOG_LINES" | tee -a "$RESULTS_PATH"
  exit 1
fi

# Template modification test
cp "$TEMPLATE_PATH" "$BACKUP_PATH"
sed -i 's/title: "Scale Analysis"/title: "Scale Analysis TEMPLATE TEST"/' "$TEMPLATE_PATH"
sed -i 's/table_title: "Item analysis and reliability\."/table_title: "Item analysis TEST."/' "$TEMPLATE_PATH"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "$TEMPLATE_PATH"
sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "$TEMPLATE_PATH"
sed -i 's/label: "Item"/label: "Item TEST"/' "$TEMPLATE_PATH"

run_scale "$MOD_OUT"
MOD_REPORT="$MOD_OUT/$DATASET_LABEL/apa_report.md"
if [[ ! -f "$MOD_REPORT" ]]; then
  echo "Missing report: $MOD_REPORT" | tee -a "$RESULTS_PATH"
  exit 1
fi

assert_contains "$MOD_REPORT" "Scale Analysis TEMPLATE TEST"
assert_contains "$MOD_REPORT" "Item analysis TEST."
assert_contains "$MOD_REPORT" "*Note-TEST.*"
assert_contains "$MOD_REPORT" "**Narrative TEST**"
assert_contains "$MOD_REPORT" "Item TEST"

echo "scale output tests: OK" | tee -a "$RESULTS_PATH"
cp -f "$RESULTS_PATH" "$SUMMARY_PATH"

echo "Results written to: $RESULTS_PATH"
echo "Latest summary: $SUMMARY_PATH"
cleanup_runs
