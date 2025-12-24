#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CONFIG_PATH="$ROOT/core-stats/scripts/config.yml"
SCRIPT_PATH="$ROOT/core-stats/scripts/R/data_transform.R"
TEMPLATE_PATH="$ROOT/core-stats/assets/data-transform/default-template.md"

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
WORKDIR="$(mktemp -d "$RUN_ROOT/data-transform-template-test.XXXXXX")"
CSV_PATH="$WORKDIR/data_transform_test.csv"
DATASET_LABEL="$(basename "${CSV_PATH}")"
DATASET_LABEL="${DATASET_LABEL%.*}"
BASE_OUT="$WORKDIR/out-baseline"
MOD_OUT="$WORKDIR/out-modified"
BACKUP_PATH="$WORKDIR/template-backup.md"
TMPDIR_PATH="$WORKDIR/tmp"

mkdir -p "$BASE_OUT" "$MOD_OUT" "$TMPDIR_PATH"

CONFIG_BAK="$(mktemp)"
cp "$CONFIG_PATH" "$CONFIG_BAK"

cleanup() {
  if [[ -f "$BACKUP_PATH" ]]; then
    mv -f "$BACKUP_PATH" "$TEMPLATE_PATH"
  fi
  cp "$CONFIG_BAK" "$CONFIG_PATH"
  rm -f "$CONFIG_BAK"
}
trap cleanup EXIT

assert_contains() {
  local file="$1"
  local expected="$2"
  if command -v rg >/dev/null 2>&1; then
    if ! rg -q --fixed-strings "$expected" "$file"; then
      echo "Expected to find: $expected"
      echo "In file: $file"
      exit 1
    fi
  else
    if ! grep -qF "$expected" "$file"; then
      echo "Expected to find: $expected"
      echo "In file: $file"
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

run_transform() {
  local out_dir="$1"
  python3 - "$CONFIG_PATH" "$out_dir" <<'PY'
import sys
path, value = sys.argv[1], sys.argv[2]
lines = open(path, "r", encoding="utf-8").read().splitlines()
for idx, line in enumerate(lines):
    stripped = line.strip()
    if stripped.startswith("output_dir:"):
        indent = len(line) - len(line.lstrip(" "))
        lines[idx] = (" " * indent) + f'output_dir: "{value}"'
        break
else:
    raise SystemExit("output_dir not found in config.yml")
with open(path, "w", encoding="utf-8", newline="") as handle:
    handle.write("\\n".join(lines) + "\\n")
PY
  TMPDIR="$TMPDIR_PATH" Rscript "$SCRIPT_PATH" \
    --csv "$CSV_PATH" \
    --calc "bmi=weight/(height^2)" \
    --transform "score=log" \
    --standardize age \
    --recode "gender='M':1,'F':0" \
    --percentile-bins "weight=4" \
    --bins "age=30,20,40" \
    --rename gender_rec:gender_binary \
    --drop height \
    --confirm-drop \
    --log FALSE >/dev/null
}

cat >"$CSV_PATH" <<'DATA'
age,gender,score,height,weight
23,M,10,1.8,70
25,F,12,1.7,60
22,F,15,1.6,55
26,M,11,1.9,80
24,M,14,1.8,75
DATA

run_transform "$BASE_OUT"
BASE_REPORT="$BASE_OUT/$DATASET_LABEL/apa_report.md"
if [[ ! -f "$BASE_REPORT" ]]; then
  echo "Missing report: $BASE_REPORT"
  exit 1
fi

assert_contains "$BASE_REPORT" "Data Transformations"
assert_contains "$BASE_REPORT" "Transformation log."
assert_contains "$BASE_REPORT" "Table 1"
assert_contains "$BASE_REPORT" "Standardized"
assert_contains "$BASE_REPORT" "Renamed"
assert_contains "$BASE_REPORT" "sorted breaks"
assert_contains "$BASE_REPORT" "Actions:"
assert_contains "$BASE_REPORT" "Step 1:"

cp "$TEMPLATE_PATH" "$BACKUP_PATH"
sed -i 's/title: "Data Transformations"/title: "Data Transformations TEMPLATE TEST"/' "$TEMPLATE_PATH"
sed -i 's/table_title: "Transformation log\."/table_title: "Transformation log TEST."/' "$TEMPLATE_PATH"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "$TEMPLATE_PATH"
sed -i 's/narrative_heading: "\*\*Narrative\*\*"/narrative_heading: "**Narrative TEST**"/' "$TEMPLATE_PATH"

run_transform "$MOD_OUT"
MOD_REPORT="$MOD_OUT/$DATASET_LABEL/apa_report.md"
if [[ ! -f "$MOD_REPORT" ]]; then
  echo "Missing report: $MOD_REPORT"
  exit 1
fi

assert_contains "$MOD_REPORT" "Data Transformations TEMPLATE TEST"
assert_contains "$MOD_REPORT" "Transformation log TEST."
assert_contains "$MOD_REPORT" "*Note-TEST.*"
assert_contains "$MOD_REPORT" "**Narrative TEST**"

echo "data_transform template tests: OK"
cleanup_runs
