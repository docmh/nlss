#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT_PATH="$ROOT/r-core-stats/scripts/R/data_transform.R"
TEMPLATE_PATH="$ROOT/r-core-stats/assets/data-transform/default-template.md"

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

TEST_ROOT="$ROOT/outputs/test"
mkdir -p "$TEST_ROOT"
WORKDIR="$(mktemp -d "$TEST_ROOT/data-transform-template-test.XXXXXX")"
CSV_PATH="$WORKDIR/data_transform_test.csv"
BASE_OUT="$WORKDIR/out-baseline"
MOD_OUT="$WORKDIR/out-modified"
BACKUP_PATH="$WORKDIR/template-backup.md"
TMPDIR_PATH="$WORKDIR/tmp"

mkdir -p "$BASE_OUT" "$MOD_OUT" "$TMPDIR_PATH"

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

run_transform() {
  local out_dir="$1"
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
    --log FALSE \
    --out "$out_dir" >/dev/null
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
BASE_REPORT="$BASE_OUT/apa_report.md"
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
MOD_REPORT="$MOD_OUT/apa_report.md"
if [[ ! -f "$MOD_REPORT" ]]; then
  echo "Missing report: $MOD_REPORT"
  exit 1
fi

assert_contains "$MOD_REPORT" "Data Transformations TEMPLATE TEST"
assert_contains "$MOD_REPORT" "Transformation log TEST."
assert_contains "$MOD_REPORT" "*Note-TEST.*"
assert_contains "$MOD_REPORT" "**Narrative TEST**"

echo "data_transform template tests: OK"
