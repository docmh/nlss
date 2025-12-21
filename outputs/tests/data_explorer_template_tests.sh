#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT_PATH="$ROOT/r-core-stats/scripts/R/data_explorer.R"
TEMPLATE_PATH="$ROOT/r-core-stats/assets/data-explorer/default-template.md"

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

mkdir -p "$ROOT/outputs/tmp"
WORKDIR="$(mktemp -d "$ROOT/outputs/tmp/data-explorer-template-test.XXXXXX")"
CSV_PATH="$WORKDIR/data_explorer_test.csv"
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

run_explorer() {
  local out_dir="$1"
  TMPDIR="$TMPDIR_PATH" Rscript "$SCRIPT_PATH" \
    --csv "$CSV_PATH" \
    --vars age,gender,segment \
    --max-levels 2 \
    --top-n 1 \
    --digits 1 \
    --log FALSE \
    --out "$out_dir" >/dev/null
}

cat >"$CSV_PATH" <<'EOF'
age,gender,segment
23,M,A
25,F,B
22,F,C
26,M,D
24,M,A
EOF

run_explorer "$BASE_OUT"
BASE_REPORT="$BASE_OUT/apa_report.md"
if [[ ! -f "$BASE_REPORT" ]]; then
  echo "Missing report: $BASE_REPORT"
  exit 1
fi

assert_contains "$BASE_REPORT" "Data Exploration"
assert_contains "$BASE_REPORT" "Variable overview."
assert_contains "$BASE_REPORT" "Value levels."
assert_contains "$BASE_REPORT" "Table 1"
assert_contains "$BASE_REPORT" "Table 2"
assert_contains "$BASE_REPORT" "Other (remaining)"

cp "$TEMPLATE_PATH" "$BACKUP_PATH"
sed -i 's/title: "Data Exploration"/title: "Data Exploration TEMPLATE TEST"/' "$TEMPLATE_PATH"
sed -i 's/overview_table_title: "Variable overview\."/overview_table_title: "Variable overview TEST."/' "$TEMPLATE_PATH"
sed -i 's/levels_table_title: "Value levels\."/levels_table_title: "Value levels TEST."/' "$TEMPLATE_PATH"
sed -i 's/note_prefix: "\*Note\.\*"/note_prefix: "*Note-TEST.*"/' "$TEMPLATE_PATH"

run_explorer "$MOD_OUT"
MOD_REPORT="$MOD_OUT/apa_report.md"
if [[ ! -f "$MOD_REPORT" ]]; then
  echo "Missing report: $MOD_REPORT"
  exit 1
fi

assert_contains "$MOD_REPORT" "Data Exploration TEMPLATE TEST"
assert_contains "$MOD_REPORT" "Variable overview TEST."
assert_contains "$MOD_REPORT" "Value levels TEST."
assert_contains "$MOD_REPORT" "*Note-TEST.*"

echo "data_explorer template tests: OK"
