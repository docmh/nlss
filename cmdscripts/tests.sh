# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "${SCRIPT_DIR}/.." && pwd)
TESTS_YML="${REPO_ROOT}/tests/tests.yml"

if [[ ! -f "$TESTS_YML" ]]; then
  echo "Missing tests.yml at $TESTS_YML" >&2
  exit 2
fi

get_value() {
  local pattern="$1"
  sed -n "s/^${pattern}:[[:space:]]*//p" "$TESTS_YML" | head -n1 | sed 's/^"//;s/"$//'
}

get_module_script() {
  local module="$1"
  awk -v mod="$module" '
    function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
    function stripq(s) { sub(/^"/, "", s); sub(/"$/, "", s); return s }
    /^  scripts:/ { in_scripts=1; next }
    in_scripts && /^    modules:/ { in_modules=1; next }
    in_scripts && in_modules {
      if ($0 ~ /^    [^ ]/ && $0 !~ /^    modules:/) { exit }
      if ($0 ~ /^      [^ ]/) {
        line=$0
        sub(/^      /, "", line)
        split(line, parts, ":")
        key=trim(parts[1])
        if (key == mod) {
          val=substr(line, index(line, ":")+1)
          val=stripq(trim(val))
          print val
          exit
        }
      }
    }
  ' "$TESTS_YML"
}

list_modules() {
  awk '
    function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
    /^  scripts:/ { in_scripts=1; next }
    in_scripts && /^    modules:/ { in_modules=1; next }
    in_scripts && in_modules {
      if ($0 ~ /^    [^ ]/ && $0 !~ /^    modules:/) { exit }
      if ($0 ~ /^      [^ ]/) {
        line=$0
        sub(/^      /, "", line)
        split(line, parts, ":")
        key=trim(parts[1])
        if (key != "") print key
      }
    }
  ' "$TESTS_YML"
}

OUTPUT_DIR=$(get_value "  output_dir")
DATA_DIR=$(get_value "  data_dir")
GOLDEN_DATASET=$(get_value "  golden_dataset")
TEMPLATE_DIR=$(get_value "  template_dir")
TEMPLATE_MARKER=$(get_value "  template_marker")
KEEP_DEFAULT=$(get_value "  keep_runs_default")
SMOKE_SCRIPT=$(get_value "    smoke_unix")
DELIBERATE_SCRIPT=$(get_value "    deliberate_unix")

if [[ -z "$OUTPUT_DIR" || -z "$DATA_DIR" || -z "$GOLDEN_DATASET" || -z "$TEMPLATE_DIR" ]]; then
  echo "Invalid tests.yml content." >&2
  exit 2
fi

SUITE="smoke"
MODULE=""
RUN_ROOT=""
KEEP_RUNS=""
CLEAN_ONLY=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    smoke|deliberate|all)
      SUITE="$1"
      shift 1
      ;;
    --suite)
      SUITE="$2"
      shift 2
      ;;
    --module|--subskill)
      MODULE="$2"
      shift 2
      ;;
    --root)
      RUN_ROOT="$2"
      shift 2
      ;;
    --keep)
      KEEP_RUNS="$2"
      shift 2
      ;;
    --clean)
      CLEAN_ONLY=1
      shift 1
      ;;
    --help|-h)
      echo "Usage: $0 [smoke|deliberate|all] [--suite <name>] [--module <name>] [--root <path>] [--keep <n>] [--clean]" >&2
      echo "  --module <name> runs a single deliberate module script from tests/tests.yml." >&2
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
 done

OUTPUT_DIR="${REPO_ROOT}/${OUTPUT_DIR}"
DATASET_PATH="${REPO_ROOT}/${GOLDEN_DATASET}"
TEMPLATE_PATH="${REPO_ROOT}/${TEMPLATE_DIR}"

if [[ ! -f "$DATASET_PATH" ]]; then
  echo "Golden dataset not found: $DATASET_PATH" >&2
  exit 2
fi

mkdir -p "$OUTPUT_DIR"

if [[ -z "$KEEP_RUNS" ]]; then
  KEEP_RUNS="${NLSS_KEEP_RUNS:-$KEEP_DEFAULT}"
fi

cleanup_runs() {
  local root="$1"
  local keep="$2"
  if [[ -z "$keep" || "$keep" == "0" ]]; then
    return 0
  fi
  if ! [[ "$keep" =~ ^[0-9]+$ ]]; then
    return 0
  fi
  if [[ ! -d "$root" ]]; then
    return 0
  fi
  local dirs
  mapfile -t dirs < <(ls -1d "$root"/* 2>/dev/null | sort || true)
  local count=${#dirs[@]}
  if (( count <= keep )); then
    return 0
  fi
  local remove_count=$((count - keep))
  for ((i=0; i<remove_count; i++)); do
    rm -rf "${dirs[$i]}"
  done
}

if [[ $CLEAN_ONLY -eq 1 ]]; then
  cleanup_runs "$OUTPUT_DIR" "$KEEP_RUNS"
  echo "Cleaned old runs in $OUTPUT_DIR (keep=$KEEP_RUNS)."
  exit 0
fi

if [[ -z "$RUN_ROOT" ]]; then
  timestamp=$(date +%Y%m%d%H%M%S)
  RUN_ROOT="$OUTPUT_DIR/$timestamp"
fi

mkdir -p "$RUN_ROOT"

export NLSS_TEST_ROOT="$RUN_ROOT"
export NLSS_TEST_REPO_ROOT="$REPO_ROOT"
export NLSS_TEST_DATASET="$DATASET_PATH"
export NLSS_TEST_TEMPLATE_DIR="$TEMPLATE_PATH"
export NLSS_TEST_TEMPLATE_MARKER="$TEMPLATE_MARKER"
TMP_DIR="$RUN_ROOT/tmp"
mkdir -p "$TMP_DIR"
export TMPDIR="$TMP_DIR"
export TMP="$TMP_DIR"
export TEMP="$TMP_DIR"

FAILURES=0

run_suite() {
  local label="$1"
  local script="$2"
  if [[ ! -f "$REPO_ROOT/$script" ]]; then
    echo "Missing script: $REPO_ROOT/$script" >&2
    FAILURES=$((FAILURES + 1))
    return
  fi
  bash "$REPO_ROOT/$script" \
    --root "$RUN_ROOT" \
    --repo-root "$REPO_ROOT" \
    --golden-dataset "$DATASET_PATH" \
    --template-dir "$TEMPLATE_PATH" \
    --template-marker "$TEMPLATE_MARKER" || FAILURES=$((FAILURES + 1))
}

run_module() {
  local label="$1"
  local script="$2"
  if [[ ! -f "$REPO_ROOT/$script" ]]; then
    echo "Missing script: $REPO_ROOT/$script" >&2
    FAILURES=$((FAILURES + 1))
    return
  fi
  bash "$REPO_ROOT/$script" || FAILURES=$((FAILURES + 1))
}

if [[ -n "$MODULE" ]]; then
  module_script="$(get_module_script "$MODULE")"
  module_label="$MODULE"
  if [[ -z "$module_script" ]]; then
    module_key="${MODULE//-/_}"
    if [[ "$module_key" != "$MODULE" ]]; then
      module_script="$(get_module_script "$module_key")"
      if [[ -n "$module_script" ]]; then
        module_label="$module_key"
      fi
    fi
  fi
  if [[ -z "$module_script" ]]; then
    echo "Unknown module: $MODULE" >&2
    echo "Available modules: $(list_modules | tr '\n' ' ' | sed 's/[[:space:]]*$//')" >&2
    exit 2
  fi
  run_module "$module_label" "$module_script"
else
  case "$SUITE" in
    smoke)
      run_suite "smoke" "$SMOKE_SCRIPT"
      ;;
    deliberate)
      run_suite "deliberate" "$DELIBERATE_SCRIPT"
      ;;
    all)
      run_suite "smoke" "$SMOKE_SCRIPT"
      run_suite "deliberate" "$DELIBERATE_SCRIPT"
      ;;
    *)
      echo "Unknown suite: $SUITE" >&2
      exit 2
      ;;
   esac
fi

cleanup_runs "$OUTPUT_DIR" "$KEEP_RUNS"

if [[ $FAILURES -gt 0 ]]; then
  echo "Test run completed with $FAILURES failure(s)." >&2
  exit 1
fi

echo "Test run completed successfully."
