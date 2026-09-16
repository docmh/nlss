#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Standalone module smoke. Uses the isolated public runner, never edits config.yml.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "${SCRIPT_DIR}/../.." && pwd)"
PYTHON_BIN="${PYTHON_BIN:-}"
if [ -z "${PYTHON_BIN}" ]; then
  if command -v python3 >/dev/null 2>&1; then PYTHON_BIN=python3
  elif command -v python >/dev/null 2>&1; then PYTHON_BIN=python
  else echo "Python 3 is required." >&2; exit 2; fi
fi
export PYTHON_BIN
mapfile -t SETTINGS < <(Rscript -e 'args <- commandArgs(TRUE); x <- yaml::read_yaml(args[1])$tests; cat(x$output_dir, x$keep_runs_default, x$scripts$phase2_missings_r, sep="\n")' "${NLSS_TESTS_CONFIG:-${REPO}/tests/tests.yml}")
if [ "${#SETTINGS[@]}" -ne 3 ]; then echo "Missings test registration is missing." >&2; exit 2; fi
ROOT="${NLSS_TEST_ROOT:-${REPO}/${SETTINGS[0]}/$(date +%Y%m%d%H%M%S)-$$}"
KEEP="${NLSS_KEEP_RUNS:-${SETTINGS[1]}}"
while [ "$#" -gt 0 ]; do
  case "$1" in
    --root) ROOT="$2"; shift 2 ;;
    --keep) KEEP="$2"; shift 2 ;;
    --help) echo "Usage: run_missings_tests.sh [--root PATH] [--keep N]"; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done
case "${ROOT}" in /*|[A-Za-z]:*) ;; *) ROOT="${REPO}/${ROOT}" ;; esac
mkdir -p "${ROOT}"
LOG="${ROOT}/missings_test.log"
run_ok() {
  local label="$1"; shift
  echo "[RUN] ${label}" | tee -a "${LOG}"
  "$@" >>"${LOG}" 2>&1
  echo "[PASS] ${label}" | tee -a "${LOG}"
}
run_ok "missings public CLI golden cases" Rscript "${REPO}/${SETTINGS[2]}" --root "${ROOT}" --keep "${KEEP}" --match '^missings_.*_independent_goldens_smoke$'
LATEST="$(Rscript -e 'd <- list.dirs(commandArgs(TRUE)[1], recursive=FALSE, full.names=TRUE); d <- sort(d[grepl("^run-[0-9]{14}-[0-9]+$", basename(d))]); if (!length(d)) stop("Missing runner output"); cat(tail(d,1))' "${ROOT}/phase2-missings")"
for method in auto listwise impute indicator drop; do
  run_ok "missings ${method} independent JSONL golden values" "${PYTHON_BIN}" "${REPO}/tests/values/check_missings_values.py" \
    "${LATEST}/cases/golden-${method}/project/sample/analysis_log.jsonl" 0 "${REPO}/tests/values/missings_golden.csv" "${method}"
done
echo "Missings smoke completed. Evidence: ${LATEST}/results.json"
