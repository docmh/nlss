#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
set -euo pipefail
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "${SCRIPT_DIR}/../.." && pwd)
TESTS_YML="${NLSS_TESTS_CONFIG:-${REPO_ROOT}/tests/tests.yml}"
RUNNER=$(Rscript -e 'cfg <- yaml::read_yaml(commandArgs(TRUE)[1], eval.expr=FALSE); cat(cfg$tests$scripts$phase2_categorical_r)' "$TESTS_YML")
if [[ -z "$RUNNER" || ! -f "${REPO_ROOT}/${RUNNER}" ]]; then
  echo "Missing categorical acceptance runner in tests/tests.yml" >&2
  exit 2
fi
exec Rscript "${REPO_ROOT}/${RUNNER}" --modules frequencies "$@"
