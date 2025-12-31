#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTPUTS_DIR="${ROOT_DIR}/outputs"

if [[ ! -d "${OUTPUTS_DIR}" ]]; then
  echo "outputs directory not found: ${OUTPUTS_DIR}" >&2
  exit 1
fi

# Remove everything in outputs except test-runs.
while IFS= read -r -d '' entry; do
  base="$(basename "${entry}")"
  if [[ "${base}" == "test-runs" ]]; then
    continue
  fi
  rm -rf "${entry}"
done < <(find "${OUTPUTS_DIR}" -mindepth 1 -maxdepth 1 -print0)
