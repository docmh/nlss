#!/usr/bin/env bash
set -euo pipefail

command -v python3 >/dev/null 2>&1 || { echo "python3 is required." >&2; exit 1; }
command -v codex >/dev/null 2>&1 || { echo "codex CLI not found in PATH." >&2; exit 1; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROMPTS_CSV="$SCRIPT_DIR/prompts.csv"
OUTPUT_ROOT="$SCRIPT_DIR/../../outputs/prompt-robustness-runs"
RUN_TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
RUN_DIR="$OUTPUT_ROOT/$RUN_TIMESTAMP"
CURSOR_FILE="$RUN_DIR/prompts.cursor"
CODEX_LOG_OUT="$RUN_DIR/codex_last.out"
CODEX_LOG_ERR="$RUN_DIR/codex_last.err"
PROTOCOL_LOG="$RUN_DIR/protocol_log.jsonl"
WORKSPACE_BACKUP_DIR="$RUN_DIR/workspace_backup"
EFFORT="low"
CODEX_CD=""
CODEX_CD_RUN=""
BASE_DIR="$SCRIPT_DIR"
EXTRA_ARGS=()
PROMPT_CURSOR_OVERRIDE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --cd)
      CODEX_CD="${2:-}"
      shift 2
      ;;
    --prompt-cursor)
      PROMPT_CURSOR_OVERRIDE="${2:-}"
      shift 2
      ;;
    --effort)
      EFFORT="${2:-}"
      shift 2
      ;;
    --effort=*)
      EFFORT="${1#*=}"
      shift
      ;;
    --)
      shift
      EXTRA_ARGS+=("$@")
      break
      ;;
    *)
      EXTRA_ARGS+=("$1")
      shift
      ;;
  esac
done

if [[ -z "$EFFORT" ]]; then
  EFFORT="low"
fi

EFFORT="$(printf '%s' "$EFFORT" | tr '[:upper:]' '[:lower:]')"

if [[ ! -f "$PROMPTS_CSV" ]]; then
  echo "prompts.csv not found at $PROMPTS_CSV" >&2
  exit 1
fi

mkdir -p "$RUN_DIR"

if [[ ! -d "$OUTPUT_ROOT" ]]; then
  mkdir -p "$OUTPUT_ROOT"
fi

if [[ -d "$OUTPUT_ROOT" ]]; then
  mapfile -t RUN_DIRS < <(ls -1dt "$OUTPUT_ROOT"/*/ 2>/dev/null || true)
  if [[ ${#RUN_DIRS[@]} -gt 10 ]]; then
    for ((i=10; i<${#RUN_DIRS[@]}; i++)); do
      rm -rf "${RUN_DIRS[$i]}"
    done
  fi
fi

if [[ -n "$CODEX_CD" ]]; then
  BASE_DIR="$CODEX_CD"
  if command -v wslpath >/dev/null 2>&1; then
    if [[ "$CODEX_CD" == *:* ]]; then
      BASE_DIR="$(wslpath -u "$CODEX_CD")"
    fi
  fi
  if [[ "$BASE_DIR" != /* ]]; then
    BASE_DIR="$SCRIPT_DIR/$BASE_DIR"
  fi
  if [[ -d "$BASE_DIR" ]]; then
    BASE_DIR="$(cd "$BASE_DIR" && pwd)"
  fi
  CODEX_CD_RUN="$CODEX_CD"
  if command -v wslpath >/dev/null 2>&1; then
    if [[ "$CODEX_CD_RUN" == *:* ]]; then
      CODEX_CD_RUN="$(wslpath -u "$CODEX_CD_RUN")"
    fi
  fi
fi

if [[ -n "$PROMPT_CURSOR_OVERRIDE" ]]; then
  if ! [[ "$PROMPT_CURSOR_OVERRIDE" =~ ^[0-9]+$ ]]; then
    echo "Invalid --prompt-cursor value (expected integer): $PROMPT_CURSOR_OVERRIDE" >&2
    exit 1
  fi
  printf '%s\n' "$PROMPT_CURSOR_OVERRIDE" > "$CURSOR_FILE"
fi

WORKSPACE_ROOT="$(
  BASE_DIR="$BASE_DIR" NLSS_WORKSPACE="${NLSS_WORKSPACE:-}" python3 - <<'PY'
import os
import sys
from pathlib import Path

base_dir = Path(os.environ["BASE_DIR"])
env_ws = os.environ.get("NLSS_WORKSPACE")
if env_ws:
    p = Path(env_ws).expanduser()
    if (p / "nlss-workspace.yml").exists():
        print(p)
        sys.exit(0)
    print(f"NLSS_WORKSPACE does not contain nlss-workspace.yml: {p}", file=sys.stderr)
    sys.exit(2)

parent = base_dir.parent
if (parent / "nlss-workspace.yml").exists():
    print(parent)
    sys.exit(0)

if (base_dir / "nlss-workspace.yml").exists():
    print(base_dir)
    sys.exit(0)

child_candidates = [
    d for d in base_dir.iterdir()
    if d.is_dir() and (d / "nlss-workspace.yml").exists()
]
if len(child_candidates) == 1:
    print(child_candidates[0])
    sys.exit(0)
if len(child_candidates) > 1:
    print("Multiple nlss-workspace.yml files found in child directories; set NLSS_WORKSPACE explicitly.", file=sys.stderr)
    sys.exit(2)

candidates = [
    d for d in parent.iterdir()
    if d.is_dir() and (d / "nlss-workspace.yml").exists()
]
if len(candidates) == 1:
    print(candidates[0])
    sys.exit(0)
if len(candidates) == 0:
    print("No nlss-workspace.yml found in parent or sibling directories. Use --cd or set NLSS_WORKSPACE.", file=sys.stderr)
    sys.exit(2)
print("Multiple nlss-workspace.yml files found; set NLSS_WORKSPACE explicitly.", file=sys.stderr)
sys.exit(2)
PY
)"

DATASET_DIR="$(
  WORKSPACE_ROOT="$WORKSPACE_ROOT" python3 - <<'PY'
import os
import sys
from pathlib import Path

workspace = Path(os.environ["WORKSPACE_ROOT"])
manifest = workspace / "nlss-workspace.yml"
if not manifest.exists():
    print(f"Missing workspace manifest: {manifest}", file=sys.stderr)
    sys.exit(2)

active = None
for line in manifest.read_text().splitlines():
    s = line.strip()
    if s.startswith("active_dataset:"):
        active = s.split(":", 1)[1].strip().strip("'\"")
        break

if active:
    candidate = workspace / active
    if candidate.is_dir():
        print(candidate)
        sys.exit(0)
    print(f"active_dataset directory not found: {candidate}", file=sys.stderr)
    sys.exit(2)

candidates = []
for d in workspace.iterdir():
    if not d.is_dir():
        continue
    if (d / "analysis_log.jsonl").exists() or (d / "report_canonical.md").exists():
        candidates.append(d)

if len(candidates) == 1:
    print(candidates[0])
    sys.exit(0)
if len(candidates) == 0:
    print("No dataset folder found in workspace.", file=sys.stderr)
    sys.exit(2)
print("Multiple dataset folders found; set active_dataset in nlss-workspace.yml.", file=sys.stderr)
sys.exit(2)
PY
)"

rm -rf "$WORKSPACE_BACKUP_DIR"
mkdir -p "$WORKSPACE_BACKUP_DIR"
cp -a "$DATASET_DIR"/. "$WORKSPACE_BACKUP_DIR"/

restore_workspace() {
  rm -rf "$DATASET_DIR"/*
  rm -rf "$DATASET_DIR"/.[!.]* "$DATASET_DIR"/..?* || true
  cp -a "$WORKSPACE_BACKUP_DIR"/. "$DATASET_DIR"/
}

while true; do
  mapfile -t OUT < <(
    PROMPTS_CSV="$PROMPTS_CSV" CURSOR_FILE="$CURSOR_FILE" python3 - <<'PY'
import csv
import os
import sys
from pathlib import Path

prompts_csv = Path(os.environ["PROMPTS_CSV"])
cursor_file = Path(os.environ["CURSOR_FILE"])

with prompts_csv.open(newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    rows = list(reader)

if not rows:
    print("prompts.csv is empty.", file=sys.stderr)
    sys.exit(2)

cursor = 1
if cursor_file.exists():
    try:
        cursor = int(cursor_file.read_text().strip())
    except Exception:
        cursor = 1

if cursor < 1 or cursor > len(rows):
    cursor = 1

subskill = rows[cursor - 1].get("subskill", "")
prompt = rows[cursor - 1].get("nl_prompt", "")
if not prompt:
    print(f"nl_prompt missing at row {cursor}.", file=sys.stderr)
    sys.exit(2)

import io
buf = io.StringIO()
writer = csv.DictWriter(buf, fieldnames=reader.fieldnames)
writer.writerow(rows[cursor - 1])
row_line = buf.getvalue().strip("\r\n")

next_idx = cursor + 1
if next_idx > len(rows):
    next_idx = 1

print(row_line)
print(subskill)
print(prompt)
print(next_idx)
PY
  )

  ROW_LINE="${OUT[0]}"
  SUBSKILL="${OUT[1]}"
  PROMPT="${OUT[2]}"
  NEXT_INDEX="${OUT[3]}"
  FINAL_PROMPT="\$nlss ${PROMPT} No follow-ups please."
  printf '%s\n' "$FINAL_PROMPT"

  codex_ok=1
  run_codex() {
    local effort="$1"
    local -a config_args=()
    if [[ -n "$effort" ]]; then
      config_args=(--config "model_reasoning_effort=$effort")
    fi
    local run_dir="${CODEX_CD_RUN:-${CODEX_CD:-$WORKSPACE_ROOT}}"
    printf '%s' "$FINAL_PROMPT" | codex exec --cd "$run_dir" "${config_args[@]}" --sandbox workspace-write --skip-git-repo-check "${EXTRA_ARGS[@]}" - >"$CODEX_LOG_OUT" 2>"$CODEX_LOG_ERR"
  }

  if [[ -n "$EFFORT" ]]; then
    if [[ "$EFFORT" == "xhigh" || "$EFFORT" == "extra_high" || "$EFFORT" == "extra-high" || "$EFFORT" == "extra high" ]]; then
      if run_codex "xhigh"; then
        codex_ok=0
      else
        printf '%s\n' "info: reasoning effort 'xhigh' failed; falling back to 'high'."
        if run_codex "high"; then
          codex_ok=0
        fi
      fi
    else
      if run_codex "$EFFORT"; then
        codex_ok=0
      fi
    fi
  else
    if run_codex ""; then
      codex_ok=0
    fi
  fi

  if [[ $codex_ok -ne 0 ]]; then
    printf '%s\n' "error: codex exec failed; see $CODEX_LOG_OUT and $CODEX_LOG_ERR" >&2
    restore_workspace
    exit 1
  fi

  DATASET_DIR="$DATASET_DIR" PROTOCOL_LOG="$PROTOCOL_LOG" TARGET_SUBSKILL="$SUBSKILL" TARGET_PROMPT="$PROMPT" TARGET_ROW="$ROW_LINE" python3 - <<'PY'
import os
import json
from pathlib import Path

dataset_dir = Path(os.environ["DATASET_DIR"])
protocol = Path(os.environ["PROTOCOL_LOG"])
analysis = dataset_dir / "analysis_log.jsonl"
target_subskill = os.environ.get("TARGET_SUBSKILL", "")
target_prompt = os.environ.get("TARGET_PROMPT", "")
target_row = os.environ.get("TARGET_ROW", "")

if analysis.exists():
    lines = analysis.read_text(encoding="utf-8").splitlines()
    if lines:
        with protocol.open("a", encoding="utf-8") as f:
            if target_row or target_subskill or target_prompt:
                header = {
                    "prompt_csv_line": target_row,
                    "target_subskill": target_subskill,
                    "target_prompt": target_prompt,
                }
                f.write(json.dumps(header, ensure_ascii=False) + "\n")
            for line in lines:
                if not line.strip():
                    continue
                try:
                    obj = json.loads(line)
                except Exception:
                    f.write("target_subskill={}\ttarget_prompt={}\t{}\n".format(target_subskill, target_prompt, line))
                    continue
                if obj.get("module") == "init_workspace":
                    continue
                merged = {"target_subskill": target_subskill, "target_prompt": target_prompt}
                for k, v in obj.items():
                    if k in merged:
                        continue
                    merged[k] = v
                f.write(json.dumps(merged, ensure_ascii=False) + "\n")
        pass
PY
  restore_workspace
  printf '%s\n' "$NEXT_INDEX" > "$CURSOR_FILE"
done
