---
name: init-workspace
description: Create workspace root and per-dataset folders with scratchpad.md, report_canonical.md, analysis_log.jsonl, parquet copies, and nlss-workspace.yml; supports multiple datasets and blocks nested manifests.
license: Apache-2.0
---

# Init Workspace (Base R)

## Overview

Create dataset workspace folders under the workspace root (current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir`):

- For each dataset, a subfolder `<workspace-root>/<dataset-name>/` containing `scratchpad.md`, `report_canonical.md`, `analysis_log.jsonl`, and `<dataset-name>.parquet`.
- An appended `analysis_log.jsonl` entry (module: `init_workspace`) per dataset when logging is enabled.
- If no datasets are provided, a placeholder folder `<workspace-root>/workspace/` is created with a scratchpad and NLSS format report noting that no datasets were supplied.
- `nlss-workspace.yml` is created/updated in the workspace root to track datasets and `active_dataset`.
- Nested or sibling workspace manifests are not allowed; the script stops if another manifest is detected in an ancestor, descendant, or sibling folder.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Provide one or more datasets (CSV/SAV/RDS/RData/Parquet) or run without data.
2. Run `scripts/R/init_workspace.R`.
3. Use the dataset workspace `scratchpad.md` to plan analysis steps and track transformations.

## Script: `scripts/R/init_workspace.R`

Input paths must point to local files that R can access. URLs or cloud share links are not supported; download the data first. Use Windows-style paths in PowerShell (for example `C:\Data\study.sav`) and WSL-style paths in WSL (for example `/mnt/c/Data/study.sav`).

### CSV Input

```bash
Rscript <path to scripts/R/init_workspace.R> --csv <path to CSV file>
```

### RDS Input (Data Frame)

```bash
Rscript <path to scripts/R/init_workspace.R> --rds <path to RDS file>
```

### RData Input (Data Frame by Name)

```bash
Rscript <path to scripts/R/init_workspace.R> --rdata <path to RData file> --df <data frame name>
```

### Parquet Input

```bash
Rscript <path to scripts/R/init_workspace.R> --parquet <path to parquet file>
```

### Multiple Datasets

Provide comma-separated paths for `--csv`, `--sav`, `--rds`, `--rdata`, or `--parquet`. When using multiple RData files, pass matching `--df` names (or a single name to reuse).

```bash
Rscript <path to scripts/R/init_workspace.R> \
  --csv data1.csv,data2.csv \
  --rds df.rds
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/init_workspace.R> --interactive
```

### Options

- Defaults are loaded from `scripts/config.yml`; CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--agent` defaults to `modules.init_workspace.agent` (fallback to `CODEX_AGENT`, else `Codex`).
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `scripts/config.yml`; not user-overridable).
- `scratchpad.md`: YAML front matter plus dataset planning sections (written inside each dataset workspace).
- `report_canonical.md`: NLSS format workspace summary using a YAML template plus front matter (per dataset workspace).
- `<dataset-name>.parquet`: One copy per referenced dataset, stored in `<workspace-root>/<dataset-name>/`.
- `analysis_log.jsonl`: Appended entry with module `init_workspace` and dataset summary (per dataset workspace).
- `analysis_log_seq`: Monotonic log counter stored in `nlss-workspace.yml` (when checksum logging is enabled); if `analysis_log.jsonl` is missing, the counter restarts at 1.
- `nlss-workspace.yml`: Workspace manifest in the workspace root (tracks dataset paths and `active_dataset`).

## NLSS format Template (YAML)

The NLSS format output uses `assets/init-workspace/default-template.md` by default. The path can be overridden via `templates.init_workspace.default` in `scripts/config.yml`.

### Table Column Keys

Available column keys for `table.columns` include:

`dataset`, `type`, `rows`, `columns`, `source_path`, `copy_path`.

Use `drop_if_empty: true` to hide optional columns (paths/object name).

### Template Tokens

In addition to the base tokens (`analysis_label`, `analysis_flags`, `table_number`, `table_body`, `note_body`, `narrative`), the module provides:

`yaml_front_matter`, `created_at`, `workspace_path`, `os`, `r_version`, `agent`, `dataset_count`, `dataset_list`.

## Scratchpad Template

The scratchpad output uses `assets/scratchpad/default-template.md` by default. The path can be overridden via `templates.init_workspace.scratchpad`.

### Scratchpad Tokens

`created_at`, `workspace_path`, `os`, `r_version`, `agent`, `dataset_sections`.

The `dataset_sections` token renders one `#`-level section per dataset, each with:

- `## Analysis Plan` (hierarchical to-do list).
- `## To Be Considered` (notes on missing data, recoding, scales, transformations).