---
name: init-workspace-r
description: Initialize per-dataset workspace folders with scratchpad.md, apa_report.md, analysis_log.jsonl, and .parquet dataset copies plus environment metadata.
---

# Init Workspace (Base R)

## Overview

Create dataset workspace folders under the workspace root (current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir`):

- For each dataset, a subfolder `<workspace-root>/<dataset-name>/` containing `scratchpad.md`, `apa_report.md`, `analysis_log.jsonl`, and `<dataset-name>.parquet`.
- An appended `analysis_log.jsonl` entry (module: `init_workspace`) per dataset when logging is enabled.
- If no datasets are provided, a placeholder folder `<workspace-root>/workspace/` is created with a scratchpad and APA report noting that no datasets were supplied.
- `core-stats-workspace.yml` is created/updated in the workspace root to track datasets and `active_dataset`.
- Nested or sibling workspace manifests are not allowed; the script stops if another manifest is detected in an ancestor, descendant, or sibling folder.

## Core Workflow

1. Provide one or more datasets (CSV/SAV/RDS/RData/Parquet) or run without data.
2. Run `scripts/R/init_workspace.R` or use the Windows wrapper.
3. Use the dataset workspace `scratchpad.md` to plan analysis steps and track transformations.

## Script: `scripts/R/init_workspace.R`

Input paths must point to local files that R can access. URLs or cloud share links are not supported; download the data first. When using the Windows wrapper, Windows paths like `C:\Data\study.sav` are converted to WSL paths automatically; for WSL direct runs, use `/mnt/<drive>/...`.

### Windows wrapper (WSL first, Windows fallback)

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\init_workspace.R> --csv <path to CSV>
```

### CSV input

```bash
Rscript <path to scripts/R/init_workspace.R> --csv <path to CSV file>
```

### RDS input (data frame)

```bash
Rscript <path to scripts/R/init_workspace.R> --rds <path to RDS file>
```

### RData input (data frame by name)

```bash
Rscript <path to scripts/R/init_workspace.R> --rdata <path to RData file> --df <data frame name>
```

### Parquet input

```bash
Rscript <path to scripts/R/init_workspace.R> --parquet <path to parquet file>
```

### Multiple datasets

Provide comma-separated paths for `--csv`, `--sav`, `--rds`, `--rdata`, or `--parquet`. When using multiple RData files, pass matching `--df` names (or a single name to reuse).

```bash
Rscript <path to scripts/R/init_workspace.R> \
  --csv data1.csv,data2.csv \
  --rds df.rds
```

### Interactive prompts

```bash
Rscript <path to scripts/R/init_workspace.R> --interactive
```

### Options

- Defaults are loaded from `core-stats/scripts/config.yml`; CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--agent` defaults to `modules.init_workspace.agent` (fallback to `CODEX_AGENT`, else `Codex`).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `core-stats-workspace.yml`; fallback to `defaults.output_dir` in `core-stats/scripts/config.yml`; not user-overridable).
- `scratchpad.md`: YAML front matter plus dataset planning sections (written inside each dataset workspace).
- `apa_report.md`: APA-style workspace summary using a YAML template plus front matter (per dataset workspace).
- `<dataset-name>.parquet`: One copy per referenced dataset, stored in `<workspace-root>/<dataset-name>/`.
- `analysis_log.jsonl`: Appended entry with module `init_workspace` and dataset summary (per dataset workspace).
- `core-stats-workspace.yml`: Workspace manifest in the workspace root (tracks dataset paths and `active_dataset`).

## APA 7 Template (YAML)

The APA output uses `core-stats/assets/init-workspace/default-template.md` by default. The path can be overridden via `templates.init_workspace.default` in `core-stats/scripts/config.yml`.

### Table column keys

Available column keys for `table.columns` include:

`dataset`, `type`, `rows`, `columns`, `source_path`, `copy_path`.

Use `drop_if_empty: true` to hide optional columns (paths/object name).

### Template tokens

In addition to the base tokens (`analysis_label`, `analysis_flags`, `table_number`, `table_body`, `note_body`, `narrative`), the module provides:

`yaml_front_matter`, `created_at`, `workspace_path`, `os`, `r_version`, `agent`, `dataset_count`, `dataset_list`.

## Scratchpad Template

The scratchpad output uses `core-stats/assets/scratchpad/default-template.md` by default. The path can be overridden via `templates.init_workspace.scratchpad`.

### Scratchpad tokens

`created_at`, `workspace_path`, `os`, `r_version`, `agent`, `dataset_sections`.

The `dataset_sections` token renders one `#`-level section per dataset, each with:

- `## Analysis Plan` (hierarchical to-do list).
- `## To Be Considered` (notes on missing data, recoding, scales, transformations).
