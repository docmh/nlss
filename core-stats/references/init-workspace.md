---
name: init-workspace-r
description: Initialize a workspace folder with scratchpad.md, apa_report.md, and .parquet dataset copies plus environment metadata.
---

# Init Workspace (Base R)

## Overview

Create a workspace folder (the configured output directory) with:

- `scratchpad.md` containing a YAML front matter block plus per-dataset sections for analysis planning.
- `apa_report.md` containing a YAML front matter block plus an APA-style summary of the workspace.
- `.parquet` copies of each referenced dataset (if any).
- An appended `analysis_log.jsonl` entry (module: `init_workspace`) when logging is enabled.

## Core Workflow

1. Provide one or more datasets (CSV/SAV/RDS/RData/Parquet) or run without data.
2. Run `scripts/R/init_workspace.R` or use the Windows wrapper.
3. Use the generated `scratchpad.md` to plan analysis steps and track transformations.

## Script: `scripts/R/init_workspace.R`

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

- Outputs are always written to `defaults.output_dir` from `core-stats/scripts/config.yml` (not user-overridable).
- `scratchpad.md`: YAML front matter plus per-dataset sections with an analysis plan and considerations.
- `apa_report.md`: APA-style workspace summary using a YAML template plus front matter.
- `*.parquet`: One copy per referenced dataset (saved in the output directory).
- `analysis_log.jsonl`: Appended entry with module `init_workspace` and dataset summary.

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
