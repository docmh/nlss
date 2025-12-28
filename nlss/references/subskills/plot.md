---
name: plot
description: Plot figures (histograms, bar charts, box/violin, scatter/line, QQ, correlation heatmaps) with APA-ready figure blocks, per-dataset figure numbering, and image files saved in a plots subfolder.
---

# Plot (ggplot2, APA 7)

## Overview

Generate APA-ready figures (with numbered captions) and save plot images to a `plots/` subfolder inside the dataset workspace. Each run appends a figure block to `report_canonical.md` and adds an entry to `analysis_log.jsonl`.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose a plot type and variables.
3. Run `scripts/R/plot.R` with the correct flags, or use the PowerShell wrapper on Windows to auto-locate Rscript.
4. Use outputs (`report_canonical.md`, figure images in `plots/`, and `analysis_log.jsonl`) to craft the response.

## Script: `scripts/R/plot.R`

### Windows wrapper (WSL first, Windows fallback)

The shared wrapper lives at `scripts/run_rscript.ps1` (relative to this skill folder). It uses WSL first and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts\run_rscript.ps1> <path to scripts\R\plot.R> --csv <path to CSV file> --type histogram --vars age
```

### CSV input

```bash
Rscript <path to scripts/R/plot.R> --csv <path to CSV file> --type scatter --x age --y score --group condition
```

### Parquet input

```bash
Rscript <path to scripts/R/plot.R> --parquet <path to parquet file> --type bar --vars gender --stat percent
```

### Interactive prompts

```bash
Rscript <path to scripts/R/plot.R> --interactive
```

## Options

- Defaults are loaded from `nlss/scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--type` selects a plot type (default: `modules.plot.type`).
- `--vars` selects variables (comma-separated). Defaults to `modules.plot.vars_default` when omitted (numeric unless `--type bar`).
- `--x` / `--y` select X/Y variables (use for scatter/line).
- `--group` applies grouping/coloring (optional).
- `--stat` controls bar chart summary (`count` or `percent`, default: `modules.plot.stat`).
- `--percent-base` sets percent base (`total` or `group`, default: `modules.plot.percent_base`).
- `--bins` / `--binwidth` set histogram binning (defaults: `modules.plot.bins` and `modules.plot.binwidth`).
- `--bw` sets density bandwidth (default: `modules.plot.bw`).
- `--smooth` / `--se` / `--span` control scatter smoothing (defaults: `modules.plot.smooth`, `modules.plot.se`, `modules.plot.span`).
- `--summary` controls line summary (`none`, `mean`, `median`, default: `modules.plot.summary`).
- `--na-action` controls missing handling (`omit` or `keep`, default: `modules.plot.na_action`).
- `--alpha` and `--position` control transparency and bar positioning (defaults: `modules.plot.alpha`, `modules.plot.position`).
- `--theme` and `--palette` control styling (defaults: `modules.plot.theme`, `modules.plot.palette`).
- `--title`, `--subtitle`, `--caption`, `--note` override figure text (optional).
- `--format` / `--width` / `--height` / `--dpi` control image output (defaults: `modules.plot.format`, `modules.plot.width`, `modules.plot.height`, `modules.plot.dpi`).
- `--file-prefix` / `--file-suffix` customize filenames (default prefix: `modules.plot.file_prefix`).
- `--figure-number` overrides the starting figure number (otherwise computed from `report_canonical.md`).
- `--overwrite` allows overwriting existing plot files (default: `FALSE`).
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path (optional).
- `--log` toggles JSONL logging (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the JSONL log (optional).

## Outputs

Subskills append to `report_canonical.md` and do not create separate report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.

- Outputs are written to the dataset workspace at `<workspace-root>/<dataset-name>/` (workspace root = current directory, its parent, or a one-level child containing `nlss-workspace.yml`; fallback to `defaults.output_dir` in `nlss/scripts/config.yml`; not user-overridable).
- Plot images are saved in `<workspace-root>/<dataset-name>/plots/` with a figure-numbered filename (for example `figure-001-histogram-age.png`).
- `report_canonical.md`: APA-ready figure blocks with **Figure N** numbering (independent from table numbering).
- `analysis_log.jsonl`: Machine-readable results and options (appended per run when logging is enabled).

## APA Figure Template (YAML)

Use the Markdown template at `nlss/assets/plot/default-template.md` when assembling plot reports. If the template exists, `plot.R` uses it for `report_canonical.md`.

- The template path can be overridden via `templates.plot.default` in `nlss/scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `narrative.row_template`: repeats one block per figure.
  - `narrative.join`: join string between figure blocks.

### Figure tokens (global + per-row)

Available tokens include:

`figure_number`, `figure_title`, `figure_caption`, `figure_note`, `figure_path`, `plot_type`, `vars`, `x`, `y`, `group`, `n`, `missing_n`, `missing_pct`, `analysis_label`, `analysis_flags`, `figure_body`, `narrative`.

Use `narrative.row_template` to render one figure block per plot.

## APA 7 Reporting Guidance

- Use `Figure N. Title` for captions and keep titles concise.
- If missing values were omitted, note it in the figure note.
- When plotting grouped data, mention the grouping variable in the caption or note.

## Dependencies

- Primary plotting engine: `ggplot2` (required for full feature support).
- If `ggplot2` is unavailable or incompatible, the script falls back to base R plotting for the supported figure types.
- Required for Parquet input: `arrow`.
