# Plot Deliberate Test Plan

## Scope

- Cover all plot types: histogram, density, box, violin, bar, scatter, line, QQ, corr-heatmap.
- Exercise input modes (csv/rds/rdata/parquet, sav when available), interactive prompts, and help output.
- Validate figure numbering, file naming (prefix/suffix), overwrite behavior, and output formats.
- Confirm option coverage for bins/binwidth, bw, smooth/se/span, summary, na-action, palette/theme, and percent-base.
- Include template override coverage (path and key) plus log on/off behavior.
- Include negative cases with expected errors and no log append.

## Prereqs

- Run from the repo root.
- Rscript available.
- R packages installed: `arrow`, `yaml`, `jsonlite`.
- Optional: `ggplot2` for ggplot output (base fallback used if missing).
- Optional: `haven` for `.sav` input coverage (skipped if missing).
- Interactive coverage uses `NLSS_PROMPT_FILE` to feed prompts during automation.

## Run Sequence (Deliberate)

1) Initialize workspace on `tests/data/golden_dataset.csv`.
2) Help output + workspace-default run.
3) Auto-numbered plots for histogram, bar percent, scatter with smooth.
4) Figure-number override, prefix/suffix, format (pdf/svg), and overwrite behavior.
5) Input mode checks (rds/rdata, sav if available).
6) Remaining plot types: density, box/violin, QQ, corr-heatmap.
7) Template override checks (path + key).
8) Missing-category note with `--na-action keep`.
9) Interactive run.
10) Negative tests (invalid type, nonnumeric corr-heatmap).

## Clean Cases

- Histogram with bins + workspace default input.
- Bar percent with group + percent-base group + position stack.
- Scatter with smooth/SE/span, palette/theme.
- Line with summary + figure-number override + file prefix/suffix + pdf output.

## Edge Cases

- Density with custom `--bw` and `--alpha`.
- QQ plot with `--log FALSE` (log unchanged).
- Corr-heatmap with `--digits` override.
- Overwrite path creation when files already exist.
- Missing-category note with `--na-action keep` (custom CSV).

## Negative Cases

- Unsupported plot type.
- Corr-heatmap with nonnumeric variables.

## Outputs

- `analysis_log.jsonl` under each plot workspace.
- `report_canonical.md` with appended figure blocks (templates applied).
- Plot images under each dataset workspace `plots/` folder.

## Script

Run via:

```bash
bash tests/run_plot_tests.sh
```

Or include it in deliberate/all via:

```bash
bash cmdscripts/tests.sh deliberate
bash cmdscripts/tests.sh all
```