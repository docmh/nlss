# Plot Deliberate Test Plan

## Scope

- Cover all plot types: histogram, density, box, violin, bar, scatter, line, QQ, corr-heatmap.
- Exercise input modes (csv/rds/rdata/parquet, sav when available), interactive prompts, and help output.
- Validate figure numbering, file naming (prefix/suffix), overwrite behavior, and output formats.
- Confirm option coverage for bins/binwidth, bw, smooth/se/span, summary, na-action, palette/theme, and percent-base.
- Include template override coverage (path and key) plus log on/off behavior.
- Preserve every legacy case and check negative cases produce one failed run without modifying earlier reports, logs, figures or run artifacts.
- Verify immutable run-local figure files, `plot-data.rds`, Markdown and artifact hashes, including `--log FALSE` and compatibility overwrites.
- Keep this compatibility suite distinct from the independent numerical, import, replay and publication acceptance in `tests/phase2/run_plot_tests.R`.

## Prereqs

- Run from the repo root.
- Rscript available.
- R packages installed: `arrow`, `yaml`, `jsonlite`, `digest`, `ggplot2`.
- Optional: Cairo capability for the native `grDevices::svg` device; PNG and PDF cases always run. SVG does not require svglite.
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
10) Negative tests (invalid type, nonnumeric corr-heatmap), with unchanged compatibility files and earlier run artifacts.
11) Verify every terminal plot bundle and image signature; no pending run or retained analysis lock may remain.

## Clean Cases

- Histogram with bins + workspace default input.
- Bar percent with group + percent-base group + position stack.
- Scatter with smooth/SE/span, palette/theme.
- Line with summary + figure-number override + file prefix/suffix + pdf output.

## Edge Cases

- Density with custom `--bw` and `--alpha`.
- QQ plot with `--log FALSE` (log bytes unchanged; a completed bundle with raw results and figure files remains mandatory).
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
- Immutable `runs/<run-id>/` bundles containing request/result JSON, Markdown, numerical plot data and figure files.
- Template-key overrides in a private `NLSS_CONFIG_PATH`; the repository's canonical YAML is never modified by this runner.
- Preserved workspace manifests and snapshots for post-run replay and visual review.

RData initialization and subsequent plotting both name the same `--df` object.
The initial Wave 10 baseline exposed an import hash mismatch between an
internally named and an ordinary CLI scalar. The valid workflow remains a
success case; the shared import descriptor normalizes the incidental R name
attribute rather than weakening source verification.

## Script

Run via:

```bash
bash tests/smoke/run_plot_tests.sh
```

For an isolated output directory with no retention cleanup:

```bash
NLSS_TEST_ROOT=/tmp/nlss-plot-review NLSS_KEEP_RUNS=0 bash tests/smoke/run_plot_tests.sh
```

The registered full independent runner is the fourteenth Phase 2 runner.
General smoke selects only `tests.suites.smoke.plot_match` from `tests/tests.yml`;
full `phase2`/`all` retains every independent plot case.

Or include it in deliberate/all via:

```bash
bash cmdscripts/tests.sh deliberate
bash cmdscripts/tests.sh all
```
