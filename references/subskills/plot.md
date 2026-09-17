---
name: plot
description: Replayable ggplot2 figures with preserved images, numerical graphical layers, source-row and label provenance, and protected canonical report publication.
license: Apache-2.0
---

# Plot (ggplot2, NLSS format)

## Overview

Generate histogram, density, bar, box, violin, scatter, line, normal QQ and Pearson
correlation-heatmap figures. Each run preserves its images, unrounded graphical
data and effective options under the [run contract](../run-contract.md). It also
appends numbered figures to the root `report_canonical.md` and preserves run evidence.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, SAV, RDS, RData data frame, Parquet, or interactive).
2. Choose a plot type and variables.
3. Run the `plot` operation through `run_nlss.R` with the correct flags.
4. Use outputs (`report_canonical.md`, figure images in `plots/`, and `result.json`) to craft the response.

Variable labels label axes; value labels describe categories without replacing
their identities. Labels alone do not turn a numeric measure into a categorical
axis: choose its role through the plot type and variable options. For an existing
figure, inspect its request, case selection and numerical layers before drawing
conclusions. A request to explain a saved figure does not itself authorize a rerun.

## Execution: `plot`

Use the [shared launcher](../../SKILL.md#rscript-execution-required); `<skill>`
is the installed NLSS skill directory.

### CSV Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" plot --csv <path to CSV file> --type scatter --x age --y score --group condition
```

### Parquet Input

```bash
Rscript "<skill>/scripts/R/run_nlss.R" plot --parquet <path to parquet file> --type bar --vars gender --stat percent
```

### Interactive Prompts

```bash
Rscript "<skill>/scripts/R/run_nlss.R" plot --interactive
```

## Options

- Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.
- `--sep` and `--header` use `defaults.csv.sep` and `defaults.csv.header` when omitted.
- `--type` selects a plot type (default: `modules.plot.type`).
- `--vars` selects variables (comma-separated). Omitted selections use `modules.plot.vars_default`; bar plots can also plot numeric category codes. Explicit selections are preferable to plotting every eligible column.
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
- `--title`, `--subtitle`, `--caption` override figure text; `--note` adds custom context while retaining case-selection and availability caveats (optional).
- `--format` / `--width` / `--height` / `--dpi` control image output (defaults: `modules.plot.format`, `modules.plot.width`, `modules.plot.height`, `modules.plot.dpi`). PNG/PDF/SVG and the retained portable JPEG/JPG, TIFF/TIF, BMP, EPS/PS routes use explicit R devices. Device support depends on the R build; unsupported devices fail visibly, without installing packages.
- `--file-prefix` / `--file-suffix` customize filenames (default prefix: `modules.plot.file_prefix`).
- `--figure-number` overrides the canonical starting figure number (otherwise computed from `report_canonical.md`). Run-local numbering always begins at one.
- `--overwrite` allows overwriting compatibility copies in dataset `plots/` (default: `FALSE`). It never overwrites images inside an earlier run; without it filename collisions get a suffix.
- `--digits` controls rounding (default: `defaults.digits`).
- `--template` selects a template key or file path (optional).
- `--log` controls optional standalone logging; project run evidence and the root protocol remain enabled (default: `defaults.log`).
- `--user-prompt` stores the original AI prompt in the saved request, subject to configured prompt-privacy settings.

## Outputs

The Phase-2 per-run `output.md` is deterministic output, not a semantic final
research report. Authored reports use freely chosen visible Markdown paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- Plot images stay in `.nlss/runs/<run-id>/plots/`; root protocol links are rebased to those same files, without a second image tree.
- `report_canonical.md`: NLSS format-ready figure blocks with **Figure N** numbering (independent from table numbering).
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `.nlss/runs/<id>/request.json`: immutable input/dictionary references, effective
  configuration, variable roles, category mapping, source cases and device context.
- `.nlss/runs/<id>/result.json`: full unrounded results, warnings and artifact hashes;
  `plots` contains prepared data, summaries and graphical layers.
- `.nlss/runs/<id>/plot-data.rds`: the same graphical audit in native R form.
- `.nlss/runs/<id>/plots/*` and `output.md`: preserved images and run-local links.
  They remain mandatory with `--log FALSE` or disabled legacy output logging.

Publication protects the root protocol and existing evidence against ordinary
write failures. Failed or `.pending-*` runs are not completed figures. Replay
checks all saved artifact hashes and uses the fixed data/configuration/template.
Its numerical layers and Markdown are reproducible in the recorded environment;
new device metadata or font rendering are not promised to be byte-identical.

## Statistical and graphical interpretation

- Case counts refer to finite observations jointly usable for the requested
  figure. Numeric missing/non-finite values are excluded. `--na-action keep`
  retains missing categories as distinct categories, never merged with a literal
  `Missing`, `NA`, duplicate label or nearby numeric code. Selection and omitted
  rows remain recorded even when a custom note is supplied.
- Bar percentages divide by all retained observations (`total`) or the retained
  observations in each group (`group`). `position=fill` subsequently normalizes
  each x-category stack to one. Read its proportion axis and recorded denominator;
  it is not the same display as a 0–100 percent chart. See
  [ggplot2 bar semantics](https://ggplot2.tidyverse.org/reference/geom_bar.html).
  Stacking within-group percentages can exceed 100% because segments have
  different denominators; the stack total is not a sample percentage.
  When `stat=percent`, `percent-base=group` and `position=fill` are combined
  with a grouping variable, within-group percentages are rescaled to sum to
  one within each x-category. With unequal group sizes, these normalized
  within-group proportions do not represent the observed group composition
  of that category.
- Histograms retain requested bins/binwidth; density plots retain bandwidth.
  Box and violin statistics are ggplot2 statistics, not a substitute independent
  hypothesis test. Small groups may have unavailable layers, which must be
  disclosed. See [boxplot definitions](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)
  and [violin definitions](https://ggplot2.tidyverse.org/reference/geom_violin.html).
- Scatter smoothers are fit separately within groups. Their optional 95% bands
  are pointwise confidence bands, not prediction intervals or simultaneous bands.
  See [ggplot2 smoothing](https://ggplot2.tidyverse.org/reference/geom_smooth.html).
- Lines connect observations in x order within groups; `mean`/`median` aggregates
  at identical x values. Without an appropriate subject grouping this does not
  depict individual longitudinal trajectories. QQ plots compare each selected
  group's observations with theoretical normal quantiles.
- Heatmaps use Pearson correlations on a common listwise-complete finite sample,
  with fixed colour limits −1 to 1. They do not claim pairwise sample sizes or
  significance testing. An unavailable coefficient must not look like zero.

Choose and explain figures in relation to the research question, design and
uncertainty. Do not turn a smooth curve, visual separation or template caption
into automatic causal or inferential conclusions. Final reports may select and
reframe useful figures; they remain context-sensitive syntheses beyond templates.

## Compatibility corrections

Previously, a separate base-R fallback could ignore grouping, bin counts or fill
normalization. Plot now requires ggplot2 and preserves all nine types through it.
Nonsyntactic variable names are addressed through safe internal aliases; numeric
and categorical roles of the same source column remain distinct. Grouped QQ,
title/subtitle rendering, non-finite case accounting and fixed heatmap limits are
explicit. Unsupported option values fail instead of silently becoming defaults.

RData initialization now normalizes the selected object name before hashing:
an internal named-scalar attribute is not a changed import option. A preexisting
binding written with the old inconsistent hash is not silently adopted; inspect
the source and use explicit `--import-action new-version` when appropriate.

## NLSS format Figure Template (YAML)

Use the Markdown template at `assets/plot/default-template.md` when assembling plot reports. If the template exists, `plot.R` uses it for `report_canonical.md`.

- The template path can be overridden via `templates.plot.default` in `scripts/config.yml`.
- Templates use YAML front matter with `{{token}}` placeholders. Supported sections:
  - `narrative.row_template`: repeats one block per figure.
  - `narrative.join`: join string between figure blocks.

### Figure Tokens (Global + per-Row)

Available tokens include:

`figure_number`, `figure_title`, `figure_caption`, `figure_note`, `figure_path`, `plot_type`, `vars`, `x`, `y`, `group`, `n`, `missing_n`, `missing_pct`, `analysis_label`, `analysis_flags`, `figure_body`, `narrative`.

Use `narrative.row_template` to render one figure block per plot.

## NLSS format Reporting Guidance

- Use `Figure N. Title` for captions and keep titles concise.
- If missing values were omitted, note it in the figure note.
- When plotting grouped data, mention the grouping variable in the caption or note.

## Dependencies

- Plotting engine: `ggplot2`; viridis styling uses its `viridisLite` dependency.
- Required for Parquet input: `arrow`, plus common import/run dependencies.
- SVG requires a Cairo-capable R build. No automatic package installation or
  silent substitution of a different plotting engine is performed.
