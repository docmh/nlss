---
name: assumptions
description: Assumption and diagnostic checks for t-tests, ANOVA, regression, mixed models, and SEM (normality, homogeneity, sphericity, VIF, influence, convergence) with NLSS format outputs.
license: Apache-2.0
---

# Assumptions Checks (NLSS format)

## Overview

Run assumption and diagnostic checks for t-tests, ANOVA (between, within, mixed), regression (including multiple and hierarchical models), mixed models, and SEM/CFA/mediation/path models. The script outputs NLSS format-ready tables/narratives plus machine-readable run results.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Core Workflow

1. Identify the input type (CSV, RDS, RData data frame, Parquet, or interactive).
2. Choose the analysis family (`ttest`, `anova`, `regression`, `mixed_models`, or `sem`) and specify variables.
3. Run `scripts/R/assumptions.R` with the correct flags.
4. Use outputs (`report_canonical.md`, `result.json`) for reporting or downstream modules.

## Script: `scripts/R/assumptions.R`

### T-Test Assumptions (Independent Samples)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --vars score --group condition
```

### T-Test Assumptions (Paired)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis ttest --x pre_score --y post_score
```

### ANOVA Assumptions (Between-Subjects)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --dv score --between group,gender
```

### ANOVA Assumptions (Within or Mixed; Wide Format)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis anova --within pre,mid,post --between group
```

### Regression Assumptions (Multiple Regression)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --ivs age,stress,trait
```

### Regression Assumptions (Hierarchical Blocks)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis regression --dv outcome --blocks "age,gender;stress,trait"
```

### Mixed Models Assumptions

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis mixed_models --formula "score ~ time + (1|id)"
```

### SEM Assumptions (CFA Builder)

```bash
Rscript <path to scripts/R/assumptions.R> --csv <path to CSV file> --analysis sem --factors "F1=item1,item2;F2=item3,item4"
```

### Interactive Prompts

```bash
Rscript <path to scripts/R/assumptions.R> --interactive
```

## Options

Defaults are loaded from `scripts/config.yml` (requires R package `yaml`); CLI flags override config values.

- `--analysis` defaults to `modules.assumptions.analysis` (`auto`, `ttest`, `anova`, `regression`, `mixed_models`, `sem`; `cfa`, `path`, `mediation`, `invariance` also route to SEM checks).
- `--vars` defaults to `modules.assumptions.vars_default` (numeric columns).
- `--normality` defaults to `modules.assumptions.normality` (`shapiro` or `none`).
- `--homogeneity` defaults to `modules.assumptions.homogeneity` (`levene`, `bartlett`, `fligner`, `f`, `all`, `none`).
- `--linearity` defaults to `modules.assumptions.linearity` (TRUE/FALSE).
- `--homoscedasticity` defaults to `modules.assumptions.homoscedasticity` (TRUE/FALSE).
- `--vif` defaults to `modules.assumptions.vif` (TRUE/FALSE).
- `--durbin-watson` defaults to `modules.assumptions.durbin_watson` (TRUE/FALSE).
- `--outliers` defaults to `modules.assumptions.outliers` (TRUE/FALSE).
- `--influence` defaults to `modules.assumptions.influence` (TRUE/FALSE).
- `--alpha` defaults to `modules.assumptions.alpha`.
- Thresholds can be overridden via:
  - `--vif-warn` (`modules.assumptions.vif_warn`)
  - `--vif-high` (`modules.assumptions.vif_high`)
  - `--outlier-z` (`modules.assumptions.outlier_z`)
  - `--cook-multiplier` (`modules.assumptions.cook_multiplier`)
  - `--max-shapiro-n` (`modules.assumptions.max_shapiro_n`)
- Mixed models inputs: `--formula` or `--dv` + `--fixed` + `--random`.
  - `--reml` uses `modules.mixed_models.reml`.
  - `--optimizer` uses `modules.mixed_models.optimizer`.
  - `--maxfun` uses `modules.mixed_models.maxfun`.
  - Mixed-model assumption toggles: `--random-effects`, `--singular`, `--convergence`, `--dharma`, `--performance` (defaults from `modules.assumptions.mixed_models.*`).
- SEM inputs: `--model`, `--model-file`, `--paths`, `--factors`, or builders (`--dv`/`--ivs`, `--x`/`--m`/`--y`).
  - `--estimator` uses `modules.sem.estimator`.
  - `--missing` uses `modules.sem.missing`.
  - `--se` uses `modules.sem.se`.
  - `--ci` uses `modules.sem.ci`.
  - `--bootstrap`/`--bootstrap-samples` use `modules.sem.bootstrap`/`modules.sem.bootstrap_samples`.
  - `--seed` uses `modules.sem.seed` for reproducible SEM diagnostic refits (mixed-model seed defaults are separate, below).
  - `--std` uses `modules.sem.std`; the Heywood loading screen always uses `std.all`. `--std` and `--ci` remain recorded compatibility/context options; these diagnostic tables do not emit parameter confidence intervals or change the loading-screen scale.
  - SEM assumption toggles: `--mardia`, `--mahalanobis`, `--mahalanobis-alpha`, `--collinearity`, `--max-cor`, `--max-kappa`, `--heywood`, `--convergence` (defaults from `modules.assumptions.sem.*`).
- `--digits` uses `defaults.digits`.
- `--template` selects a template key or file path for NLSS format outputs (falls back to defaults).
- `--log` uses `defaults.log`.
- `--seed` controls mixed-model simulations (default
  `modules.assumptions.mixed_models.seed = 123`) or SEM refits (inherited
  `modules.sem.seed = 1`). Relevant fit defaults are frozen in each request.
- Unknown/duplicate flags, malformed Booleans, invalid numeric domains,
  conflicting sources and explicit options for another family are errors.
  `--covariates` is supported by the SEM builder, not standalone ANCOVA
  diagnostics. `--invariance` sequences belong to `sem.R`; here use
  `--analysis invariance --group ... --group-equal ...` for one constrained refit.

## Inputs and Handling

- Data sources: CSV, SAV, RDS, Parquet, or RData data frame (`--df` required for RData).
- `ttest`:
  - One-sample/independent: use `--vars` and optional `--group`.
  - Paired: use `--x` and `--y` lists of equal length.
- `anova`:
  - Between-subjects: use `--dv` and `--between`.
  - Within/mixed: use `--within` (wide format, one column per repeated measure) and optional `--between`.
- `regression`:
  - Multiple regression: use `--dv` and `--ivs`.
  - Hierarchical regression: use `--blocks` (semicolon-separated blocks; blocks are cumulative).
- `mixed_models`:
  - Use `--formula` (recommended) or `--dv` + `--fixed` + `--random`.
  - Requires the `lme4` package; random-effects terms are required.
- `sem`:
  - Use `--analysis sem/cfa/path/mediation/invariance` plus `--model`/`--model-file` or a builder (`--factors`, `--dv`/`--ivs`, `--x`/`--m`/`--y`).
  - Optional `--ordered` for ordered categorical indicators; `--group` for multigroup fits.
  - Provide only one explicit syntax source (`--model`, `--paths`, `--model-file`). Custom syntax retains legacy precedence over builder inputs, but ignored builder inputs and the chosen source are recorded in `diagnostics.model_specification` and disclosed. Explicit grouping requires at least two observed groups.
- Missing values are handled per test or model. SEM fits honor their selected
  missing-data method; continuous-data screens use their own recorded cases.

### Execution and common diagnostic boundaries

All five families use the [dataset-backed run/replay contract](../run-contract.md).
`.nlss/runs/<run-id>/request.json` preserves the input/dictionary hashes, resolved
options, case/model provenance, used configuration, seed/RNG and templates;
`result.json` preserves unrounded `results.checks_df` and `results.diagnostics`.
`output.md` is the deterministic table/narrative, not a semantic final report.
Replay creates a new run from this evidence; it does not rerun a mutable external
SEM model file or replace the working dataset. `--log FALSE` disables only the
optional standalone logging projection. Fatal errors after run initialization produce failed
bundles without normal output and protect previous report/log projections;
pre-input errors can end with stderr only.

Every requested check has `status` (`available`, `skipped`, `unavailable`).
Disabled checks remain absent. A missing estimate is not zero, and an unavailable
check has no pass/fail decision. Per-check thresholds are screens, not a global
approval/rejection rule. Match the actual recorded model, case selection,
estimator and grouping to the substantive analysis before interpreting results.
No family gains pooled multiple-imputation inference through this contract.

Numeric SPSS value labels remain metadata; they do not silently define factors.
Raw grouping identities and source rows are preserved separately from display
labels. Selected nonfinite numeric inputs are rejected. T-test diagnostics use
per-variable/group or complete-pair cases; normality for pairs concerns their
differences, not the two marginal distributions.

### ANOVA and regression diagnostic boundaries

ANOVA uses joint complete cases over the requested outcomes and factors. Wide
repeated measurements are aligned by source row; an explicit `--subject-id`
participates in complete-case selection and must be unique among retained cases.
Between-factor combinations retain unambiguous
identities even when display strings contain separators. Normality screens
concern observed cell values; they are not an ANCOVA residual diagnostic.
Mauchly uses `stats::mauchly.test(..., X = ~1)`, testing the within-contrast
covariance after fitting the full between-factorial model. Two repeated levels
need no sphericity test (`skipped`); singular/unestimable tests are `unavailable`.
This replaces the older raw-covariance test and can materially change results.

Hierarchical regression fits every cumulative block to common complete cases
over all requested variables. Each model's rank, residual degrees of freedom,
source rows and residual/influence vectors are recorded. The old correlation
of included predictors with OLS residuals was zero by construction; it is
replaced by **Quadratic added-term F**, adding one centered squared numeric
predictor at a time to that block on identical cases. This detects that limited
curvature only, not every nonlinear relationship; visual and design-based
assessment still matter. Unestimable added terms are unavailable.

The studentized Breusch–Pagan (Koenker) calculation uses effective auxiliary
design rank for degrees of freedom. Durbin–Watson is the statistic in retained
source-row order, without an inferential p-value or a claim of independence.
VIF is per model-matrix column, not a factor-term GVIF. Numerical linear dependence
(auxiliary residual sum of squares at most machine epsilon times the target
sum of squares) is explicitly high/infinite (JSON numeric value is null with an explanatory
note); failed VIF calculation is distinct. Undefined residual/Cook diagnostics
do not become zero outlier counts. Consider sample size, multiplicity and
model/design implications when discussing any flagged threshold.

Primary method references: [R Mauchly documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/mauchly.test.html),
[nested linear-model comparisons](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.lm.html),
[studentized Breusch–Pagan](https://search.r-project.org/CRAN/refmans/lmtest/html/bptest.html).

### SEM diagnostic boundaries

The SEM branch is a replayable **diagnostic refit**. Match its syntax, input,
grouping, estimator and missing-data settings
to the analysis. Actual lavaan case/group selection, effective settings,
convergence/admissibility, bootstrap counts, seed and warnings are recorded in
`results.diagnostics.fit`; this does not certify a different model.

Numeric labels do not change continuous variables into categorical indicators.
Declare ordinal indicators with `--ordered`, or preserve an existing ordered
factor type; factor levels or numeric-code ordering are preserved and recorded.
Distinct numeric ordinal codes retain their identity even when rounded display
strings would coincide. `diagnostics.reporting` separates requested `std`/`ci`
context from the effective `std.all` loading screen and absence of diagnostic
parameter intervals.
Selected infinite values and invalid model
roles are rejected. Shapiro screens use each continuous variable's observed
cases; Mardia, Mahalanobis and collinearity screens use jointly complete
continuous cases, pooled across groups. These screens are not residual tests or
group-specific normality tests. Their rows and model-fit cases may differ under
FIML, and the two selections are explicitly separated in the audit.

Mardia uses the exported `MVN::mardia` numeric result with population covariance
and asymptotic inference, avoiding rounded presentation strings. Mahalanobis
distances retain source-row identities. Requested but failed/inapplicable checks
remain visible as `unavailable`; Shapiro sample limits can be `skipped`. Disabled
checks remain absent. These availability guarantees apply to completed diagnostic
outputs. A fatal model-fit error terminates the run without a new normal report;
failed-run publication follows the shared contract above.
Neither missing estimates nor failed refits justify a zero Heywood/outlier count
or a passed assumption. The loading screen is inapplicable in a pure path model.

Standalone `--analysis invariance` checks one fit with the explicit
`--group-equal` constraints; it does not perform the SEM module's invariance
sequence. Interpret diagnostic flags with the design and estimator rather than
selecting substantive conclusions by automatic pass/fail rules.

Primary references: [MVN package manual](https://cran.r-project.org/web/packages/MVN/MVN.pdf),
[lavaan estimators and missing data](https://lavaan.ugent.be/tutorial/est.html).

### Mixed-model diagnostic boundaries

This standalone entrypoint refits its own model, now with saved-request replay.
Match its formula, input and ML/REML settings
to the fitted analysis; a diagnostic report does not retroactively certify
another model or imply pooled multiple-imputation inference.

The mixed-model branch records actual source cases and cluster assignments,
ML/REML and optimizer controls/status in `results.diagnostics`. `--maxfun` maps
to the selected optimizer's evaluation/iteration-limit key. Optimizer failure,
singular boundary fits and unavailable checks are distinct. Requested check
rows carry `status` (`available`, `skipped`, `unavailable`); disabled checks
remain absent and their switches are retained in `diagnostics.requested`.
An unavailable check is never a passed assumption.

The performance heteroscedasticity check retains its returned p-value, including
zero. Cluster influence uses the `influence.ME` refits and the
`stats::cooks.distance` generic, retaining cluster distances when available;
unsupported refits or package errors are explicit unavailable results.
DHARMa's retained simulation settings are explicit (default `seed=123`, `n=250`,
`refit=FALSE`); `--seed` overrides the canonical default. The shared boundary
initializes RNG before simulation, removing DHARMa's previous dependence on
whether an RNG state already existed. Simulated results can therefore differ
from old fresh-session runs despite the same seed number. These diagnostics
are screens: conditional random-effect modes are shrinkage estimates, so their
Shapiro tests do not establish population normality. Interpret results in the
research design and model context rather than using automatic pass/fail rules
to select a substantive conclusion.

## Outputs

Procedures preserve deterministic per-run output and extend the root protocol;
semantic reports remain separately authored at freely chosen visible paths.

Outputs in a current project follow the [shared run contract](../run-contract.md):
`.nlss/runs/<run-id>/` holds request/result/output and artifacts; the automatic
`report_canonical.md` stays at the project root. No additional project JSONL log
is produced. `--log` affects optional standalone logging, not this evidence.

- `report_canonical.md`: NLSS format report containing analysis flags, table, and narrative.
- `result.json`: Machine-readable results and options, always retained in the saved run.
- `.nlss/runs/<run-id>/`: mandatory request, result, deterministic Markdown and frozen templates as described above.
- Diagnostics include normality tests, homogeneity tests, sphericity (when applicable), regression diagnostics (VIF, Breusch-Pagan, Durbin-Watson, outliers, influence), mixed-model checks (singularity, convergence, random-effects normality, optional DHARMa/performance/influence.ME), and SEM checks (univariate/multivariate normality, Mahalanobis outliers, collinearity, Heywood cases, convergence).

## NLSS format Templates (YAML)

Templates are stored under `assets/assumptions/` and mapped in `scripts/config.yml`:

- `templates.assumptions.ttest`: `assumptions/ttest-template.md`
- `templates.assumptions.anova`: `assumptions/anova-template.md`
- `templates.assumptions.regression`: `assumptions/regression-template.md`
- `templates.assumptions.mixed_models`: `assumptions/mixed-models-template.md`
- `templates.assumptions.sem`: `assumptions/sem-template.md`

Templates use YAML front matter with `{{token}}` placeholders. Supported sections:

- `table.columns`: ordered column definitions (`key`, optional `label`, optional `drop_if_empty`).
- `note.template`: overrides the note text (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: overrides the narrative text.

### Table Column Keys

Available column keys include:

`analysis_type`, `model`, `assumption`, `test`, `target`, `target_label`, `group`, `group_label`, `n`, `statistic`, `df`, `p`, `value`, `status`, `decision`, `note`.

Use `drop_if_empty: true` to hide columns with all empty values.

### Note Tokens

Available note tokens include:

`note_default`, `alpha`, `homogeneity_tests`, `vif_warn`, `vif_high`, `outlier_z`, `cook_threshold`, `mahalanobis_alpha`, `max_cor`, `max_kappa`.

### Narrative Tokens

Use `narrative.row_template` for per-row lines. Available row tokens include:

`full_sentence`, `analysis_type`, `model`, `assumption`, `test`, `target`, `group`, `statistic`, `df`, `p`, `value`, `status`, `decision`, `n`.

## NLSS format Reporting Guidance

- Report the assumption tests used (e.g., Shapiro-Wilk, Levene, Mauchly, Breusch-Pagan) with statistics and p-values.
- For regression, report VIF values and any influence/outlier flags.
- For mixed models, report singularity/convergence flags, residual diagnostics, and any random-effects normality/outlier findings.
- For SEM, report multivariate normality (Mardia), Mahalanobis outliers, collinearity/Heywood cases, and convergence status.
- Note any violations and consider corrections or robust alternatives when assumptions are not met.
- Cite the run and actual model/cases; distinguish evidence, uncertainty and recommendations. Do not claim that a nonsignificant screen proves an assumption, or count missing checks as passed. Explain design-relevant consequences beyond template sentences and obtain agreement before changing the planned analysis or data.

## Dependencies

- Parquet input requires the R package `arrow`.
- Mixed-model assumptions require the R package `lme4`.
- SEM assumptions require the R package `lavaan`.
- Optional checks use `performance`, `DHARMa`, `influence.ME`, and `MVN` when available.
