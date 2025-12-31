# Smoke Test Plan (Golden Dataset)

## Scope

- Run every current module with a clean variable subset and with edge-case inputs.
- Exercise the calc utility with basic, vector, unsafe, and failure cases.
- Exercise the research-academia utility with multiple network searches (when allowed).
- Exercise key flags and template branches (grouped, cross, anova/regression).
- Verify logging toggles (enabled/fields) change `analysis_log.jsonl` content as expected, including invalid toggle values.
- Verify check-integrity on `analysis_log.jsonl`, including tampered copies (edit/delete a line) to confirm warning behavior.
- Verify reconstruct-reports rebuilds `report_canonical_reconstructed.md` from `analysis_log.jsonl`.
- Verify label-aware modules render variable/value labels using a labeled RDS dataset derived from the golden dataset and mixed-models long data.
- Validate outputs in `tests.output_dir/<timestamp>/workspace/<dataset-name>/`: `analysis_log.jsonl`, `report_canonical.md`, `scratchpad.md`, and the workspace parquet copy.

## Prereqs

- Run from the repo root.
- Python 3 available as `python3` or `python` (or set `PYTHON_BIN`).
- Rscript available.
- R package `arrow` installed (Parquet support).
- R package `lavaan` installed (SEM/CFA/mediation coverage).
- R package `pwr` installed (power analysis coverage).
- R package `semPower` installed (SEM power coverage).
- R package `psych` installed (EFA/PCA coverage).

## Workspace Setup

- Preferred: `bash cmdscripts/tests.sh smoke` (creates a run root under `tests.output_dir/<timestamp>` and uses a workspace manifest to route outputs without editing `defaults.output_dir`).
- On Windows, run `powershell -ExecutionPolicy Bypass -File cmdscripts/tests.ps1 smoke` to use Windows R.
- For manual runs, set `RUN_ROOT` and use `WORKSPACE_DIR="${RUN_ROOT}/workspace"` so outputs stay in the run folder.
- Always run `init-workspace` first to create `golden_dataset/golden_dataset.parquet` and the dataset `scratchpad.md`/`report_canonical.md`.

## Data Subsets (Data/Golden_Dataset.Csv)

- Clean numeric: `outcome_anova,outcome_reg,x1,x2,x3,mediator,f1_1,f1_2,f1_3_rev,f1_4`
- Clean categorical/group: `group3,gender,education,cat_var2,ordinal_var`
- Edge-case missingness: `age,income,satisfaction,pre_score,mid_score,post_score,outcome_reg,cat_var,high_missing_var,all_missing_var,f2_3,f3_1,f3_2,f3_3,f3_4,group2`
- Edge-case distribution/variance: `skewed_var,outlier_var,zero_var,near_zero_var`
- Mixed models long format: reshape `pre_score,mid_score,post_score` into `time` + `score` with `id,group3,x1`.
- Labeled dataset: generate `golden_dataset_labeled.rds` (and `mixed_models_labeled.rds`) via `tests/prepare_labeled_dataset.R` to validate label rendering.
- Metadata retention: generate `golden_dataset_meta` in `.rds`, `.RData`, and `.sav`, run `init_workspace` for each, and verify parquet metadata with `tests/check_parquet_labels.R`.

## Run Sequence (Simple)

1) `init-workspace` to create the parquet copy.
2) Calc utility smoke (basic, vector, unsafe, negative).
3) Research-academia utility optional network run.
4) Clean pass for read-only modules.
5) Generate labeled RDS fixtures and run label-aware modules against them.
6) Prepare mixed-models long dataset and run mixed models clean pass.
7) Edge-case pass for read-only modules.
8) Re-init, then run mutating modules (`missings`, `impute`, `data-transform`) for clean and edge-case inputs.
9) Output generation checks (template overrides).
10) Optional negative/validation checks.
11) Integrity checks on `analysis_log.jsonl` plus tampered copies (edit/delete line) to confirm warning behavior.

## Example Commands (Clean Pass)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
DATA="tests/data/golden_dataset.csv"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

Rscript scripts/R/init_workspace.R --csv "${DATA}"

Rscript scripts/R/metaskill_runner.R --parquet "${PARQUET}" --meta sample-description --intent "describe the sample"

Rscript scripts/R/data_explorer.R --parquet "${PARQUET}" --vars id,site,group3,gender,education,cat_var2,ordinal_var
Rscript scripts/R/descriptive_stats.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --group group3 --digits 3
Rscript scripts/R/plot.R --parquet "${PARQUET}" --type histogram --vars age --bins 12
Rscript scripts/R/frequencies.R --parquet "${PARQUET}" --vars gender,education,cat_var2,ordinal_var --group group3
Rscript scripts/R/crosstabs.R --parquet "${PARQUET}" --row gender --col group3 --percent row --chisq TRUE --expected TRUE --residuals TRUE
Rscript scripts/R/correlations.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --method pearson --missing pairwise
Rscript scripts/R/scale.R --parquet "${PARQUET}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean --omega TRUE
Rscript scripts/R/efa.R --parquet "${PARQUET}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev --method pca --rotation varimax
Rscript scripts/R/reliability.R --parquet "${PARQUET}" --analysis icc --vars pre_score,mid_score,post_score --icc-model twoway-random --icc-type agreement --icc-unit single
Rscript scripts/R/assumptions.R --parquet "${PARQUET}" --analysis regression --dv outcome_anova --ivs x1,x2,x3,mediator
Rscript scripts/R/regression.R --parquet "${PARQUET}" --dv outcome_reg --blocks "x1,x2;x3,mediator" --interactions x1:mediator --center mean --standardize predictors
Rscript scripts/R/power.R --parquet "${PARQUET}" --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
Rscript scripts/R/sem.R --parquet "${PARQUET}" --analysis cfa --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
Rscript scripts/R/sem.R --parquet "${PARQUET}" --analysis mediation --x x1 --m mediator --y outcome_reg
Rscript scripts/R/assumptions.R --parquet "${PARQUET}" --analysis sem --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
Rscript tests/mixed_models_prep.R "${DATA}" "${RUN_ROOT}/tmp/mixed_models_long.csv"
Rscript scripts/R/init_workspace.R --csv "${RUN_ROOT}/tmp/mixed_models_long.csv"
Rscript scripts/R/mixed_models.R --parquet "${WORKSPACE_DIR}/mixed_models_long/mixed_models_long.parquet" --formula "score ~ time + group3 + x1 + (1|id)"
Rscript scripts/R/assumptions.R --parquet "${WORKSPACE_DIR}/mixed_models_long/mixed_models_long.parquet" --analysis mixed_models --formula "score ~ time + group3 + x1 + (1|id)"
Rscript scripts/R/anova.R --parquet "${PARQUET}" --dv outcome_anova --between group3
Rscript scripts/R/t_test.R --parquet "${PARQUET}" --vars x1 --mu 0
Rscript scripts/R/t_test.R --parquet "${PARQUET}" --vars outcome_anova --group group2
Rscript scripts/R/t_test.R --parquet "${PARQUET}" --x f1_1 --y f1_2
Rscript scripts/R/nonparametric.R --parquet "${PARQUET}" --vars outcome_anova --group group2 --test mann_whitney
Rscript scripts/R/nonparametric.R --parquet "${PARQUET}" --within pre_score,mid_score,post_score --subject-id id --test friedman
```

## Calc Utility Examples

```bash
Rscript scripts/R/calc.R --expr "alpha_adj=0.05/3" --digits 4
Rscript scripts/R/calc.R --set "r=0.3" --expr "d=2*r/sqrt(1-r^2)" --digits 3 --format json
Rscript scripts/R/calc.R --expr "sqrt(0.2)|log(10)" --digits 3  # Quote to keep '|' literal in the shell
Rscript scripts/R/calc.R --expr "pnorm(c(-1,0,1))" --unsafe TRUE --digits 3
```

## Research-Academia Utility Examples

```bash
# Network run (only if allowed)
Rscript scripts/R/research_academia.R --query "effect size" --sources openalex,crossref --top-n 5
Rscript scripts/R/research_academia.R --query "power analysis" --sources openalex --top-n 2
Rscript scripts/R/research_academia.R --query "stress experience" --sources openalex --top-n 2
```

## Example Commands (Edge-Case Pass)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

Rscript scripts/R/init_workspace.R --csv "tests/data/golden_dataset.csv"

Rscript scripts/R/data_explorer.R --parquet "${PARQUET}" --vars age,income,cat_var,high_missing_var,all_missing_var,zero_var,near_zero_var
Rscript scripts/R/descriptive_stats.R --parquet "${PARQUET}" --vars skewed_var,outlier_var,zero_var,near_zero_var,high_missing_var --group group2 --digits 3 --trim 0.2 --iqr-multiplier 2 --outlier-z 2.5
Rscript scripts/R/plot.R --parquet "${PARQUET}" --type density --vars skewed_var --group group2 --bw 0.5
Rscript scripts/R/frequencies.R --parquet "${PARQUET}" --vars cat_var --group group2
Rscript scripts/R/crosstabs.R --parquet "${PARQUET}" --row cat_var --col group2 --fisher TRUE --fisher-simulate TRUE --fisher-b 200
Rscript scripts/R/correlations.R --parquet "${PARQUET}" --x skewed_var,outlier_var --y age,income --method spearman --missing complete --p-adjust BH
Rscript scripts/R/scale.R --parquet "${PARQUET}" --vars f2_1,f2_2,f2_3,f2_4_rev --reverse f2_4_rev --reverse-min 1 --reverse-max 5 --missing complete --omega FALSE
Rscript scripts/R/efa.R --parquet "${PARQUET}" --vars f1_1,f1_2,f1_3_rev,f1_4,f2_1,f2_2,f2_3,f2_4_rev --method minres --n-factors 2 --missing pairwise --cor spearman --loading-cutoff 0.4 --sort-loadings FALSE
Rscript scripts/R/reliability.R --parquet "${PARQUET}" --analysis kappa --vars cat_var,cat_var2 --kappa-weight none
Rscript scripts/R/assumptions.R --parquet "${PARQUET}" --analysis anova --dv outcome_anova --between group3 --within pre_score,mid_score,post_score --subject-id id
Rscript scripts/R/regression.R --parquet "${PARQUET}" --dv outcome_reg --ivs x1,x2,x3 --bootstrap TRUE --bootstrap-samples 200 --seed 42
Rscript scripts/R/power.R --parquet "${PARQUET}" --analysis correlation --mode sensitivity --n 120 --power 0.8
Rscript scripts/R/sem.R --parquet "${PARQUET}" --analysis path --dv outcome_reg --ivs skewed_var,outlier_var
Rscript scripts/R/mixed_models.R --parquet "${WORKSPACE_DIR}/mixed_models_long/mixed_models_long.parquet" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise --p-adjust holm
Rscript scripts/R/anova.R --parquet "${PARQUET}" --within pre_score,mid_score,post_score --between group3 --subject-id id --posthoc pairwise
Rscript scripts/R/t_test.R --parquet "${PARQUET}" --vars pre_score --group group2 --var-equal TRUE
Rscript scripts/R/t_test.R --parquet "${PARQUET}" --x pre_score --y post_score --bootstrap TRUE --bootstrap-samples 200 --seed 42
Rscript scripts/R/nonparametric.R --parquet "${PARQUET}" --vars skewed_var --mu 0 --test wilcoxon
Rscript scripts/R/nonparametric.R --parquet "${PARQUET}" --vars outcome_anova --group group3 --test kruskal --posthoc pairwise
```

## Mutating Modules (Clean and Edge-Case)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
DATA="tests/data/golden_dataset.csv"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

# Clean missings
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/missings.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --method listwise

# Edge-case missings (indicator + thresholds)
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/missings.R --parquet "${PARQUET}" --vars age,income,satisfaction,outcome_reg,high_missing_var,all_missing_var --method indicator --indicator-threshold 0.2 --drop-threshold 0.5

# Clean impute
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/impute.R --parquet "${PARQUET}" --vars age,income,cat_var --engine simple --numeric-method median --categorical-method mode

# Edge-case impute
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/impute.R --parquet "${PARQUET}" --vars age,cat_var,all_missing_var --engine simple --numeric-method mean --categorical-method mode --indicator TRUE --indicator-suffix _miss

# Clean data-transform
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/data_transform.R --parquet "${PARQUET}" --calc "score_avg=(f1_1+f1_2+f1_4)/3" --standardize x1

# Edge-case data-transform
Rscript scripts/R/init_workspace.R --csv "${DATA}"
Rscript scripts/R/data_transform.R --parquet "${PARQUET}" --transform "skewed_var=log" --percentile-bins "outlier_var=4" --recode "group3=A:1,B:2,C:3" --drop zero_var
```

## Output Generation Checks (Templates)

- For each module with templates, run a clean pass to establish baseline output, then run again with a temporary template override and confirm the marker appears in `report_canonical.md`.
- Suggested marker: add `TEMPLATE_SMOKE_TEST` to the template body or `note.template`.

Template keys and how to trigger them:

- init-workspace: `templates.init_workspace.default` and `templates.init_workspace.scratchpad` (run `init_workspace.R`).
- metaskill-runner: `templates.metaskill_runner.default` (any run).
- descriptive-stats: `templates.descriptive_stats.default` (default run), `templates.descriptive_stats.robust` (`--template robust`), `templates.descriptive_stats.distribution` (`--template distribution`).
- frequencies: `templates.frequencies.default` (no group) and `templates.frequencies.grouped` (use `--group`).
- crosstabs: `templates.crosstabs.default` (no group) and `templates.crosstabs.grouped` (use `--group`).
- correlations: `templates.correlations.default` (`--vars`), `templates.correlations.cross` (`--x` and `--y`), `templates.correlations.matrix` (`--template matrix`), and `templates.correlations.comparison` (`--group` + `--compare-groups TRUE`).
- scale: `templates.scale.default` (any run).
- efa: `templates.efa.default` (any run).
- reliability: `templates.reliability.default` (any run).
- data-explorer: `templates.data_explorer.default` (any run).
- plot: `templates.plot.default` (any run).
- data-transform: `templates.data_transform.default` (any run, optional in runner).
- missings: `templates.missings.default` (any run, optional in runner).
- impute: `templates.impute.default` (any run, optional in runner).
- assumptions: `templates.assumptions.ttest` (`--analysis ttest`), `templates.assumptions.anova` (`--analysis anova`), `templates.assumptions.regression` (`--analysis regression`), `templates.assumptions.mixed_models` (`--analysis mixed_models`), `templates.assumptions.sem` (`--analysis sem`).
- regression: `templates.regression.default` (any run).
- power: `templates.power.default` (any run).
- sem: `templates.sem.default` (`--analysis sem` or `--analysis path`), `templates.sem.cfa` (`--analysis cfa`), `templates.sem.mediation` (`--analysis mediation`), `templates.sem.invariance` (`--analysis invariance`).
- mixed-models: `templates.mixed_models.default` (any run) and `templates.mixed_models.emmeans` (`--emmeans`, when `emmeans` is installed).
- anova: `templates.anova.default` (omnibus table) and `templates.anova.posthoc` (post-hoc table).
- t-test: `templates.t_test.default` (any run).
- nonparametric: `templates.nonparametric.default` (any run) and `templates.nonparametric.posthoc` (`--posthoc pairwise`).

Template override steps (per module):

- Copy the template to `RUN_ROOT/templates/<module>-template.md` and insert `TEMPLATE_SMOKE_TEST`.
- Update `scripts/config.yml` for the template key to point at the copy.
- Re-run the module and check `report_canonical.md` for the marker.
- Restore the original template path and delete the temp file.
- For `--template` reference coverage, the smoke runner adds a temporary key (for example `templates.descriptive_stats.temporal`) and runs with `--template temporal`. For manual runs, add the key yourself before executing.

## Flag Coverage Hints

- descriptive-stats: `--group`, `--digits`, `--trim`, `--iqr-multiplier`, `--outlier-z`
- frequencies: `--group`, `--include-numeric`
- crosstabs: `--percent`, `--nlss-percent`, `--chisq`, `--fisher`, `--fisher-simulate`; verify phi/Cramer's V columns appear in NLSS format output
- correlations: `--method`, `--missing`, `--p-adjust`, `--controls`, `--x/--y`, `--bootstrap`, `--bootstrap-samples`, `--r0`, `--compare-groups`
- scale: `--reverse`, `--reverse-min`, `--reverse-max`, `--score`, `--omega`, `--missing`
- efa: `--method`, `--rotation`, `--n-factors`, `--eigen-threshold`, `--cor`, `--missing`, `--loading-cutoff`, `--sort-loadings`
- reliability: `--analysis`, `--format`, `--icc-model`, `--icc-type`, `--icc-unit`, `--kappa-weight`, `--method`
- data-explorer: `--max-levels`, `--top-n`
- plot: `--type`, `--vars`, `--x`, `--y`, `--group`, `--stat`, `--percent-base`, `--bins`, `--bw`, `--smooth`, `--summary`, `--na-action`, `--format`, `--file-prefix`, `--file-suffix`, `--figure-number`
- data-transform: `--calc`, `--transform`, `--percentile-bins`, `--recode`, `--drop`
- missings: `--method`, `--indicator-threshold`, `--drop-threshold`, `--max-patterns`
- impute: `--engine`, `--numeric-method`, `--categorical-method`, `--method-map`, `--indicator`, `--m`, `--k`
- assumptions: `--analysis`, `--between`, `--within`, `--blocks`, `--formula`, `--vif`, `--outliers`, `--random-effects`, `--mardia`
- regression: `--blocks`, `--interactions`, `--center`, `--bootstrap`, `--bootstrap-samples`
- power: `--analysis`, `--mode`, `--effect-metric`, `--effect-size`, `--n`, `--n-per-group`, `--estimate-effect`, `--df`, `--rmsea0`, `--rmsea1`
- sem: `--analysis`, `--model`, `--factors`, `--x`, `--m`, `--y`, `--estimator`, `--missing`, `--se`, `--ci`, `--std`, `--group`, `--fit`
- mixed-models: `--formula`, `--random`, `--df-method`, `--emmeans`, `--contrasts`, `--p-adjust`
- anova: `--between`, `--within`, `--subject-id`, `--type`, `--posthoc`, `--p-adjust`
- t-test: `--mu`, `--var-equal`, `--bootstrap`, `--bootstrap-samples`
- init-workspace: `--csv`, `--parquet`, `--agent`, `--sep`, `--header`
- nonparametric: `--test`, `--posthoc`, `--p-adjust`, `--exact`, `--continuity`
- metaskill-runner: `--meta`, `--intent`, `--notes`, `--label`

## Optional Negative Checks (Expected Failures)

- t-test with >2 group levels: `--vars outcome_anova --group group3`
- t-test invalid paired + group combo: `--x pre_score --y post_score --group group3`
- anova missing subject-id with within variables: `--within pre_score,post_score`
- regression missing dv: `--ivs x1,x2`
- mixed-models missing random effects: `--dv score --fixed time,group3`
- sem missing vars: `--analysis cfa --factors "F1=missing1,missing2"`
- nonparametric Mann-Whitney with >2 group levels: `--vars outcome_anova --group group3 --test mann_whitney`

For expected failures, informational output counts as a pass (e.g., `t_test` supports `--expect-two-groups TRUE` and prints `EXPECTED_NEGATIVE: t_test group levels`).
The paired + group negative check relies on `analysis_log.jsonl` with `results.status = invalid_input`.
For automated checks, prefer the JSONL log entry (`results.status = expected_invalid_input`) when informational output is not captured.

## Smoke-Test Checks

- `analysis_log.jsonl` gets appended for each module in `tests.output_dir/<timestamp>/workspace/golden_dataset/`.
- `scratchpad.md` and `report_canonical.md` exist in `tests.output_dir/<timestamp>/workspace/golden_dataset/`.
- `golden_dataset/golden_dataset.parquet` exists in the workspace and updates after `missings`, `impute`, and `data-transform`.
- Template override runs show `TEMPLATE_SMOKE_TEST` in `report_canonical.md` for the target module.
- Template reference runs (`--template <ref>`) show the marker in `report_canonical.md`.
