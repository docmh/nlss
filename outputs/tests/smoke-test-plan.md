# Smoke Test Plan (Golden Dataset)

## Scope

- Run every current module with a clean variable subset and with edge-case inputs.
- Exercise key flags and template branches (grouped, cross, anova/regression).
- Validate outputs in `defaults.output_dir/<dataset-name>/`: `analysis_log.jsonl`, `apa_report.md`, `scratchpad.md`, and the workspace parquet copy.

## Prereqs

- Run from the repo root.
- Rscript available.
- R package `arrow` installed (Parquet support).
- R package `lavaan` installed (SEM/CFA/mediation coverage).

## Workspace setup

- Preferred: `bash scripts/tests.sh smoke` (creates a run root under `outputs/test-runs/<timestamp>` and sets `defaults.output_dir` automatically).
- On Windows, run `powershell -ExecutionPolicy Bypass -File scripts/tests.ps1 smoke` to use Windows R.
- For manual runs, set `RUN_ROOT` and use `WORKSPACE_DIR="${RUN_ROOT}/workspace"` so outputs stay in the run folder.
- Always run `init-workspace` first to create `golden_dataset/golden_dataset.parquet` and the dataset `scratchpad.md`/`apa_report.md`.

## Data subsets (golden_dataset.csv)

- Clean numeric: `outcome_anova,outcome_reg,x1,x2,x3,mediator,f1_1,f1_2,f1_3_rev,f1_4`
- Clean categorical/group: `group3,gender,education,cat_var2,ordinal_var`
- Edge-case missingness: `age,income,satisfaction,pre_score,mid_score,post_score,outcome_reg,cat_var,high_missing_var,all_missing_var,f2_3,f3_1,f3_2,f3_3,f3_4,group2`
- Edge-case distribution/variance: `skewed_var,outlier_var,zero_var,near_zero_var`
- Mixed models long format: reshape `pre_score,mid_score,post_score` into `time` + `score` with `id,group3,x1`.

## Run sequence (simple)

1) `init-workspace` to create the parquet copy.
2) Clean pass for read-only modules.
3) Prepare mixed-models long dataset and run mixed models clean pass.
4) Edge-case pass for read-only modules.
5) Re-init, then run mutating modules (`missings`, `data-transform`) for clean and edge-case inputs.
6) Output generation checks (template overrides).
7) Optional negative/validation checks.

## Example commands (clean pass)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
DATA="outputs/tests/golden_dataset.csv"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

Rscript core-stats/scripts/R/init_workspace.R --csv "${DATA}"

Rscript core-stats/scripts/R/data_explorer.R --parquet "${PARQUET}" --vars id,site,group3,gender,education,cat_var2,ordinal_var
Rscript core-stats/scripts/R/descriptive_stats.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --group group3 --digits 3
Rscript core-stats/scripts/R/frequencies.R --parquet "${PARQUET}" --vars gender,education,cat_var2,ordinal_var --group group3
Rscript core-stats/scripts/R/crosstabs.R --parquet "${PARQUET}" --row gender --col group3 --percent row --chisq TRUE --expected TRUE --residuals TRUE
Rscript core-stats/scripts/R/correlations.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --method pearson --missing pairwise
Rscript core-stats/scripts/R/scale.R --parquet "${PARQUET}" --vars f1_1,f1_2,f1_3_rev,f1_4 --reverse f1_3_rev --reverse-min 1 --reverse-max 5 --score mean --omega TRUE
Rscript core-stats/scripts/R/assumptions.R --parquet "${PARQUET}" --analysis regression --dv outcome_anova --ivs x1,x2,x3,mediator
Rscript core-stats/scripts/R/regression.R --parquet "${PARQUET}" --dv outcome_reg --blocks "x1,x2;x3,mediator" --interactions x1:mediator --center mean --standardize predictors
Rscript core-stats/scripts/R/sem.R --parquet "${PARQUET}" --analysis cfa --factors "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4"
Rscript core-stats/scripts/R/sem.R --parquet "${PARQUET}" --analysis mediation --x x1 --m mediator --y outcome_reg
Rscript outputs/tests/mixed_models_prep.R "${DATA}" "${RUN_ROOT}/tmp/mixed_models_long.csv"
Rscript core-stats/scripts/R/init_workspace.R --csv "${RUN_ROOT}/tmp/mixed_models_long.csv"
Rscript core-stats/scripts/R/mixed_models.R --parquet "${WORKSPACE_DIR}/mixed_models_long/mixed_models_long.parquet" --formula "score ~ time + group3 + x1 + (1|id)"
Rscript core-stats/scripts/R/anova.R --parquet "${PARQUET}" --dv outcome_anova --between group3
Rscript core-stats/scripts/R/t_test.R --parquet "${PARQUET}" --vars x1 --mu 0
Rscript core-stats/scripts/R/t_test.R --parquet "${PARQUET}" --vars outcome_anova --group group2
Rscript core-stats/scripts/R/t_test.R --parquet "${PARQUET}" --x f1_1 --y f1_2
```

## Example commands (edge-case pass)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

Rscript core-stats/scripts/R/init_workspace.R --csv "outputs/tests/golden_dataset.csv"

Rscript core-stats/scripts/R/data_explorer.R --parquet "${PARQUET}" --vars age,income,cat_var,high_missing_var,all_missing_var,zero_var,near_zero_var
Rscript core-stats/scripts/R/descriptive_stats.R --parquet "${PARQUET}" --vars skewed_var,outlier_var,zero_var,near_zero_var,high_missing_var --group group2 --digits 3
Rscript core-stats/scripts/R/frequencies.R --parquet "${PARQUET}" --vars cat_var --group group2
Rscript core-stats/scripts/R/crosstabs.R --parquet "${PARQUET}" --row cat_var --col group2 --fisher TRUE --fisher-simulate TRUE --fisher-b 200
Rscript core-stats/scripts/R/correlations.R --parquet "${PARQUET}" --x skewed_var,outlier_var --y age,income --method spearman --missing complete --p-adjust BH
Rscript core-stats/scripts/R/scale.R --parquet "${PARQUET}" --vars f2_1,f2_2,f2_3,f2_4_rev --reverse f2_4_rev --reverse-min 1 --reverse-max 5 --missing complete --omega FALSE
Rscript core-stats/scripts/R/assumptions.R --parquet "${PARQUET}" --analysis anova --dv outcome_anova --between group3 --within pre_score,mid_score,post_score --subject-id id
Rscript core-stats/scripts/R/regression.R --parquet "${PARQUET}" --dv outcome_reg --ivs x1,x2,x3 --bootstrap TRUE --bootstrap-samples 200 --seed 42
Rscript core-stats/scripts/R/sem.R --parquet "${PARQUET}" --analysis path --dv outcome_reg --ivs skewed_var,outlier_var
Rscript core-stats/scripts/R/mixed_models.R --parquet "${WORKSPACE_DIR}/mixed_models_long/mixed_models_long.parquet" --formula "score ~ time * group3 + x1 + (1|id)" --emmeans "time*group3" --contrasts pairwise --p-adjust holm
Rscript core-stats/scripts/R/anova.R --parquet "${PARQUET}" --within pre_score,mid_score,post_score --between group3 --subject-id id --posthoc pairwise
Rscript core-stats/scripts/R/t_test.R --parquet "${PARQUET}" --vars pre_score --group group2 --var-equal TRUE
Rscript core-stats/scripts/R/t_test.R --parquet "${PARQUET}" --x pre_score --y post_score --bootstrap TRUE --bootstrap-samples 200 --seed 42
```

## Mutating modules (clean and edge-case)

```bash
RUN_ROOT="outputs/test-runs/<timestamp>"
WORKSPACE_DIR="${RUN_ROOT}/workspace"
DATA="outputs/tests/golden_dataset.csv"
PARQUET="${WORKSPACE_DIR}/golden_dataset/golden_dataset.parquet"

# Clean missings
Rscript core-stats/scripts/R/init_workspace.R --csv "${DATA}"
Rscript core-stats/scripts/R/missings.R --parquet "${PARQUET}" --vars outcome_anova,x1,x2,x3,mediator --method listwise

# Edge-case missings (indicator + thresholds)
Rscript core-stats/scripts/R/init_workspace.R --csv "${DATA}"
Rscript core-stats/scripts/R/missings.R --parquet "${PARQUET}" --vars age,income,satisfaction,outcome_reg,high_missing_var,all_missing_var --method indicator --indicator-threshold 0.2 --drop-threshold 0.5

# Clean data-transform
Rscript core-stats/scripts/R/init_workspace.R --csv "${DATA}"
Rscript core-stats/scripts/R/data_transform.R --parquet "${PARQUET}" --calc "score_avg=(f1_1+f1_2+f1_4)/3" --standardize x1

# Edge-case data-transform
Rscript core-stats/scripts/R/init_workspace.R --csv "${DATA}"
Rscript core-stats/scripts/R/data_transform.R --parquet "${PARQUET}" --transform "skewed_var=log" --percentile-bins "outlier_var=4" --recode "group3=A:1,B:2,C:3" --drop zero_var
```

## Output generation checks (templates)

- For each module with templates, run a clean pass to establish baseline output, then run again with a temporary template override and confirm the marker appears in `apa_report.md`.
- Suggested marker: add `TEMPLATE_SMOKE_TEST` to the template body or `note.template`.

Template keys and how to trigger them:
- init-workspace: `templates.init_workspace.default` and `templates.init_workspace.scratchpad` (run `init_workspace.R`).
- descriptive-stats: `templates.descriptive_stats.default` (any run).
- frequencies: `templates.frequencies.default` (no group) and `templates.frequencies.grouped` (use `--group`).
- crosstabs: `templates.crosstabs.default` (no group) and `templates.crosstabs.grouped` (use `--group`).
- correlations: `templates.correlations.default` (`--vars`) and `templates.correlations.cross` (`--x` and `--y`).
- scale: `templates.scale.default` (any run).
- data-explorer: `templates.data_explorer.default` (any run).
- data-transform: `templates.data_transform.default` (any run, optional in runner).
- missings: `templates.missings.default` (any run, optional in runner).
- assumptions: `templates.assumptions.ttest` (`--analysis ttest`), `templates.assumptions.anova` (`--analysis anova`), `templates.assumptions.regression` (`--analysis regression`).
- regression: `templates.regression.default` (any run).
- sem: `templates.sem.default` (`--analysis sem` or `--analysis path`), `templates.sem.cfa` (`--analysis cfa`), `templates.sem.mediation` (`--analysis mediation`), `templates.sem.invariance` (`--analysis invariance`).
- mixed-models: `templates.mixed_models.default` (any run) and `templates.mixed_models.emmeans` (`--emmeans`, when `emmeans` is installed).
- anova: `templates.anova.default` (omnibus table) and `templates.anova.posthoc` (post-hoc table).
- t-test: `templates.t_test.default` (any run).

Template override steps (per module):
- Copy the template to `RUN_ROOT/templates/<module>-template.md` and insert `TEMPLATE_SMOKE_TEST`.
- Update `core-stats/scripts/config.yml` for the template key to point at the copy.
- Re-run the module and check `apa_report.md` for the marker.
- Restore the original template path and delete the temp file.

## Flag coverage hints

- descriptive-stats: `--group`, `--digits`
- frequencies: `--group`, `--include-numeric`
- crosstabs: `--percent`, `--apa-percent`, `--chisq`, `--fisher`, `--fisher-simulate`
- correlations: `--method`, `--missing`, `--p-adjust`, `--controls`, `--x/--y`
- scale: `--reverse`, `--reverse-min`, `--reverse-max`, `--score`, `--omega`, `--missing`
- data-explorer: `--max-levels`, `--top-n`
- data-transform: `--calc`, `--transform`, `--percentile-bins`, `--recode`, `--drop`
- missings: `--method`, `--indicator-threshold`, `--drop-threshold`, `--max-patterns`
- assumptions: `--analysis`, `--between`, `--within`, `--blocks`, `--vif`, `--outliers`
- regression: `--blocks`, `--interactions`, `--center`, `--bootstrap`, `--bootstrap-samples`
- sem: `--analysis`, `--model`, `--factors`, `--x`, `--m`, `--y`, `--estimator`, `--missing`, `--se`, `--ci`, `--std`, `--group`, `--fit`
- mixed-models: `--formula`, `--random`, `--df-method`, `--emmeans`, `--contrasts`, `--p-adjust`
- anova: `--between`, `--within`, `--subject-id`, `--type`, `--posthoc`, `--p-adjust`
- t-test: `--mu`, `--var-equal`, `--bootstrap`, `--bootstrap-samples`
- init-workspace: `--csv`, `--parquet`, `--agent`, `--sep`, `--header`

## Optional negative checks (expected failures)

- t-test with >2 group levels: `--vars outcome_anova --group group3`
- t-test invalid paired + group combo: `--x pre_score --y post_score --group group3`
- anova missing subject-id with within variables: `--within pre_score,post_score`
- regression missing dv: `--ivs x1,x2`
- mixed-models missing random effects: `--dv score --fixed time,group3`
- sem missing vars: `--analysis cfa --factors "F1=missing1,missing2"`

For expected failures, informational output counts as a pass (e.g., `t_test` supports `--expect-two-groups TRUE` and prints `EXPECTED_NEGATIVE: t_test group levels`).
The paired + group negative check relies on `analysis_log.jsonl` with `results.status = invalid_input`.
For automated checks, prefer the JSONL log entry (`results.status = expected_invalid_input`) when informational output is not captured.

## Smoke-test checks

- `analysis_log.jsonl` gets appended for each module in `defaults.output_dir/golden_dataset/`.
- `scratchpad.md` and `apa_report.md` exist in `defaults.output_dir/golden_dataset/`.
- `golden_dataset/golden_dataset.parquet` exists in the workspace and updates after `missings` and `data-transform`.
- Template override runs show `TEMPLATE_SMOKE_TEST` in `apa_report.md` for the target module.
