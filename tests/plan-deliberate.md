# Deliberate Suite Plan

## Scope

- Run all module-specific deliberate scripts under `tests/`.
- Use a shared run root so module workspaces stay grouped per run.
- Defer detailed coverage to the module plans listed below.

## How to Run

Preferred (uses `tests/tests.yml` paths):

```bash
bash cmdscripts/tests.sh deliberate
```

Direct runner:

```bash
bash tests/run_deliberate_tests.sh
```

## Modules and Scripts

- calc: `tests/run_calc_tests.sh`
- research_academia: `tests/run_research_academia_tests.sh`
- descriptive_stats: `tests/run_descriptive_stats_tests.sh`
- data_explorer: `tests/run_data_explorer_tests.sh`
- data_transform: `tests/run_data_transform_tests.sh`
- impute: `tests/run_impute_tests.sh`
- plot: `tests/run_plot_tests.sh`
- crosstabs: `tests/run_crosstabs_tests.sh`
- correlations: `tests/run_correlations_tests.sh`
- scale: `tests/run_scale_tests.sh`
- efa: `tests/run_efa_tests.sh`
- reliability: `tests/run_reliability_tests.sh`
- regression: `tests/run_regression_tests.sh`
- power: `tests/run_power_tests.sh`
- sem: `tests/run_sem_tests.sh`
- mixed_models: `tests/run_mixed_models_tests.sh`
- assumptions: `tests/run_assumptions_tests.sh`
- anova: `tests/run_anova_tests.sh`
- t_test: `tests/run_t_test_tests.sh`
- nonparametric: `tests/run_nonparametric_tests.sh`
- metaskill_runner: `tests/run_metaskill_runner_tests.sh`
- check_integrity: `tests/run_check_integrity_tests.sh`
- reconstruct_reports: `tests/run_reconstruct_reports_tests.sh`

## Module Plans (Details)

- `tests/plan-anova.md`
- `tests/plan-assumptions.md`
- `tests/plan-mixed-models.md`
- `tests/plan-plot.md`
- `tests/plan-crosstabs.md`
- `tests/plan-correlations.md`
- `tests/plan-efa.md`
- `tests/plan-descriptive-stats.md`
- `tests/plan-power.md`
- `tests/plan-regression.md`
- `tests/plan-sem.md`
- `tests/plan-check-integrity.md`
- `tests/plan-reconstruct-reports.md`
- `tests/plan-research-academia.md`

## Outputs

- `outputs/test-runs/<timestamp>/deliberate_test.log`
- Per-module logs and outputs under `outputs/test-runs/<timestamp>/<module>_workspace/`
