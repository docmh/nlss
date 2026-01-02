# Deliberate Suite Plan

## Scope

- Run all module-specific deliberate scripts under `tests/smoke/`.
- Use a shared run root so module workspaces stay grouped per run.
- Defer detailed coverage to the module plans listed below.

## Prereqs

- Python 3 available as `python3` or `python` (or set `PYTHON_BIN`).

## How to Run

Preferred (uses `tests/tests.yml` paths):

```bash
bash cmdscripts/tests.sh deliberate
```

Direct runner:

```bash
bash tests/smoke/run_deliberate_tests.sh
```

## Modules and Scripts

- calc: `tests/smoke/run_calc_tests.sh`
- research_academia: `tests/smoke/run_research_academia_tests.sh`
- descriptive_stats: `tests/smoke/run_descriptive_stats_tests.sh`
- data_explorer: `tests/smoke/run_data_explorer_tests.sh`
- data_transform: `tests/smoke/run_data_transform_tests.sh`
- impute: `tests/smoke/run_impute_tests.sh`
- plot: `tests/smoke/run_plot_tests.sh`
- crosstabs: `tests/smoke/run_crosstabs_tests.sh`
- correlations: `tests/smoke/run_correlations_tests.sh`
- scale: `tests/smoke/run_scale_tests.sh`
- efa: `tests/smoke/run_efa_tests.sh`
- reliability: `tests/smoke/run_reliability_tests.sh`
- regression: `tests/smoke/run_regression_tests.sh`
- power: `tests/smoke/run_power_tests.sh`
- sem: `tests/smoke/run_sem_tests.sh`
- mixed_models: `tests/smoke/run_mixed_models_tests.sh`
- assumptions: `tests/smoke/run_assumptions_tests.sh`
- anova: `tests/smoke/run_anova_tests.sh`
- t_test: `tests/smoke/run_t_test_tests.sh`
- nonparametric: `tests/smoke/run_nonparametric_tests.sh`
- metaskill_runner: `tests/smoke/run_metaskill_runner_tests.sh`
- check_integrity: `tests/smoke/run_check_integrity_tests.sh`
- reconstruct_reports: `tests/smoke/run_reconstruct_reports_tests.sh`

## Module Plans (Details)

- `tests/smoke/plan-anova.md`
- `tests/smoke/plan-assumptions.md`
- `tests/smoke/plan-mixed-models.md`
- `tests/smoke/plan-plot.md`
- `tests/smoke/plan-crosstabs.md`
- `tests/smoke/plan-correlations.md`
- `tests/smoke/plan-efa.md`
- `tests/smoke/plan-descriptive-stats.md`
- `tests/smoke/plan-power.md`
- `tests/smoke/plan-regression.md`
- `tests/smoke/plan-sem.md`
- `tests/smoke/plan-check-integrity.md`
- `tests/smoke/plan-reconstruct-reports.md`
- `tests/smoke/plan-research-academia.md`

## Outputs

- `outputs/test-runs/<timestamp>/deliberate_test.log`
- Per-module logs and outputs under `outputs/test-runs/<timestamp>/<module>_workspace/`
