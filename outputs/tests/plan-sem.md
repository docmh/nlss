# SEM Deliberate Test Plan

## Scope

- Cover all SEM module analysis types: sem, cfa, path, mediation, invariance.
- Exercise input modes (csv/rds/rdata/parquet, sav when available), interactive prompts, and help output.
- Exercise model builders (factors, dv/ivs, mediation), inline model syntax, model-file input, and custom defined parameters (`:=`).
- Validate robust estimators, ordered indicators, grouping/group-equal, bootstrapping, modindices, residual logging, and fit/SE/CI/std options.
- Confirm optional logging behaviors (`--log`, `--user-prompt`) and `--paths` alias handling.
- Include clean, edge, and negative cases with expected errors logged.

## Prereqs

- Run from the repo root.
- Rscript available.
- R packages installed: `arrow`, `yaml`, `jsonlite`, `lavaan`.
- Optional: `haven` for `.sav` input coverage (skipped if missing).
- Interactive coverage uses `CORE_STATS_PROMPT_FILE` to feed prompts during automation.

## Run sequence (deliberate)

1) Initialize workspace on `outputs/tests/data/golden_dataset.csv`.
2) Input mode checks (csv/rds/rdata, sav if available) plus interactive/help smoke checks.
3) Clean model runs for CFA, path, mediation, SEM, and invariance (including builder + inline model + `--paths`).
4) Estimator and option coverage (MLM/MLMV/MLMVS/ULSMV/DWLS/GLS/ULS, missing=pairwise, std none/std.lv, fit=aic/bic, r2 off).
5) Edge runs with ordered indicators, grouping/group-equal, bootstrapping (bca), and modindices/residuals.
6) Template override checks for SEM default/mediation/invariance.
7) Negative tests: missing model, invalid factor spec, invalid mediation, missing group for invariance, unknown variables, missing model file.

## Clean cases

- CFA with factor builder (MLR, FIML), plus inline std.lv.
- Path model with dv/ivs builder (including `--std none` and custom fit list).
- Mediation models (single mediator, serial, and parallel).
- SEM model from `--model-file`, plus inline `--model` and `--paths` alias runs.
- Invariance (configural + metric + scalar + strict) across `group2`.

## Edge cases

- CFA with ordered indicators and WLSMV.
- Grouped CFA (`--group group2`) and group-equal constraints.
- Bootstrap with `--ci bca` and custom `--conf-level`.
- Modindices + residuals logging.
- Missing handling `pairwise`, `--r2 FALSE`, `--log FALSE`, and `--user-prompt` logging.

## Negative cases

- Missing model (no `--model`/builder inputs).
- Invalid factor spec (missing `=` or items).
- Serial mediation with insufficient mediators.
- Invariance without `--group`.
- Unknown ordered variable.
- Unknown variable in model.
- Missing model file path.

## Outputs

- `analysis_log.jsonl` under the sem workspace with expected statuses (`ok` and `invalid_input`).
- `apa_report.md` with appended SEM sections and template override markers.

## Script

Run via:

```bash
bash outputs/tests/run_sem_tests.sh
```

Or include it in deliberate/all via:

```bash
bash scripts/tests.sh deliberate
bash scripts/tests.sh all
```
