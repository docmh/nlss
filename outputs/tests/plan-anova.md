# ANOVA Exhaustive Test Plan

## Scope

- Validate all ANOVA modes (between, within, mixed) in `core-stats/scripts/R/anova.R`.
- Exercise input types, configuration defaults, interactive prompts, and override flags.
- Verify post-hoc logic, effect sizes, sphericity handling, assumptions output, and logging.
- Cover edge cases (missing values, listwise removal, coercions) and negative cases (invalid inputs).

## Data Sources

- `outputs/tests/data/golden_dataset.csv` (primary positive coverage; used to create `golden_dataset/golden_dataset.parquet`).
- `outputs/tests/data/anova_edge_dataset.csv` (targeted edge/negative scenarios: non-numeric dv, covariates, within vars).
- `outputs/tests/data/anova_all_missing.csv` (forces listwise deletion to zero rows).
- Generated during tests (RDS/RData/SAV, CSV with custom separator, headerless CSV).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/anova_workspace`.
- Outputs (per dataset):
  - `outputs/test-runs/<timestamp>/anova_workspace/<dataset-name>/apa_report.md`
  - `outputs/test-runs/<timestamp>/anova_workspace/<dataset-name>/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/anova_test.log`

## Coverage Matrix

Positive coverage:
- Between-subjects one-way ANOVA with assumption checks and default Type II (fallback to Type I if `car` missing).
- Between-subjects factorial with covariate, `--type 3` alias, `--effect-size` alias, `--p-adjust`, `--conf-level`, and `--user-prompt` logging.
- Explicit Type I run and post-hoc alias (`--posthoc no`).
- Within-subjects repeated measures (sphericity auto) and alias (`--sphericity no`).
- Mixed ANOVA (within + between + covariate) with grouped post-hoc checks.
- Mixed ANOVA with 3 within levels to trigger Mauchly sphericity rows.
- Post-hoc suppression (within) and override check (request tukey in within -> forced to pairwise).
- Bootstrap CI runs for between and within designs (eta_sq).
- Input types: parquet, CSV (custom sep + header FALSE), RDS, RData (`--df`), SAV (when `haven` is available).
- Interactive run via prompt file and `--help` output verification.
- Workspace run without input flags (manifest + dataset folder resolution).
- `--log FALSE` (no log entry appended).
- Template override checks for default + posthoc APA templates.

Negative coverage:
- Missing `--subject-id` with within variables.
- Within list length < 2.
- Missing `--dv` for between-subjects.
- Missing both `--between` and `--within`.
- Overlap between `--within` and `--dv`/`--between`/`--covariates`.
- Non-numeric dependent variable.
- Non-numeric within variable.
- Non-numeric covariate.
- Unknown dv/between/within/covariate names.
- `--rdata` without `--df`.
- No complete cases after listwise deletion.

## How To Run

```bash
bash outputs/tests/run_anova_tests.sh
```

Or run the full deliberate suite:

```bash
bash scripts/tests.sh deliberate
```

The script asserts successful runs by reading new entries in `analysis_log.jsonl`, and asserts negative cases by expecting failures or `results.status = invalid_input` entries.
