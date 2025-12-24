# ANOVA Exhaustive Test Plan

## Scope

- Validate all ANOVA modes (between, within, mixed) in `core-stats/scripts/R/anova.R`.
- Exercise input types, configuration defaults, and override flags.
- Verify post-hoc logic, effect sizes, sphericity handling, and logging.
- Cover edge cases (missing values, listwise removal, coercions) and negative cases (invalid inputs).

## Data Sources

- `outputs/tests/golden_dataset.csv` (primary positive coverage; used to create `golden_dataset/golden_dataset.parquet`).
- `outputs/tests/anova_edge_dataset.csv` (targeted edge/negative scenarios: non-numeric dv, covariates, within vars).
- `outputs/tests/anova_all_missing.csv` (forces listwise deletion to zero rows).

## Workspace and Outputs

- Test runner writes to `outputs/test-runs/<timestamp>/anova_workspace`.
- Outputs (per dataset):
  - `outputs/test-runs/<timestamp>/anova_workspace/<dataset-name>/apa_report.md`
  - `outputs/test-runs/<timestamp>/anova_workspace/<dataset-name>/analysis_log.jsonl`
  - `outputs/test-runs/<timestamp>/anova_exhaustive.log`

## Coverage Matrix

Positive coverage:
- Between-subjects one-way ANOVA (post-hoc Tukey).
- Between-subjects factorial with covariate, Type III, eta_sq.
- Within-subjects repeated measures (sphericity auto).
- Mixed ANOVA (within + between + covariate).
- Post-hoc none (no post-hoc section appended).
- Bootstrap CI run for effect sizes.
- CSV input path and workspace parquet copy creation.
- sphericity none (no Mauchly test rows).
- post-hoc override check (request tukey in within -> forced to pairwise).
- `--log FALSE` (no log entry appended).

Negative coverage:
- Missing `--subject-id` with within variables.
- Within list length < 2.
- Missing `--dv` for between-subjects.
- Missing both `--between` and `--within`.
- Overlap between `--within` and `--dv`/`--between`/`--covariates`.
- Non-numeric dependent variable.
- Non-numeric within variable.
- Non-numeric covariate.
- Unknown variable names.
- No complete cases after listwise deletion.

## How To Run

```bash
bash outputs/tests/run_anova_exhaustive.sh
```

Or run the full deliberate suite:

```bash
bash scripts/tests.sh deliberate
```

The script asserts successful runs by reading new entries in `analysis_log.jsonl`, and asserts negative cases by expecting failures or `results.status = invalid_input` entries.
