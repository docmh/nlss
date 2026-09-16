# Mixed-model module regression plan

Run `bash tests/smoke/run_mixed_models_tests.sh --root <private-dir> --keep 0`.
The harness uses the canonical test registry/dataset and a private configuration
copy for template overrides; it never edits the installed canonical configuration.
`NLSS_TEST_ROOT` and `NLSS_KEEP_RUNS` remain accepted. The outer suite harness owns
retention when this module runs as part of deliberate/all.

## Coverage

- Formula and dv/fixed/random entrypoints; random-group shorthand, ML/REML,
  Satterthwaite/Kenward–Roger, omnibus types and descriptive standardized beta.
- Independently computed fixed effects, random intercept variance, fit statistics,
  model-based R²/ICC, omnibus effects and Shapiro–Wilk values.
- Marginal means, all 36 pairwise contrasts in a nine-cell family, custom weight
  vectors and a built-in contrast method. Compare every family p-value so a
  recycled first p-value cannot pass the golden suite.
- CSV (including separator override), RDS, selected RData, SAV and working Parquet;
  direct workspace and template overrides; logging disabled.
- Mandatory singularity/convergence diagnostics with optional residual checks
  disabled, and explicit unavailable Shapiro diagnostics outside its valid range.
- Invalid formulas/roles, absent random effects, missing columns, incomplete data
  and invalid contrast requests: nonzero exit, one failed run, no normal output,
  and unchanged canonical report/log/project-manifest bytes.

## Numeric oracle

`tests/values/mixed_models_compute_golden.R` calls installed `lme4`, `lmerTest`,
`performance`, `emmeans` and base R directly. It does not source NLSS analysis
functions. Fixed and omnibus finite-df inference specifies its type and df method;
marginal means explicitly use the requested Satterthwaite method. Numeric tolerances
are relative 1e-6/absolute 1e-6, with zero absolute slack for p-values. Expected
unavailable fields are checked, not silently ignored.

The independent public-CLI acceptance runner in
[Phase 2 mixed-model tests](../phase2/mixed-model-README.md) additionally exercises
unbalanced, nested/crossed/random-slope designs, method combinations, numeric-ID
collisions, SPSS labels/missings, optimizer failures, immutable inputs/templates/
contrast definitions, seeded replay and protected invalid requests. Its scientific
oracles also call established packages without sourcing NLSS internals.

## Interpretation limits

These checks validate the implementation, not the suitability of a mixed model
for a research question. Singularity is not equivalent to failed optimizer
convergence; a residual normality test is not a random-effect normality or
independence test. Native Windows verification remains separate from Linux tests.
