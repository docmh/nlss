# Linear mixed-model acceptance

`run_mixed_model_tests.R` exercises the public `mixed_models.R` and
`replay_run.R` commands in private projects. Numeric expectations come from
independently fitted `lme4`/`lmerTest` models, `car`, `emmeans`, `performance`,
and direct variance/interval calculations. The runner never sources NLSS
scientific functions or reads the historical mixed-model goldens.

```bash
Rscript tests/phase2/run_mixed_model_tests.R --root /tmp/nlss-mixed-check --keep 0
Rscript tests/phase2/run_mixed_model_tests.R --root /tmp/nlss-mixed-smoke --keep 0 --match smoke
```

The complete 73-group suite is required by `phase2`/`all`. The general smoke runner
selects five named groups already included in the full suite; it is not a
replacement for full acceptance. `--match REGEX` records its selection and
fails on an empty selection. `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and
`NLSS_TESTS_CONFIG` follow the other Phase 2 runners. The runner uses installed
`yaml`, `jsonlite`, `arrow`, `digest`, `haven`, `lme4`, `lmerTest`, `pbkrtest`,
`car`, `emmeans`, and `performance`; it neither downloads data nor installs
packages.

The five smoke groups cover sequential factorial inference, random slopes,
SPSS labels/user missings, Kenward–Roger marginal means, and private frozen-input
replay. They include numeric comparisons and replay, not only successful exits.

## Scientific coverage

- Unequal clusters, nonmonotonic numeric subject IDs, reordered factor levels,
  and distinct missing rows; random intercepts, correlated/uncorrelated slopes,
  nested/crossed intercepts, factorial effects, covariates, and intercept-only
  models through full formulas and the existing builder.
- Fixed coefficients, SE, actual denominator df, test statistics, probabilities,
  requested confidence levels, variance components, random-effect correlations,
  sample size, likelihood, AIC/BIC, Nakagawa marginal/conditional R squared and
  adjusted ICC.
- Explicit Types I/II/III with Satterthwaite/Kenward–Roger inference; sequential
  Type I without denominator-df inference and `car` Wald Type II/III tests;
  ML/REML distinction and incompatible inference requests.
- Supported predictor transformations and actual source-row selection; transformed
  responses are explicitly rejected instead of silently interpreted as a different
  model. Nearby numeric subject IDs and literal `NA` IDs must not collapse into
  other identities or missing values. Numeric
  labelled predictors remain numeric unless explicitly assigned a categorical
  role. SPSS user-missing codes are excluded before estimation, while the
  dictionary and Markdown retain variable labels.
- Descriptive standardized numeric main effects without pretending that factor,
  interaction, or transformed terms were standardized by model refitting.
- Emmeans grids with each declared df method, pairwise/built-in/custom contrasts,
  nonalphabetic named/ordered weights, JSON reference arguments, confidence and
  multiplicity settings, effective correction disclosure and seeded multivariate
  t inference. Nonestimable grid cells and planned comparisons retain explicit
  unavailable states in JSON and their own Markdown section.
- Optional residual diagnostics versus mandatory fit-status auditing; singular
  fits, dropped fixed-effect columns, skipped tests, optimizer-specific evaluation
  budgets and nonzero optimizer return codes, invalid input/option domains,
  and protected failure of models that cannot be estimated.

Ordinary numerical comparisons use a scaled `3e-6` tolerance to accommodate
numerical mixed-model optimization. Probabilities use relative `3e-6` with a
`1e-300` floor: a small nonzero p rounded to zero cannot pass. Source-row and
identity checks are exact. Unavailable estimates must remain unavailable; the
tests do not replace them with zero or quietly accept empty tables.

## Reproducibility and publication

Completed bundles require matching request, template, input, dictionary and
artifact hashes. Replay must reproduce raw statistical results and deterministic
Markdown, including after editing original input, current working data,
configuration, templates, or external contrast JSON. It must preserve the
changed working Parquet. Privacy tests require the mandatory bundle with legacy
logging disabled and reject leaked prompts/external source paths.

Failed analyses must leave earlier report/log bytes intact, release their lock,
record an error without normal output, and refuse successful-run replay.
Per-case inputs, command logs, configurations, published bundles, and a
`results.json` summary (including runtime/test source hashes and installed
package versions) remain under the selected root.

These checks cover the existing Gaussian LMM adapter, not new GLMM or pooled
multiple-imputation inference. They complement the shared publication/import
tests and the legacy mixed-model numerical suite. Deterministic test tables do
not constrain the structure or semantic interpretation of final research reports.
