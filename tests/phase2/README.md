# Phase 2 execution-contract acceptance tests

This offline suite checks the migrated descriptive-statistics and regression
CLI adapters, their immutable analysis inputs, and the public saved-request
replay entrypoint. It does not replace the existing numerical golden suites or
claim migration of every NLSS statistical module.

The normal `phase2` harness additionally runs the
[categorical/data-explorer acceptance suite](categorical-README.md) and the
[scale/reliability acceptance suite](psychometric-README.md), plus the
[t-test/correlation acceptance suite](inference-README.md) and
[ANOVA/rank-test acceptance suite](design-README.md),
[linear mixed-model acceptance](mixed-model-README.md) and the
[bounded legacy mixed-diagnostic regression suite](mixed-diagnostic-README.md),
[EFA](efa-README.md), [SEM](sem-README.md) and
[bounded legacy SEM diagnostics](sem-diagnostic-README.md), [Power](power-README.md),
[parameter-only planning](planning-README.md), [all-family diagnostics](assumptions-README.md)
and [Plot](plot-README.md), plus [data transformations](transform-README.md)
and [missing-data handling](missings-README.md) and [imputation](impute-README.md),
plus [Calc](calc-README.md), [history/publication](history-README.md),
[workspace/report lifecycle](lifecycle-README.md) and
[literature retrieval](research-README.md). Utility audit is explicitly not
statistical replay. The direct R command
below runs this original core suite only.

Run from the repository root:

```sh
bash cmdscripts/tests.sh phase2 --root /tmp/nlss-phase2-tests --keep 0
```

The underlying portable R runner can also be called directly:

```sh
Rscript tests/phase2/run_phase2_tests.R --root /tmp/nlss-phase2-tests --keep 0
```

`--mi-only` selects the MI core contract, seeded artifact preparation and the
end-to-end MI model/rejection cases (seven groups). This is the module-specific
route for the ordinary test harness; it avoids rerunning unrelated Phase 2
execution-contract scenarios. Its artifacts are under `<root>/phase2-mi/`.

`--match REGEX` selects named groups for focused shared-boundary regression
checks and records that selection in `results.json`; zero selected groups fail.
For MI, retain its preparation groups (or use `--mi-only`) because those cases
intentionally share their seeded fixture.

```sh
bash cmdscripts/tests.sh --module mi_regression --root /tmp/nlss-mi-tests --keep 0
```

The runner reads `tests/tests.yml` (or `NLSS_TESTS_CONFIG`) for the golden
dataset and output settings. `NLSS_TEST_ROOT` and `NLSS_KEEP_RUNS` are supported.
Every scenario uses its own project and a private copy of the canonical YAML
configuration; it never edits the shipped configuration. Explicit output roots
are never pruned. Results and subprocess diagnostic logs are retained under
`<root>/phase2/run-<timestamp>-<pid>/`, including `results.json` with per-case
status and the package versions used for testing. No downloads are required.

## Acceptance coverage

- Existing descriptive/regression CLI calls still produce the legacy reports
  and, additionally, a complete per-run `request.json`, `result.json` and
  `output.md` bundle.
- Requests retain resolved scientific options, the effective configuration,
  R/package versions, RNG configuration/state, code identity, templates, and
  exact data/dictionary SHA-256 references. Stored references are checked
  against the actual preserved bytes.
- Descriptive statistics match independent base-R calculations; regression
  coefficients, standard errors, test statistics, p-values, confidence
  intervals and fit summaries match direct `stats::lm`.
- JSON results agree numerically with the rounded Markdown table cells.
- Seeded bootstrap percentile intervals match independently resampled direct
  `stats::lm` fits; repeated commands and saved-request replay produce the same
  numerical results and deterministic Markdown.
- Bootstrap calls without an explicit user seed also preserve enough RNG state
  to replay their numerical results and deterministic Markdown exactly.
- Replay uses the original input and configuration even after source data,
  the active working Parquet file and the current private YAML are changed.
  It does not overwrite the current working data or the original request.
- Replay uses its preserved custom template after the original file changes.
- Replay restores recorded numeric formatting options even when the calling
  process changes its R startup profile, decimal separator and display options.
- Corrupted preserved Parquet or dictionary files are refused and cannot add
  a completed analysis.
- Editing a saved request's scientific options cannot silently redefine the
  original analysis; the completed run's request hash must still match.
- Invalid model variables result in terminal machine-readable failure, no
  normal `output.md`, and no mutation of prior completed Markdown.
- Invalid scientific option domains (confidence levels, fractional bootstrap
  counts, non-finite thresholds and nonnumeric precision) cannot produce a
  completed run. Failures before dataset resolution may remain stderr-only.
- Ordinary R statistical warnings are retained in the result envelope.
- `--log FALSE` suppresses optional legacy JSONL, not the mandatory run bundle.
- `logging.include_user_prompt: false` also suppresses the user prompt in the
  mandatory saved request and legacy JSONL/Markdown, with optional logging both
  off and on; replayable CLI or command data never bypass this privacy choice.
- Quoted `"true"`/`"false"` logging flags in an isolated canonical YAML retain
  their enabled/privacy semantics. This tests the canonical loader itself,
  not only normalized overrides, without changing the shipped installation.
- Bootstrap/default-configuration and independent MI-core contract scripts are
  executed through their `tests.phase2.*` registrations.
- An injected publication-failure contract verifies rollback of affected legacy
  projections; its subprocess diagnostics are retained with the suite results.
- A seeded public imputation call preserves all three `mids` imputations with
  exactly the draws from an independent direct `mice::mice` call. End-to-end MI
  CLI tests fit Gaussian, binomial-logit, binomial-probit and Poisson models per
  imputation. Pooled coefficients, SEs, statistics, degrees of freedom, p-values,
  confidence intervals and Rubin diagnostics are compared with direct
  `mice::pool`; replay must reproduce both results and Markdown. Unsupported
  formulas cannot produce a completed run.

## Independence and limits

The runner does not source NLSS numerical or execution helpers. Numerical
expectations use public base-R/stats and mice APIs; JSON and Markdown are
inspected as external artifacts. Default numeric tolerance is `1e-8`; rounded table cells
are compared with their JSON values rounded to the declared number of digits.
Bootstrap equality is verified within the recorded R/RNG/package environment,
not claimed across arbitrary software versions or hardware platforms.

This suite is designed for deterministic execution output. It does not
constrain the separate semantic final report to a fixed narrative template.
