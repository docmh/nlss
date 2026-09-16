# Independent assumptions acceptance

Run the registered runner through `bash cmdscripts/tests.sh phase2`, or directly:

```bash
Rscript tests/phase2/run_assumptions_tests.R --root /tmp/nlss-assumptions-check --keep 0
Rscript tests/phase2/run_assumptions_tests.R --root /tmp/nlss-assumptions-smoke --keep 0 --match '_smoke$'
```

The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`.
The full selection contains 118 cases; the smoke selection contains five
explicit cases, one per diagnostic family.
An empty selection fails. Do not present a subset as full-suite validation.
Only this runner's timestamped default-root outputs are subject to retention;
explicit roots are retained for audit. Private projects/configuration are used;
the installed canonical configuration and researcher data are never changed.

## Numerical independence

Every module invocation and replay runs the public R CLI in a fresh process.
The test process never sources NLSS scientific helpers, uses old NLSS results as
truth, or calls NLSS calculations to obtain expected values. Expectations use:

- `stats::shapiro.test` on the independently selected observed samples,
  paired differences, or fitted residuals; `car::leveneTest(center = median)`,
  `stats::bartlett.test`, `fligner.test` and `var.test` for all variance screens.
- `stats::lm` and `mauchly.test(X = ~1)` for repeated-measure contrast-space
  sphericity, with full crossed between-factor models and common subject rows.
  The ordinary multivariate covariance-sphericity test is not a valid substitute.
- Separately fitted nested `lm` models and their ANOVA F comparison for the
  added quadratic term. This is a bounded curvature screen; the OLS residual
  correlation with included predictors is algebraically uninformative.
- An independently fitted squared-residual auxiliary model for Koenker's
  studentized `n R²` statistic and effective design-rank degrees of freedom;
  direct Durbin–Watson residual differences; inverse design-column correlation
  matrices for VIF; `rstandard` and `cooks.distance` for case diagnostics.
  Dummy-column VIF is deliberately not relabelled as a term-level GVIF.
- Direct `lme4::lmer`, `DHARMa::simulateResiduals` and DHARMa tests for the
  mixed-model sample, residual diagnostics and controlled simulation seed;
  `lavaan::cfa` for SEM fit/case identities.

Raw comparisons use `2e-7` relative/absolute tolerance, with relative probability
checks and a `1e-300` floor. Counts/source identities use exact equality. The
runner records the number of scalar comparisons, not just a schema-pass count.
Bootstrap fixture counts are intentionally small execution-equivalence probes,
not recommended research settings or claims of bootstrap adequacy.

The existing full `run_mixed_diagnostic_tests.R` and
`run_sem_diagnostic_tests.R` suites remain separate regression safeguards.
They cover the previously corrected optional package diagnostics, optimizer
limits/singularity, Mardia/Mahalanobis/collinearity, FIML/ordinal/group identities,
admissibility and unavailable checks. Their presence is not a reason to replace
the new run-contract acceptance with legacy JSONL-only checks.

## Cases and failure semantics

Coverage includes CSV/SAV/RDS/RData/Parquet source identity; user-missing and value
labels; one-sample/independent/paired designs; auto-routing; crossed cells whose
display labels collide; nearby numeric category identities; common hierarchical
model cases; every variance method, residual toggle and diagnostic threshold;
singular covariance, constant predictors, aliased and saturated designs; and
explicit available/unavailable/skipped distinctions. A statistic without a
p-value (Durbin–Watson) can be available without an automatic diagnostic verdict.
Numerically perfect fits must not turn roundoff residuals into scientific
diagnostics. Legal nested random-slope expressions are compared with lme4.
SEM checks distinguish the requested display standardization from the fixed
fully standardized loading screen, explicitly state that confidence intervals
are not computed here, preserve adjacent-double ordinal category identities,
reject a requested grouping with only one observed group, and disclose explicit
syntax precedence over unused builder inputs. Conflicting explicit syntax
sources are rejected instead of silently choosing one.

Saved requests/results, immutable data/dictionaries, templates, artifacts and
run identities are checked against their SHA-256. Each family has replay coverage.
Dedicated probes mutate the original model file, input, editable working data,
template, private configuration and inherited SEM/mixed-model defaults after a
run. Replay must use the saved context, preserve raw results and deterministic
Markdown, and leave the original run/working data unchanged. Other probes verify
custom simulation/bootstrap seeds and reject tampered input/output/templates,
request/result identity changes and incompatible code/environment metadata.

Strict invalid options, impossible roles/designs and runtime failures must return
nonzero without changing earlier protected report/log/data/manifest bytes.
Project publication and dataset analysis locks are exercised; a contending
process may not remove another lock. `--log FALSE` and legacy output omission
do not suppress the mandatory run bundle. Prompt/source paths are masked without
corrupting URLs. Failed or pending analyses cannot be replayed as completed runs.

## Evidence and limits

`phase2-assumptions/run-*/results.json` records the exact selection, each case's
status, scalar comparison count, elapsed time, runner/module SHA-256 and relevant
R/package versions. Run final replay validation from a frozen source snapshot,
not while another process edits the R tree.

The five core smoke cases need `yaml`, `jsonlite`, `digest`, `arrow`, `haven`,
`car`, `lme4` and `lavaan`. The DHARMa seed case additionally requires DHARMa when
selected; its absence is not a reason to omit unrelated core checks. The runner
does not install packages or use the network. Native Windows execution and real
terminal prompting require their own evidence; Linux CLI tests do not certify
them. Diagnostic agreement does not establish model adequacy, causal validity,
pooled MI inference, or approval/rejection of a substantive research conclusion.
The final research report remains a semantic synthesis rather than this table.
