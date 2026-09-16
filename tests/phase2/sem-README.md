# Independent SEM acceptance

Phase-2 closeout additionally checks adjacent floating-point ordinal codes
through RDS/SAV/Parquet against separately constructed integer-category lavaan
references, including exact saved-run replay. Nonlexical existing factor order
remains covered; tests do not reuse NLSS category-conversion helpers as an oracle.

Run `Rscript tests/phase2/run_sem_tests.R --root /tmp/nlss-sem-check --keep 0`.
The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`.
`--match REGEX` selects named groups; `_smoke$` is an explicit six-group subset.
An empty selection exits nonzero. Never report smoke/subset results as a full run.

Public `sem.R` and `replay_run.R` subprocesses run in private fixture projects with
private configuration/templates. Expected numbers are independently fitted with
`lavaan::cfa` and `lavaan::sem`; a path regression additionally checks base-R OLS.
The runner neither sources NLSS scientific functions nor reads stored goldens.

Coverage includes CFA builders, explicit models/paths, path builders, simple,
parallel and serial mediation; ML/MLR/MLM/MLMV/MLMVS plus ordinal WLSMV/ULSMV/DWLS;
listwise/FIML/pairwise paths where supported; standard/robust/bootstrap inference;
normal, percentile and legacy `bca.simple` bootstrap intervals; standardization
and confidence level options; all raw parameter rows (including variance,
intercept, threshold and defined effects), selected legacy parameter projection,
fit, R-squared, full modification indices and standardized residual matrices.
Bootstrap uses explicit seeds and 40 resamples: enough for numerical/execution
equivalence, not a recommended research precision setting or bootstrap adequacy
claim.

Fitted group order, exact source cases, effective lavaan options, convergence,
admissibility and free parameter counts are checked separately from values.
Configural/metric/scalar/strict continuous invariance is independently fitted at
each step, including fit deltas. SAV labels/missing codes do not redefine numeric
or ordinal roles; explicitly ordered factors retain nonlexical level order.

Publication/replay cases preserve frozen model-file text, source data, private
configuration/templates, mandatory raw output despite legacy opt-out, and exact
Markdown/statistics after original artifacts are changed. Invalid selected data,
model/options and failed fit attempts must not become completed publications or
overwrite protected earlier reports/logs. External paths and disabled prompts
remain private. Replay is tested on a frozen R tree, not while another agent edits
its source.

`phase2-sem/run-*/results.json` records the exact selection, per-case numeric counts
and pass/failure, runtime/test SHA-256, the R source inventory and R/package
versions. Tolerance is `3e-6` relative/absolute; probability comparison is relative
with a `1e-300` floor. Numerical agreement does not establish causal mediation,
instrument validity or substantive invariance. A converged but inadmissible model
must remain visibly qualified; free semantic research reports are not replaced by
these deterministic statistical tables.
