# Independent EFA acceptance

Run `Rscript tests/phase2/run_efa_tests.R --root /tmp/nlss-efa-check --keep 0`.
The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`.
`--match REGEX` selects named groups; `_smoke$` selects the explicit five-group
smoke subset. An empty selection exits nonzero and is never a successful suite.

All data, workspace manifests, configuration and templates are private temporary
fixtures. Public `efa.R` and `replay_run.R` subprocesses are the only NLSS execution
interface. The numerical reference uses direct `psych::principal`, `psych::fa`,
`psych::polychoric`, `psych::tetrachoric`, `psych::KMO`,
`psych::cortest.bartlett` and base-R matrix calculations. No NLSS scientific helper
or stored golden values supply an expected answer.

Coverage includes all eight retained extraction methods; unrotated, orthogonal
and oblique rotations; Pearson, Spearman, polychoric and tetrachoric correlations;
complete and pairwise data; fixed and eigenvalue-based retention; exact source-row
and pairwise sample counts; unsuppressed pattern/structure loadings, factor
correlations, reproduced/residual matrices, communalities, uniqueness, complexity,
item and overall KMO, Bartlett, eigenvalues and extracted variance. Numeric labels
remain numeric, declared ordered levels retain their order, and gapped SPSS codes
and duplicate value labels do not redefine categories. Missing groups and a real
`NA` level, close floating-point group IDs and unused levels remain distinct.

Display cutoff/sorting must not change scientific matrices. Singular PCA remains
available with explicit diagnostic limitations. Invalid selected data/options
must fail; a failed analysis must protect earlier report/log bytes and cannot
replay as completed. Replay must preserve statistics and Markdown after the
source, working Parquet, configuration and template are changed, without leaking
external paths or disabled prompts. These are fixture manipulations, not writes
to canonical installed configuration or user datasets.

Each run writes `phase2-efa/run-*/results.json` with the exact selection, per-group
pass/failure and numeric counts, source/test SHA-256 and R/package versions. Run
against a frozen R source tree: shared replay correctly rejects changed source.
Numeric tolerances are relative/absolute `3e-6`; probabilities use a relative
comparison with a `1e-300` floor. Smoke and targeted runs are not full acceptance.
No claim is made that exploratory fit validates a measurement theory, that nominal
pairwise N supplies a common-sample Bartlett test, or that template output is a
semantic final research report.
