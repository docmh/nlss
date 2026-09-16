# Nonparametric regression and numerical checks

Run `bash tests/smoke/run_nonparametric_tests.sh --root <private-root> --keep 0`.
Use a private repository copy because the legacy runner owns temporary workspace
manifest state. Paths/defaults and the module runner remain in `tests/tests.yml`.

The legacy suite covers all five designs, automatic selection, alternatives,
exact/continuity options, post-hoc adjustment, effects, supported import formats,
workspace continuation, templates, interactive input and log suppression. Invalid
dataset-backed designs must return nonzero, publish exactly one failed bundle,
release the analysis lock and preserve prior report/log bytes.

`tests/values/nonparametric_compute_golden.R` computes five focused summary
references, two selected post-hoc comparisons and six diagnostic references.
It calls base R directly and never imports the NLSS module. Independent effect
references use pairwise superiority for independent rank biserial correlation
and invert base R's uncorrected one-sided normal probability for rank `r`.
Regenerate with `--data`, `--out`, `--posthoc-out`, `--diagnostics-out` to private
paths first, then inspect scientific changes before replacing fixtures.

Wave 5 corrects the previous duplicate subtraction from R's Mann-Whitney
statistic and incorporates tied-rank variance in `r`; primary R p-values and
intervals remain unchanged for the existing fixtures. The old `epsilon_sq`
option retains its numerical formula, now correctly named `eta_H_sq`. Probability
checks use relative tolerance without an absolute 1e-6 floor; explicit one-sided
infinity statuses are checked rather than treating JSON null as an ordinary
missing bound.

The independent Phase 2 omnibus/rank acceptance suite additionally covers raw
group identity, labels/user-missings, actual complete cases, signed zero/tie
policy, CI coverage and unavailable states, Friedman subject uniqueness,
post-hoc family membership, publication failure, exact replay and input/template
changes. It supplements the legacy fixtures rather than copying their algorithms.
