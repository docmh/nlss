# Plot numerical and artifact acceptance

The runner is registered in `tests/tests.yml` and included in the complete
Phase-2 harness. It honors `--root`, `--keep`, `--match`, `NLSS_TEST_ROOT`,
`NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`. It uses private offline projects and
configuration, never edits the installed defaults and preserves explicit roots.

```bash
Rscript tests/phase2/run_plot_tests.R --root /tmp/nlss-plot-check --keep 0
bash cmdscripts/tests.sh --module plot --root /tmp/nlss-plot-legacy --keep 0
```

The new suite compares unrounded graphical values with independent base-R and
ggplot2 calls through the public CLI, without using NLSS calculation helpers as
oracles. It covers all nine types, smoothing/summary choices, group identities,
labels, missing/non-finite observations, percentage denominators and image formats.
Exact source-case and numeric-category identity are checked independently of
formatted labels. Run-local figures and Markdown are checked separately from
continuing canonical figure numbers and compatibility image paths.

Replay, source/configuration/template independence, tampered artifacts and
invalid requests are tested alongside numerical equivalence. The standalone
`plot_publication_contract.R` exercises ordinary publication faults after image
copying, report/log writes and bundle rename, verifying exact rollback of prior
images/report/log/manifest and absence of orphan new copies. Those injected bytes
test file transactions, not image rendering. Actual device outputs and graphical
layers are covered by the public-CLI suite and manual visual inspection.

The smoke selector is an explicit representative subset from `tests/tests.yml`;
it does not replace the full numerical suite. Machine-readable `results.json`
records cases and comparisons. The older module runner retains its original
valid invocations, now with isolated template configuration and stronger artifact
checks. Native Windows/device/font equivalence is not implied by Linux results.
