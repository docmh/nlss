# ANOVA and rank-test acceptance

`run_design_tests.R` runs the public `anova.R`, `nonparametric.R`, and
`replay_run.R` entrypoints in private dataset projects. It does not source any
NLSS statistical function or read the historical numerical goldens.

```bash
Rscript tests/phase2/run_design_tests.R --root /tmp/nlss-design-check --keep 0
Rscript tests/phase2/run_design_tests.R --root /tmp/nlss-design-smoke --keep 0 --match smoke
Rscript tests/phase2/run_design_tests.R --root /tmp/nlss-anova-check --keep 0 --modules anova
```

The complete suite is mandatory in the normal `phase2`/`all` harness. The smoke
runner selects ten representative groups already present in that suite; it does
not substitute a reduced acceptance run for the complete suite. `--match REGEX`
supports diagnosis and records the selection in `results.json`; an empty
selection fails. `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and `NLSS_TESTS_CONFIG` follow
the other Phase 2 runners. Dependencies: `yaml`, `jsonlite`, `arrow`, `digest`,
`haven`, and, for ANOVA cases, `car` and `emmeans`; there are no downloads or
automatic installations. A nonparametric-only selection does not require the
ANOVA-specific packages.

## Scientific coverage

| Area | Independent expectations |
| --- | --- |
| Between-subjects ANOVA/ANCOVA | `stats::lm`/`anova` and `car::Anova`, Types I/II/III, unequal factorial cells, covariates, explicit sum coding for Type III |
| Effect sizes | Centered observed total SS, fitted residual SS and the relevant error-stratum MSE; eta, partial eta, omega and partial omega are checked on every row |
| Within/mixed designs | Separately constructed long data and `stats::aov(... + Error(id/within))`, numeric nonmonotonic IDs, nonalphabetic within order, listwise missingness, with/without covariates |
| Posthoc | `TukeyHSD` at the requested confidence level, independent Welch tests, paired tests on the omnibus complete-subject subset, declared adjustment families |
| Marginal means and contrasts | `emmeans` grids, built-in/custom and interaction-named weights, covariate adjustment, repeated/mixed contrasts, external contrast-file replay, seeded multivariate-t adjustment, requested versus effective correction (Tukey to Sidak for non-pairwise contrasts) |
| Sphericity | `stats::mauchly.test(..., X=~1)` and `car::Anova` repeated-measures GG/HF epsilons with corrected df and p |
| Bootstrap | Independently sampled complete case/subject rows with the documented seed and draw order; percentile effect intervals, valid/discarded counts in JSON and Markdown, exact replay |
| Signed-rank/rank-sum | Installed `stats::wilcox.test` primary tests and separate location CI calls: one-sample, paired and Mann–Whitney; all three alternatives; exact/automatic/asymptotic settings; continuity, ties and zero differences |
| Rank effects | Signed weighted rank balance; independent pairwise superiority; variance of pooled ranks; standardized tie-corrected rank score, rank-biserial effect, untruncated rank eta, and Kendall W |
| Kruskal/Friedman | `kruskal.test`/`friedman.test`, same complete blocks for Friedman posthoc, every `stats::p.adjust.methods` option, unavailable planned comparisons included in family size |

The fixtures deliberately differ from the legacy golden dataset and contain
unequal sample sizes, reordered factor levels, shuffled subject identifiers,
different missing rows across variables, and actual SAV variable/value labels
with declared user-missing values. Nearby numeric and fractional-timestamp group
values test separation of raw identity from display text.

## Numerical and output checks

Ordinary finite values use a scaled tolerance of `3e-8`; probabilities use
relative `2e-7` with a `1e-300` floor, so rounding a small nonzero p to zero does
not pass. Infinite interval bounds require their explicit JSON status. Rank
effect expectations are not reconstructed from p-values: exact inference and
the descriptive standardized rank effect need not imply the same z.

The installed R method is authoritative for exact/tied rank inference. R 4.5.2
and newer R development versions differ in exact handling of ties; the suite
checks the actual `wilcox.test` method rather than assuming one across versions.
It separately checks reduced attainable confidence coverage (three observations:
a requested 95% signed-rank interval may achieve only 75%), effective
approximation, and unavailable posthoc status in the **Markdown**, not only JSON.
See the official [Wilcoxon test documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html).

Sphericity is tested in within-subject contrast space, not by asking whether the
untransformed response covariance is spherical. See the official
[Mauchly documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mauchly.test.html)
and [multivariate ANOVA documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.mlm.html).
Contrast expectations use the public package behavior described in
[emmeans comparisons and contrasts](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html).

## Audit and regression boundaries

Each completed run is checked for input/dictionary/template/artifact hashes,
resolved model context, and raw results. Explicit source-row, group-partition,
zero-difference and subject mappings are checked against the fixture. Replay
must reproduce raw JSON results and deterministic Markdown after changing the
source, editable working data, user configuration, and external templates or
contrast specifications; it must not overwrite the current working data.

Privacy cases combine disabled user-prompt logging, disabled legacy result
logging, and `--log FALSE` while requiring the mandatory run bundle. Failed
analyses must preserve earlier report/log bytes, release locks, contain an error
without normal output, and refuse completed-run replay. Invalid option domains,
duplicate roles/IDs, nonnumeric factors, infinity and nonestimable primary tests
are negative cases. ANOVA includes nonzero constant responses, exact constant
factor cells, and identical within-subject change profiles; near-perfect but
genuinely varying observations are not excluded by an arbitrary epsilon cutoff.
These module checks complement, not replace, the shared
publication-rollback tests in the core Phase 2 suite.

Per-case commands, fixtures, configurations, published bundles and `results.json`
remain under the selected run root. The normal test harness and historical
module suites remain separate regression safeguards. None of these deterministic
tables prescribes the semantic structure or interpretation of a final research
report.
