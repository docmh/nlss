# Multiple-imputation inference contract

NLSS fits one identical model to each of the **preserved** `mice` imputations, then delegates coefficient and uncertainty pooling to `mice::pool(rule = "rubin1987")`. It never substitutes the mean/mode `_imp` columns produced by `impute.R`. Successful results carry `inference_pooled: true`; unsupported or failed models produce an error, not a fallback result.

## Supported first adapter

| Family | Fitter / links | Response |
| --- | --- | --- |
| Gaussian | `stats::lm`, identity | Finite numeric outcome |
| Binomial | `stats::glm`, logit, probit, cloglog | 0/1 or exactly two factor levels; first level failure, second success |
| Poisson | `stats::glm`, log | Nonnegative integer counts |

Models accept named numeric, integer, logical, or factor columns. Formula syntax is deliberately plain: `y ~ x + group`, `y ~ x * group`, interactions with `:`, term removal with `-`, parentheses, and intercept `0`/`1`. Backticks permit non-syntactic column names. Formula function calls, implicit `.`, transformations, powers, random effects, offsets, weights, subsets, matrix outcomes and other model families are not accepted by this adapter. Prepare transformations explicitly and regenerate the imputations when required; do not insert new columns into an old artifact. Other existing NLSS statistical modules and packages remain available unchanged.

Unordered predictors use treatment contrasts with their stored first level as reference; ordered predictors use polynomial contrasts. Contrast matrices, response coding, family, link and formula are recorded explicitly, independent of ambient R contrast options. All imputations must retain identical cases, model terms, classes, factor levels and residual degrees of freedom. No incomplete cases are silently removed. Model warnings, rank deficiency, non-finite uncertainty, failed GLM convergence or a boundary solution abort pooling. This is intentionally conservative; a blocked model needs scientific review, not automatic changes to its estimand.

## Input verification

Pass a current `.nlss/imputations/mice-<sha256>/mids.rds` file or its containing
directory. Its adjacent `metadata.json` must identify the same hashed `mids`
object, at least two imputations, iteration count, generation version and
available seed. The existing project-relative metadata path identifies the root,
not a fixed number of parent directories. An explicit selected project must agree
with that root; a present marker must use the supported current format. A newly
run unmarked file analysis uses the same artifact layout without creating a marker.
Old dataset-local imputation directories are not read or converted.

The original dataset reference is mandatory. NLSS checks the preserved input
Parquet and dictionary hashes and the corresponding version record: the existing
`.nlss/datasets/<id>/versions/<version>.json` for registered object storage, or
the existing snapshot `provenance.json` for an ordinary explicit source import.
These are input-storage choices within the current workflow, not old-project
adapters. Every original `mids$data` value and missingness position is compared
against the selected source columns, allowing only the established
character-to-factor preparation. A working Parquet subsequently gaining `_imp`
columns does not change this historical source reference.

The workspace-relative paths must remain within the supplied workspace. Keep the artifact, metadata, original snapshot and dictionary together when moving a project. Hashes provide integrity and association checks, not an externally signed proof of authorship; coordinated rewriting of a complete project is outside that guarantee. RDS artifacts are local trusted-project research objects, not a general-purpose untrusted upload interface.

## Statistical outputs and diagnostics

The unrounded coefficient table contains `estimate`, `std.error`, `statistic`, `df`, `p.value`, `conf.low`, `conf.high`, `m`, `ubar`, `b`, `t`, `riv`, `lambda`, `fmi`, and `dfcom`. The last fields expose within-imputation, between-imputation and total uncertainty and the diagnostics returned by `mice`. Confidence intervals and tests come from `summary.mipo`, not a separately implemented approximation. The complete-data degrees of freedom are taken explicitly from the common per-imputation residual degrees of freedom; `mice` applies its Barnard–Rubin adjustment.

Gaussian coefficients are on the response scale; binomial and Poisson coefficients and intervals are on the **link scale**, with no implied exponentiation or universal odds-ratio interpretation (in particular for probit/cloglog). The raw per-imputation fit objects and pooled `mipo` object remain available for persistence by the entrypoint. Each fit includes sample size, rank, residual degrees of freedom, convergence, boundary status, iteration count, deviance and AIC. AIC and deviance are per-fit diagnostics, **not pooled model-comparison statistics**.

The source `mids` object retains its imputation-chain diagnostics; generation iteration count and logged events are exposed in the results. Successful fitting/pooling does not demonstrate adequate imputation convergence, MAR plausibility, a congenial imputation model, independence, linearity, absence of overdispersion, or a justified causal interpretation. Review these against the research question. No pooled omnibus tests, model comparisons, marginal predictions, robust/survey inference, random-effects pooling, or automatic imputation adequacy decision is claimed.

## Reporting

Use the deterministic table as evidence, then interpret it semantically against hypotheses, outcome coding, effect scale, missingness mechanism, limitations and the substantive research context. Templates only organize presentation; they are not a substitute for a final scientific narrative. Document how imputations were generated, all `m` models, uncertainty pooling, relevant diagnostics and any limitations. A failed model must not result in a confident report or a hidden analysis of averaged data.

## Primary references

- [mice pooling documentation](https://amices.org/mice/reference/pool.html): model-per-imputation workflow, Rubin pooling and degrees-of-freedom behavior.
- [mice completed datasets documentation](https://amices.org/mice/reference/complete.mids.html): extraction of all preserved imputations.
- [R GLM documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html): family/link specification, response coding, convergence and rank behavior.

The adapter is tested against installed `mice` and base R using independently fitted `lm`/`glm` models and direct `mice::pool` summaries, including each supported link, interaction syntax, intercept variants and confidence levels. No numerical golden is computed through the NLSS pooling adapter.
