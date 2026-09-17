# Resolved analysis requests and replay

## Current project location boundary

For a current marked project, common publishers route all statistical and parameter-only
records to `.nlss/runs/<id>/`, utility records to `.nlss/utility-runs/<id>/`,
and the automatic cumulative protocol to root-level `report_canonical.md`.
Figures, numerical plot data and fitted objects stay in their owning run;
new standalone mids artifacts use `.nlss/imputations/<id>/` and are referenced,
not copied per consumer. Visible working data and original sources stay separate.
Current-project working changes reuse preserved before-versions and protected
publication, with no permanent backup family or duplicate project-level plots.

Replay supports the current marker/run layout through shared discovery and keeps
its exact code/environment/input/artifact checks. It never activates an older
working dataset. No old-layout reader or conversion is supplied. See
[replay-run](utilities/replay-run.md) and [MI input locations](mi-pooling.md).
New unmarked data-file analyses remain available without implicit project
adoption; newly generated mids artifacts use that same imputation layout.

Use this reference for `descriptive_stats.R`, `regression.R`, `mi_regression.R`,
`frequencies.R`, `crosstabs.R`, `data_explorer.R`, `scale.R`, `reliability.R`,
`t_test.R`, `correlations.R`, `anova.R`, `nonparametric.R`, `mixed_models.R`,
`efa.R`, `sem.R`, `power.R`, `assumptions.R`, `plot.R`, `data_transform.R`, `missings.R` and `impute.R`,
or when repeating one of their saved runs. Utility/lifecycle entrypoints use
their own [evidence contract](utility-contract.md); standalone history readers
remain read-only/explicit-export tools. See [compatibility inventory](compatibility-inventory.md).

## Execution boundary

These migrated entrypoints use one mandatory bootstrap and execution boundary.
Statistical calculations remain in their existing R functions or established
`stats`/`mice`/`car`/`emmeans`/`lme4`/`lmerTest`/`psych`/`lavaan`/`pwr`/`semPower` methods. Canonical defaults have one source, `scripts/config.yml`.
Use `NLSS_CONFIG_PATH` for a partial, validated user/test override rather than
editing the installed canonical default/type definition. CLI flags still win.

An ordinary analysis in a current project produces:

```text
.nlss/runs/<run-id>/
  request.json          # resolved model/options, fixed input, relevant config
  result.json           # completed/failed state, unrounded results, warnings
  output.md             # deterministic statistical output for this run
  templates/*.md        # exact templates used
  fits.rds              # MI: all per-imputation fitted models
  pooled.rds            # MI: the pooled mice object
  plots/*               # Plot: preserved figure files
  plot-data.rds         # Plot: unrounded data, summaries and graphical layers
```

`request.json` includes the dataset and dictionary SHA-256, effective scientific
options, model/factor/case-selection information where applicable, relevant
configuration, R/package/system-library versions, locale/timezone, R contrast/NA
options and RNG information. Unrelated module configuration, including literature
service credentials, is not included. Source paths are taken from the verified
dataset reference; workspace-external paths in saved context are masked.

For Gaussian/GLM regression, missing cases are excluded jointly over all variables
needed by its blocks. Each group's included rows, factor levels and variable
classes are recorded. Existing centering, standardization, blocks, interactions,
grouping and confidence/Bootstrap options remain available. Bootstrap now uses
the explicit seed or `modules.regression.seed` (default 1); the seed and RNG state
are saved. Numeric option domains are validated before estimation.

Frequencies and cross-tabulations record their actual group partitions and
factor/category ordering. Missing group values form a separate group; a real
category named `NA` is never merged with it. Cross-tabulations also record the
complete cases for every row/column pair. Simulated Fisher tests use `--seed` or
`modules.crosstabs.seed` (default 1), independently of regression settings; R's
actual exact/simulated method is recorded. Data exploration records its selected
variables and explicitly identifies measurement-level inference as heuristic.

Scale analysis records effective itemwise reverse bounds, coercion losses,
variablewise/joint/pairwise case selection and matrix-validity diagnostics.
Its standardized one-factor omega uses `stats::factanal` with one deterministic
start. Reliability records the chosen ICC model/type/unit, actual missing-case
rules, kappa category ordering and long-to-wide ID/rater/source-row mapping.
Neither module changes the working Parquet. See their [scale](subskills/scale.md)
and [reliability](subskills/reliability.md) references for estimand and interval
limitations; sharing an execution contract does not establish instrument validity.

T-tests record their actual one-sample, independent or paired design, observed
group order, retained/excluded source rows, null hypothesis and effect-size
definition. One-sample analytic intervals for the mean are distinguished from
intervals for mean minus the null value. Correlations preserve each group's
pairwise/listwise selection and effective control-design rank, with explicit
exact/approximate inference and unavailable-estimate statuses. Both modules use
their own canonical `modules.<module>.seed` (default 1) when bootstrap is enabled
without `--seed`; unsuccessful bootstrap draws are counted and disclosed.
See [t-tests](subskills/t-test.md) and [correlations](subskills/correlations.md)
for interval targets, rank-method approximations and bootstrap limitations.

ANOVA preserves complete-case selection after numeric conversion, exact factor
and subject assignments, formula/model rank, requested/effective sums of squares,
and effect-size definitions. Its bootstrap uses `--seed` or `modules.anova.seed`
(default 1), resampling complete subjects jointly for repeated measurements.
External custom-contrast definitions are saved in the resolved request rather
than reread from a mutable file during replay. Type-II/III between-subject tests
require `car`; they must not silently become Type I when a dependency or fit fails.
The repeated-measures adapter retains its sequential error-stratum model and
does not imply newly implemented Type-II/III repeated-measures inference.
Explicit Type-II/III repeated-measures requests are rejected; an implicit
between-subjects default is disclosed alongside the effective Type I.

Rank tests preserve sample/pair direction, actual groups, repeated subject rows,
ties/zero differences, requested/effective exactness, continuity correction and
p-adjustment families. Their deterministic base-R tests need no RNG seed.
Unavailable estimates and approximations are disclosed, not silently presented
as exact inference. See [ANOVA](subskills/anova.md) and
[nonparametric tests](subskills/nonparametric.md) for the retained estimands,
legacy naming compatibility and design-specific limits. Neither adapter claims
pooled multiple-imputation inference.

Linear mixed models record the actual fitted model frame and source rows,
cluster memberships, factor contrasts, model-matrix rank, ML/REML choice,
optimizer status and requested/effective inference. Explicitly requested
Satterthwaite or Kenward–Roger inference must not silently fall back to a
different method, including for marginal means. Singular fits and convergence
issues are distinct; disabling optional residual diagnostics does not suppress
the mandatory fit-status record. Dropped fixed effects, non-estimable means or
contrasts and unavailable R²/ICC estimates retain explicit statuses. External
contrast definitions are preserved in the request; marginal-mean inference
uses an explicit seed or `modules.mixed_models.seed` (default 1) to make
stochastic adjustments replayable. See [mixed models](subskills/mixed-models.md)
for the linear-model scope and inference boundaries. This adapter does not add
GLMMs or pooled MI. Standalone diagnostics have their own matching requirement below.

EFA preserves requested/effective extraction, rotation and factor retention,
source cases and group identities, category ordering, correlation/count matrices,
raw loadings and factor correlations. Display cutoffs do not erase the underlying
estimates. SEM preserves resolved lavaan syntax (including external model files),
actual groups/cases, requested/effective estimation, fit/admissibility and
bootstrap availability. Their separate canonical `modules.efa.seed` and
`modules.sem.seed` (default 1) control stochastic execution and replay.
See [EFA](subskills/efa.md) and [SEM](subskills/sem.md) for scientific corrections,
existing method scope and explicit limitations. Neither claims pooled MI.

`assumptions.R` now uses the same dataset-backed contract for all five families:
t-test, ANOVA, linear regression, linear mixed models and SEM. Each requested
check records `available`, `skipped` or `unavailable`; a completed run is not a
blanket assumption pass. Diagnostic refits record their actual formulas, cases,
groups and fit status. They do not certify a different analysis model or add
pooled MI. Regression blocks share complete cases; repeated-measures Mauchly
tests use the within-contrast space. The quadratic added-term F test is a limited
curvature screen, not a proof of linearity.

Mixed diagnostics preserve `modules.mixed_models` fit defaults and use `--seed`
or `modules.assumptions.mixed_models.seed` (123) for DHARMa; SEM refits preserve
`modules.sem` defaults and use `--seed` or `modules.sem.seed` (1). Only the used
family's inherited configuration is frozen alongside assumptions defaults.
External SEM model text and selected diagnostic templates are saved for replay.
See [assumptions](subskills/assumptions.md) for diagnostic scope and interpretation.

Plot preserves the exact source rows, variable roles, category/label identity,
summary calculations and unrounded ggplot2 layers used to draw its figures.
Its run-local images are SHA-256 artifacts. `output.md` numbers figures from one
and links directly to these images; the root protocol retains continuing
numbering and links rebased to the same run-local files, without image copies.
Images use explicit R graphics devices, with their selection recorded.
Replay verifies the preserved image bytes and recalculates the numerical layers
and Markdown. It does not promise byte-identical newly rendered image files:
device metadata and font rendering are not a cross-platform pixel-equivalence
contract. See [Plot](subskills/plot.md) for denominators and graphical limitations.

`result.json` and `output.md` use the same calculated result values and shared
renderer. The run's Markdown starts its own table numbering and excludes changing
timestamps/path headers; metadata belongs in JSON. The automatic root
`report_canonical.md` is the human-readable projection. No parallel current-project
JSONL log is produced. `--log FALSE` suppresses neither run evidence nor the root
protocol, and saved results do not depend on `logging.include_outputs`.

## Data-changing transformations

`data_transform.R`, `missings.R` and `impute.R` keep the dataset-backed schema-1 input contract and add
`results.data_change` plus the hash-registered `data-change.json` artifact.
They bind the unchanged input reference to the immutable output data/dictionary
through shared version descriptors and content-addressed objects. Requested/effective rules, execution
order, source rows, column changes and missing/nonfinite statuses remain in the
request/result. The current import binding is not replaced by a fabricated new
external source. A run-local `codebook.md` preserves the output preview.

On a current-project data change, the common publisher binds the preserved
before/after versions and rechecks the visible working file before replacement.
It reuses the preserved input object for rollback; there is no additional
permanent backup family. No-ops do not replace working data. Detected external
edits are not blindly overwritten, and only a completed publication establishes
an applied change.

Failure may retain calculated results, a candidate object or pending evidence
with explicit publication/recovery state. Inspect that state before using the
result or recovering a working file. This is not crash-atomic multi-file recovery
or a lock against external editors. See the shared
[publication and recovery boundary](utilities/project-create.md#publication-and-recovery).
Do not reimport the original to recover a change: that can discard valid edits.

For `data_transform`, replay requires both saved and recomputed eligibility: built-in operations and
a conservative set of explicitly bound plain-vector expressions are supported.
General R expressions remain available on the ordinary CLI, but unrecorded
external state, randomness or unverified functions/classes make them explicitly
non-replayable. They are never tried speculatively during replay. Exact authored
expressions remain in private request/result artifacts; contextual human-readable
output masks external paths. These artifacts need research-data access controls.

Transformation replay verifies the original before/after references and retained
artifacts, recalculates from the original input and checks output data/dictionary
identity. It creates a new run without activating its output, changing the
current dictionary/codebook or creating a working-data backup. Publication mode
is explicit in JSON; the numerical transformation Markdown remains stable.
See [data-transform](subskills/data-transform.md) for operations and safeguards.

Missingness handling uses the same publication and replay boundary, without
evaluating authored R expressions. Its `data_change.source_rows` and resolved
`design.source_rows` explicitly map each output observation to its input-version
row (`observation_basis: input_version_rows`). Only ordered, unique subsets are
allowed; zero output rows have an empty map. This does not permit variable
transformations to delete/reorder rows. Replay verifies both the row map and
output data/dictionary identity. Original source missing codes/tags remain
historical metadata, not a mask reapplied after filtering or imputation.
Per-variable handling evidence records actual filling, remaining missingness
and affected observations. All five methods remain; the automatic threshold
choice is a heuristic, not an inferred missing-data mechanism or approval for
inference. See [missings](subskills/missings.md).

Imputation also uses this boundary but preserves every original column and row
order, adding only collision-safe completion/indicator columns. Requests freeze
the actual simple/mice/VIM engine, methods, input version and seed (canonical
`modules.impute.seed` is 1). Results distinguish attempted from actual filling,
remaining missingness and single-completion inference limits. The exact original
`mids` and its metadata remain at their existing content-addressed location;
an authenticated run-local reference additionally binds both hashes. Replay
verifies that evidence, compares recomputed scientific fields (excluding only
top-level runtime call/date) and reuses the immutable artifact, without activating
old output or claiming new RDS-byte identity. Verified unreferenced artifacts may
remain after a later failed data/run publication. See
[impute](subskills/impute.md); existing supported [MI inference](mi-pooling.md)
continues to consume all preserved imputations, never averaged `_imp` columns.

## Parameter-only study planning

Power supports a second, explicit input kind: parameters without a dataset.
`--planning TRUE` selects it, as does omitting dataset selectors when effect
estimation is disabled. `--planning FALSE` retains loading from the current or
active dataset. Explicit source flags retain dataset-backed operation. See
[Power](subskills/power.md) for conflicting-input validation and effect estimation.

In a current project, parameter-only runs use `.nlss/runs/<run-id>/`, just like
dataset-backed analyses. They never create, select or alter a dataset.
Unmarked standalone planning can use its existing configured planning directory;
it does not adopt an NLSS project or imply support for an older project marker.

Planning requests/results use schema version 2, `input: {"kind": "parameters"}`
and `dataset: null`. Existing dataset-backed schema-1 runs are unchanged.
Resolved options record the design, effect-size basis and source, sample-size
interpretation and calculation assumptions. There is no fabricated input hash;
the request/result hash association protects the recorded parameters. Templates,
code/environment verification, warnings, staging, publication locks and failure
handling use the same execution boundary. Planning replay uses the saved parameter inputs and current project run route,
even when a dataset is active.

The same root protocol is extended automatically, without another log or
planning-local protocol. The agent keeps any useful rationale and authored report
at chosen visible project paths; ordinary report delivery uses `project-report`,
without a dataset-only lifecycle logger or mandatory filename.

This input kind does not promise data-free execution for other procedures.

## Failure and publication

A resolved run is first staged in `.nlss/runs/.pending-*`; its final directory
is published after required records, artifacts and the automatic root protocol
have been written through the shared publication boundary. Ordinary publication
errors restore protected files. Failed/partial results retain diagnostic output,
not a false completed analysis. Input errors before resolution may produce only
stderr/nonzero exit status without inventing a dataset or project.

Plot figures remain in their owning run. No project-level image copies are
published or overwritten; `--overwrite TRUE` does not replace prior run evidence.
Unsafe paths and unexpected collisions fail explicitly.

Scale may publish valid item descriptions with explicitly unavailable
alpha/omega; this is not a claim that those coefficients were estimated.
Reliability cannot publish a completed analysis without its requested primary
estimate. A valid point estimate can still have an explicitly unavailable
interval or test. Its legacy `--expect-invalid` test convention may return exit
0 for an expected input failure, but the recorded run remains `failed`, has no
normal output and cannot be replayed as a completed analysis.

Migrated publications use a short project-level `.publication-lock` as well as
the dataset analysis lock, protecting shared manifest updates and rollback from
other migrated publications. Contending publication attempts fail explicitly.
An abruptly killed process can leave `.pending-*`, `.analysis-lock`, or
`.publication-lock`. Such a
directory is **not a completed run**. Inspect it and verify no process is running
before recovery. This phase does not claim transactional recovery from power loss
across all project files or protection against uncooperative external writers.
Never treat a success-shaped JSON file inside a pending directory as published.

## Replay

```bash
Rscript "<skill>/scripts/R/run_nlss.R" replay-run \
  --request /path/to/project/.nlss/runs/<run-id>/request.json
```

Replay creates a new run in the selected current project's `.nlss/runs/`; it never overwrites the selected run
or replaces the current working Parquet. It uses the immutable input, saved
configuration and copied templates, even if original source data, working data,
user configuration or original templates have changed. The request remains in
its project-relative run hierarchy. The request hash is checked against its
completed result; data/dictionary/template hashes and recorded output artifacts are
verified as appropriate.
Only `request.json` inside the published directory matching its saved run ID is
accepted; pending or renamed directories cannot masquerade as completed runs.

Exact replay refuses changed NLSS R code, R version/platform, recorded package or
system-library versions and locale/timezone. This is a verification gate, not an
installer or a promise of bit-identical computation across arbitrary platforms.
Keep the appropriate software environment available; there is no automatic
package installation, downgrade or migration. Hashes are association/integrity
checks, not an external digital signature against coordinated file rewriting.

## Scientific interpretation

`output.md` is a SPSS-like statistical output, **not a prescribed final report**.
Use the preserved values, design, diagnostics and warnings as evidence for a
context-sensitive synthesis. A stored request does not supply a missing design
justification, validate a causal claim or establish adequate imputation.

## Shared project boundary

All statistical modules use the common execution/output route. Dataset-backed
request schema 1 and parameter-input request schema 2 retain their distinct
meanings; neither is the root project marker schema. Project management does
not add module-specific storage logic or reduce statistical capabilities.

Study documents are ordinary agent inputs. Authored-report preservation uses
the existing report store with selected evidence, not note capture or a second
utility journal. `project-inspect` reads current records to link the protocol,
data, analyses/utilities and authored-report revisions; it does not certify
their integrity, rerun analyses or maintain an index. Only the current layout is
supported, with no old-version project reader, adoption or conversion.
