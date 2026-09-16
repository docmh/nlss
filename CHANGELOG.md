# Changelog

Notable changes for researchers and maintainers, following
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
New releases use [Semantic Versioning](https://semver.org/). Older tag names
are retained as originally published. This is a curated history, not a commit log.

## [Unreleased]

## [2.0.0] - 2026-09-16

Changes relative to the previous published release, **1.0.2z**.

### Breaking changes and upgrading

- **New project layout.** A user-owned folder with `nlss-workspace.yml` now
  keeps visible working data and the automatic root `report_canonical.md`,
  with managed run/data/report evidence in `.nlss/`. The old per-dataset layout
  is not imported, adopted or converted. Preserve old projects unchanged;
  start a new project from an explicitly selected source dataset when needed.
  This does not transfer old analysis history or certify old results.
- **Changed automation entry points.** Use `project-create` for initialization,
  `project-inspect` for browsing and `project-report` for authored revisions.
  The old initializer and metaskill activation/finalization workflow are not
  the setup/report route for current projects. Saved runs use request/result
  JSON and Markdown, not a current-project `analysis_log.jsonl`.
- **Stricter arguments and dependencies.** Unknown/duplicate options, malformed
  values and invalid configuration fail explicitly. Requested methods do not
  silently fall back to different inference when required packages are absent.
  SPSS import uses `haven`; plots require `ggplot2`. Existing statistical
  families remain available. Review old scripts against operation help.
- **Numerical corrections can change earlier conclusions.** In particular,
  review prior repeated-measures/mixed ANOVAs, Mann–Whitney effects, mixed-model
  tests/contrasts, EFA summaries and diagnostic/power calculations under the
  conditions below. Recompute into a new project/run when appropriate and
  compare scientifically; do not overwrite historical evidence. Exact replay
  with old code and recalculation with corrected code are different tasks.

### Added

- Shared saved-request execution and explicit verified replay for statistical
  procedures, with selected input versions, effective settings, seeds, software
  context and artifacts. Replay does not recreate historical R installations.
- Preserved `mice` multiple-imputation artifacts and `mi-regression`: pooled
  coefficients and uncertainty for supported Gaussian identity, binomial
  logit/probit/cloglog and Poisson log models. Single completed or averaged
  columns are not a substitute for pooled inference.
- Direct project initialization/import from CSV, SAV, RDS, RData or Parquet;
  registered working data remain accessible outside `.nlss/`.
- Read-only project inspection and freely authored report revisions linked to
  their selected existing evidence. Report filenames and prose structure are
  researcher choices, not new template requirements.
- Dataset-free power planning through the ordinary project run route.
- One portable scientific payload as plugin and standalone skill, plus release
  inventories/checksums and isolated installation/update/removal support.
- Automatic operation-specific R dependency preflight and a separate approved
  installation route, including recovery when foundational packages are missing.

### Changed

- Shared import semantics keep variable/value labels, source identity and
  user-missing definitions separate from analytical values. CSV interpretation
  can explicitly select encoding, decimal mark, column types and missing values.
- All procedures use common CLI/configuration and publication boundaries.
  The root protocol is maintained automatically; figures remain run-local and
  links are rebased. Data changes preserve before/after versions without a
  parallel journal or a permanent family of duplicate backups.
- Scientific answers and reports adapt to the requested scope. Full reports
  remain semantic syntheses, with required agentic source appraisal when
  literature research is called for; no compulsory module-specific prose.
- README and the installation guide provide a researcher-first introduction
  and an explicit repository-link-to-release path. Source checkouts are
  distinguished from managed release archives.

### Fixed — changes that can affect results or their interpretation

- **Imports:** SPSS user-missing values and labels no longer take inconsistent
  analysis paths merely because data arrive as SAV, RDS/RData or Parquet.
  Lossy conversions and ambiguous source collisions fail rather than silently
  reusing a same-named dataset. See the [import contract](references/import-contract.md).
- **ANOVA:** repeated-measures subject IDs now define factor error strata;
  numeric IDs previously produced incorrect tests/df. Sphericity uses the
  within-occasion contrast space and appropriate residual covariance. Corrected
  inference, requested Tukey confidence levels and vector-valued contrast
  p-adjustments are preserved. The retained repeated-measures adapter is
  sequential, not general Type-II/III repeated-measures support.
  [ANOVA details](references/subskills/anova.md)
- **Rank tests:** Mann–Whitney no longer subtracts the rank offset twice;
  U and associated rank effects can differ materially from old output.
  The previously named `epsilon_sq` formula is identified as `eta_H_sq`,
  with the old alias's meaning disclosed. Location/pseudomedian intervals are
  distinguished from effect-size and median-difference intervals.
  [Rank-test details](references/subskills/nonparametric.md)
- **Linear mixed models:** requested sums-of-squares type and Satterthwaite/
  Kenward–Roger inference reach the actual tests and marginal-mean comparisons;
  adjusted contrast p-values are no longer recycled from a single scalar.
  Optimizer limits/status and unavailable diagnostics are reported honestly.
  [Mixed-model details](references/subskills/mixed-models.md)
- **Correlations and t-tests:** partial-correlation inference accounts for
  the effective control rank; one-sided intervals follow the requested
  alternative. Group identity, paired direction and analyzed cases are retained.
  [Correlations](references/subskills/correlations.md), [t-tests](references/subskills/t-test.md)
- **Reliability and scale summaries:** requested ICC agreement/consistency
  definitions remain distinct; grouped case/missing counts and categorical
  order are preserved rather than inferred from display labels.
  [Reliability](references/subskills/reliability.md), [scale analysis](references/subskills/scale.md)
- **EFA:** oblique communalities incorporate factor correlations; factor-model
  explained variance no longer substitutes PCA quantities. Selected rotations
  and ordered category codes reach the intended package calculations.
  [EFA details](references/subskills/efa.md)
- **SEM:** reported groups retain lavaan's actual group identity; exact option
  matching prevents collisions such as `seed`/`se`. Requested CI/bootstrap
  settings and diagnostic availability are distinguished from effective output.
  [SEM details](references/subskills/sem.md)
- **Diagnostics:** regression curvature checks no longer treat the built-in
  orthogonality of OLS residuals as a linearity test. Sphericity tests the
  relevant contrast covariance. Skipped/unavailable checks are not passes.
  [Assumption checks](references/subskills/assumptions.md)
- **Power:** one-sided effect direction and the specified SEM RMSEA null and
  alternative affect calculations; pilot groups/design rank and rounded-sample
  attained power are recorded. [Power details](references/subskills/power.md)
- **Data handling and presentation:** grouped counts, category identity,
  transformation/recode provenance and original imputation columns are preserved;
  non-estimable requested results and failed runs are explicitly identified.
- Installed operation paths containing spaces now use the shared base-R
  launcher, including replay subprocesses, without a separate argument parser.

### Removed

- Duplicated per-module infrastructure fallbacks and silent substitute import/
  graphics routes; no statistical family was removed to simplify packaging.
- Mandatory metaskill activation/finalization ceremony for ordinary research
  and report delivery, and the current-project parallel JSONL/backup layout.

### Support and known limits

Linux workflows were exercised in Codex CLI, Claude Code, Copilot, Vibe and
Antigravity; the latter remains qualified by documented scope/context problems.
Codex IDE and App had limited activation checks. Native Windows/macOS are
prepared, not live verified. Some historical test runners still assume retired
project layouts; no fully green legacy suite or universal model correctness is
claimed. See the [exact support matrix](references/installation.md#acceptance-status--16-september-2026).
Lambda-Star and LaTeX output are not part of this release.

## [1.0.2z] - 2026-01-07

Historical published baseline for the changes above. Its original release notes
and source archive remain available on GitHub; no detailed retroactive history
has been invented here. The earlier `1.0.2` tag identifies the same commit.

[Unreleased]: https://github.com/docmh/nlss/compare/v2.0.0...HEAD
[2.0.0]: https://github.com/docmh/nlss/compare/1.0.2z...v2.0.0
[1.0.2z]: https://github.com/docmh/nlss/releases/tag/1.0.2z
