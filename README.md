# NLSS — Natural Language Statistics Suite

**Ask research questions in your own words. Run established statistics in R.
Keep the evidence.**

NLSS turns your local AI coding agent into a statistical research assistant.
Describe your study, discuss an analysis, inspect the results, and develop a
scientific report through conversation.

Built for researchers in **psychology, the social sciences, and economics**,
NLSS combines an agent that understands context and conversation with an R
backend that executes explicit statistical procedures. You remain the
researcher: consequential choices are discussed, assumptions stay visible,
and interpretations can be checked against the actual results.

> “These questionnaire items measure well-being. Inspect their coding and
> missing values, assess the scale, and help me test whether well-being differs
> between the groups. Explain effect sizes and uncertainty alongside p-values.”

Begin with a question like this, refine the analysis together, and ask
“Why that method?” or “What does this mean for my hypothesis?” at any point.

[Get started](#get-started) · [Analyses](#what-you-can-analyze) ·
[Your data and results](#your-data-and-results) · [Reports](#reports-that-understand-your-study) ·
[Installation guide](references/installation.md) ·
[Changelog](https://github.com/docmh/nlss/blob/main/CHANGELOG.md)

## Why Work With NLSS?

- **A conversation, backed by R.** The agent interprets your intention and
  explains findings; established R procedures calculate the statistics.
  Methods, options and diagnostic results are inspectable.
- **A familiar evidence trail.** A cumulative analysis protocol in Markdown
  grows as you work, with tables, diagnostics and links to figures. It is
  available by default in your project folder.
- **Your project stays yours.** Work in an ordinary research folder. Original
  datasets stay untouched; working data and authored reports remain visible
  and accessible in your own tools.
- **Reports grounded in your study.** Supply study context, discuss
  competing explanations, compare selected findings, or request a full report.
  The agent shapes the report around your question, evidence and audience.
- **A record you can return to.** Saved runs retain inputs, settings, results
  and artifacts. Delivered report revisions link the evidence used.
  Verification and supported replay are explicit operations.

## Get Started

You need a **local agent that can read files and run commands**, access to an
appropriate model, and **R available as `Rscript` in that agent's environment**.
NLSS runs through a terminal or an IDE; its runtime is R. Your chosen agent
may have additional prerequisites.

### 1. Give Your Agent the Repository Link

> “Install NLSS from https://github.com/docmh/nlss for this agent.
> Follow its installation guide and ask before installing missing software.”

The [installation guide](references/installation.md) explains how to select a
release and the appropriate plugin or standalone skill. Install once for your
chosen agent and reuse that installation across research projects. Start a new
session when prompted.

**Current Release: [2.0.1](https://github.com/docmh/nlss/releases/tag/v2.0.1).**
Follow the installation guide to select the matching plugin or standalone-skill
package. [All Releases](https://github.com/docmh/nlss/releases)

### 2. Open Your Research Folder and Ask a Question

> “Use NLSS with `survey.sav`. Describe the sample and show me missing values
> for the variables we need to test H1.”

NLSS supports **CSV, SAV (`.sav`), RDS, RData and Parquet**. The agent handles
project initialization and import, preserving the original and creating a
separate working dataset. It creates the required project infrastructure and
handles format conversion, asking you about consequential ambiguities.

R packages are checked **when the selected operation needs them**. The agent
asks before installing missing requirements, keeping setup focused on the
packages needed for your selected analysis.

### 3. Inspect and Discuss Your Results

Keep **`report_canonical.md` in your project root** open while working. Ask for
a figure, an explanation, another analysis, or a report at the depth you need.

To explore NLSS with sample data, ask:

> “Run the NLSS demo in a separate folder to show me what it can do.”

The release includes sample data. Power planning also supports direct entry of
design parameters and effect sizes.

### Choose Your Agent

| Environment | NLSS Delivery |
| --- | --- |
| Codex CLI and supported local desktop | Plugin |
| Codex IDE extension | Standalone skill |
| Claude Code | Plugin; standalone fallback available |
| VS Code with GitHub Copilot | Plugin through local plugin registration |
| Google Antigravity | Plugin through the selected CLI or desktop route |
| Mistral Vibe Code | Standalone skill |

These routes share **one set of scientific instructions and R code**. Local
Linux workflows have been exercised; Antigravity has documented limitations.
Codex IDE/App checks were limited activation checks. Native Windows/macOS routes
are prepared, with live verification pending. The [exact support matrix](references/installation.md#acceptance-status--16-september-2026)
records tested versions, routes and limitations.

## What Research With NLSS Looks Like

Start with your design and the question, then build the analysis through
conversation. The agent selects the relevant NLSS procedures with you.

| Your Question | A Useful Next Request |
| --- | --- |
| “What is in this dataset?” | “Show variable labels, coding, distributions and missingness.” |
| “Does the outcome differ between conditions?” | “Account for the design, report effect sizes and confidence intervals, and check the relevant assumptions.” |
| “Do these items form a defensible scale?” | “Check coding and dimensionality; distinguish reliability from validity.” |
| “Does adding these predictors help?” | “Compare the selected models and make differences in analyzed samples explicit.” |
| “What can I conclude?” | “Explain the finding in the context of the design, including alternative explanations and limitations.” |
| “I need to write this up.” | “Draft a Results section from these analyses, using my study description.” |

Give the agent any relevant study document, hypotheses or source literature.
A file such as `research_note.md` is an ordinary study document; you choose its
name and contents. Be explicit about consequential choices such as variable
roles, score construction and missing-data handling.

## What You Can Analyze

Choose procedures that fit your study design. The method references give exact
options, assumptions and supported variants.

| Area | Procedures and References |
| --- | --- |
| Get to know the data | [Descriptive statistics](references/subskills/descriptive-stats.md), [frequencies](references/subskills/frequencies.md), [cross-tabulations](references/subskills/crosstabs.md), [data explorer](references/subskills/data-explorer.md) |
| Relationships and group differences | [Correlations](references/subskills/correlations.md) including partial/group comparisons; [t-tests](references/subskills/t-test.md); [ANOVA/ANCOVA](references/subskills/anova.md) including repeated-measures designs; [rank-based tests](references/subskills/nonparametric.md) |
| Statistical models | [Linear/logistic/Poisson regression](references/subskills/regression.md), [linear mixed models](references/subskills/mixed-models.md), [SEM/CFA, mediation and invariance](references/subskills/sem.md) |
| Psychometric instruments | [EFA/PCA](references/subskills/efa.md), [scale analysis](references/subskills/scale.md) including alpha/omega, [ICC/kappa/test-retest reliability](references/subskills/reliability.md) |
| Prepare and scrutinize data | [Transformations](references/subskills/data-transform.md), [missingness](references/subskills/missings.md), [imputation](references/subskills/impute.md), [assumption checks](references/subskills/assumptions.md) |
| Multiple-imputation inference | [Pooled regression](references/subskills/mi-regression.md) for supported Gaussian, binomial and Poisson models from preserved mice imputations |
| Plan and communicate | [Power analysis](references/subskills/power.md), including dataset-free planning; [figures](references/subskills/plot.md), with nine plot types |

Repeated-measures ANOVA uses the documented sequential `aov(Error(...))`
approach; mixed models offer a separate modeling route. For multiple-imputation
inference, use the pooled-regression procedure with preserved imputation sets.
A single completed or averaged dataset does not provide pooled inference.
Consult the linked references for the scope of each method.

### Research Workflows

NLSS also guides [sample descriptions](references/metaskills/describe-sample.md),
[data exploration](references/metaskills/explore-data.md),
[screening](references/metaskills/screen-data.md),
[preparation](references/metaskills/prepare-data.md),
[assumption checking](references/metaskills/check-assumptions.md),
[hypothesis testing](references/metaskills/test-hypotheses.md),
[instrument assessment](references/metaskills/check-instruments.md) and
[power planning](references/metaskills/plan-power.md).

For writing and understanding, use ordinary requests for
[result interpretation](references/metaskills/explain-results.md),
[statistical explanations](references/metaskills/explain-statistics.md),
[full reports](references/metaskills/write-full-report.md), or
[document formatting](references/metaskills/format-document.md).
[The demo](references/metaskills/run-demo.md) introduces the workflow.
[Custom R generation](references/metaskills/generate-r-script.md) is an
explicitly approved last resort for work outside existing procedure coverage.

Utilities cover [calculations](references/utilities/calc.md),
[literature retrieval](references/utilities/research-academia.md),
[project setup](references/utilities/project-create.md),
[inspection](references/utilities/project-inspect.md),
[report preservation](references/utilities/project-report.md) and
[verified replay](references/utilities/replay-run.md).
Historical [log checks](references/utilities/check-integrity.md) and
[report reconstruction](references/utilities/reconstruct-reports.md) remain
available for their documented historical inputs.

## Your Data and Results

An NLSS project is your research folder with two **required infrastructure
components**, created and managed by NLSS:

- **`nlss-workspace.yml`** identifies the project and registers its datasets
  and working-data paths.
- **`.nlss/`** stores preserved data versions, analysis runs, artifacts and
  report evidence used for traceability and replay.

**Both are necessary for NLSS's project workflow to operate correctly.** Keep
their names and contents intact, and include both when moving or backing up the
project. Let NLSS manage them while you work with the visible datasets, protocol
and reports. Deleting or manually changing this infrastructure can break dataset
resolution and access to saved evidence.

One example — visible data/report names are your choice:

```text
my-study/
  survey.sav                 original source, untouched by NLSS
  research_note.md           optional study context supplied by you
  nlss-workspace.yml         required project and dataset registration
  data/
    survey_working.parquet   visible, editable working data
  report_canonical.md        automatic cumulative analysis protocol
  Study-report.md            optional, freely authored report
  .nlss/                     required data versions, runs and report evidence
```

- **Originals and working data are separate.** Registered working data remain
  outside `.nlss/`; you can inspect or edit them with suitable tools. Source
  bytes and analysis-input versions are preserved for the recorded evidence.
- **Import meaning matters.** Shared readers handle variable/value labels and
  user-defined missing values consistently; CSV has explicit encoding, decimal,
  column-type and missing-value options. Measurement levels are selected as
  analytical decisions. See the [import contract](references/import-contract.md).
- **The protocol is always available after execution.** Runs write numerical
  results, deterministic Markdown and figures through the shared publisher,
  which automatically extends the root protocol.
- **Browse before rerunning.** Ask “Show the saved results in this project.”
  Inspection provides a read-only view linking data, runs and reports, with
  explicit labels for missing or incomplete evidence. Verification and replay
  are available as separate, requested operations.
- **Keep the whole project together.** Relative references allow moving the
  project as a whole. Keep individual registered files at their recorded paths
  so NLSS can resolve them reliably. Identical evidence bytes share storage objects.

Each statistical run has `request.json`, `result.json` and `output.md` beneath
`.nlss/runs/`; figures stay with that run. Authored-report revisions link their
selected evidence separately.

**Coming from an older NLSS project?** Version 2 uses a different project
layout and does not convert older project folders. Begin a new project from a
deliberately selected source dataset, keeping
the old project, its history and its original software together for reference.
Review the [breaking changes and numerical corrections](https://github.com/docmh/nlss/blob/main/CHANGELOG.md)
before comparing old and new results.

## Reports That Understand Your Study

The automatic protocol answers **“What was run, and what did R return?”**
A scientific report also asks **“What does that mean for this study?”**

NLSS keeps those responsibilities distinct. You can request a short answer,
a Methods or Results section, a comparison of selected models, or a full
research report. The agent considers your design, instruments, hypotheses,
sampling, uncertainty and limitations. It checks numerical claims against the
actual evidence and makes missing study details or diagnostic results explicit.

Reports are **freely authored, editable Markdown** with NLSS/APA-inspired
presentation, organized for your question and audience.
When you request a delivered report, it is preserved as a revision alongside
references to the analysis evidence used. Follow-up explanations draw on existing
results; you choose when to request another analysis or saved revision.

When literature research is needed, the agent must actively search, read and
assess sources, prioritizing established citable scholarship, current reviews
and meta-analyses, and well-executed relevant primary studies: **quality first,
quantity second**. The depth of the search follows the question, and adequate
supplied literature can be reused. Source appraisal guides the selection.
[Literature guidance](references/utilities/research-academia.md#agentic-literature-research-required)

Bring your own edits back for discussion. Formatting preserves scientific
meaning, while you remain responsible for reviewing the interpretation and
conclusions. LaTeX with an APA 7 template is a potential future presentation option.

## Installation Instructions for Agents

For an installation request, read **[Installation and maintenance](references/installation.md)**
and follow the route for the user's actual harness and environment. Resolve a
published release, retain its exact version, check its assets, and use the
existing native manager or standalone installer. Install one selected route per
harness at a stable location shared across the user's research projects.

The guide is the single installation authority and covers permissions,
dependencies, updates, removal and rollback. If required assets are unavailable,
report that and ask the user how to proceed. An installation request covers
installation; project creation and analysis follow the user's research request.

For research work, use the installed [SKILL.md](SKILL.md). For development,
read [AGENTS.md](AGENTS.md). These public documents provide the onboarding guidance.

## Advanced Use and Customization

Researchers can use natural language throughout. For scripts or troubleshooting,
the same operations are available through the existing launcher:

```text
Rscript "<installed-skill>/scripts/R/run_nlss.R" project-create --project "<study folder>" --source "<source file>"
Rscript "<installed-skill>/scripts/R/run_nlss.R" descriptive-stats --project "<study folder>" --dataset "<returned name>" --vars age,score
Rscript "<installed-skill>/scripts/R/run_nlss.R" project-inspect --project "<study folder>"
```

Use the dataset name returned by project creation. For a selected operation,
`--help` lists its options; procedure references document statistical choices.
Use the launcher with quoted paths for installed operations, including replay.
The standalone installation helper is the documented exception.

### Configuration and Output Formatting

Defaults and their validated types live in `scripts/config.yml`. Keep personal
overrides outside the installation and set `NLSS_CONFIG_PATH` to that YAML file;
CLI options take precedence. For example:

```yaml
defaults:
  digits: 3
modules:
  crosstabs:
    percent: "column"
```

Deterministic analysis tables use the templates in `assets/`. Register a custom
template through the existing configuration. The LLM independently shapes its
reports and short answers around the research question.

Saved requests capture execution evidence and supported seeded calculations.
[Replay](references/utilities/replay-run.md) verifies its required code, inputs
and environment before recomputing, keeping replay output separate from the
current working dataset. Provide the recorded R environment for historical
replay; rerunning with corrected software is a separate analysis.

## Troubleshooting

| Symptom | First Check |
| --- | --- |
| The agent cannot find R | Run `Rscript --version` in the agent's environment. Check the actual R installation and PATH; WSL and native Windows are separate environments. Ask before changing host configuration. |
| Missing or unloadable R package | Use the [dependency resolver](references/utilities/dependency-resolver.md); approve only the required installation/replacements. System libraries or compilers require a separate decision. |
| NLSS does not appear | Check the selected [installation route](references/installation.md), location and enabled state, then start a new agent session. |
| A dataset cannot be found | Confirm the selected research folder and source path; use an absolute path to resolve ambiguity. |
| Literature search is unavailable | Check the selected agent's network permissions and the retrieval service; distinguish supplied sources from a live search. |
| A result looks unexpected | Inspect the saved request/results and variable coding. Review the analyzed cases, assumptions and method before drawing conclusions. |

If needed, ask “Help me troubleshoot NLSS” or
[open an issue](https://github.com/docmh/nlss/issues) with the release version,
environment and a minimal non-sensitive example. Use synthetic or anonymized
data and redact credentials and private project details before sharing.

## For Developers

NLSS follows the [Agent Skills standard](https://agentskills.io/specification).
The plugin and standalone skill contain the same R runtime and guidance.
Statistical procedures use shared execution, import, publication and project
management infrastructure.

- [SKILL.md](SKILL.md): research workflow and capability reference.
- [AGENTS.md](AGENTS.md): contribution and implementation conventions.
- [Run contract](references/run-contract.md), [utility contract](references/utility-contract.md)
  and [import contract](references/import-contract.md): technical boundaries.
- [Source tests](https://github.com/docmh/nlss/tree/main/tests) and
  [test inventory](https://github.com/docmh/nlss/blob/main/tests/tests.yml):
  source-checkout checks and independent numerical references.
- [Release checklist](https://github.com/docmh/nlss/blob/main/packaging/RELEASE_CHECKLIST.md):
  build, versioning and publication. Python 3 is maintainer-only.

From a source checkout, select checks from `tests/tests.yml`. For example,
`bash cmdscripts/tests.sh phase3` exercises current project contracts;
`python3 tests/phase5/run_packaging_tests.py --root <test-folder> --keep 0`
checks packaging and standalone maintenance without personal registrations.
The documented PowerShell entrypoint supports native Windows test execution;
live platform verification status is recorded in the installation guide.

Use targeted checks for the changed boundary and retain independent numerical
coverage. Some historical smoke runners target older project layouts; select
tests appropriate to the current contract and distinguish setup failures from
method failures. Tests and detailed references describe their actual scope.

NLSS was developed with AI assistance and maintainer review. Contributions
should preserve scientific capabilities, transparent evidence and simple use.
See [contribution guidance](https://github.com/docmh/nlss/blob/main/CONTRIBUTING.md).

## License, Privacy, and Citation

NLSS is licensed under [Apache-2.0](LICENSE); see [NOTICE](NOTICE).
NLSS™ is a trademark of Mike Hammes; see the
[trademark notice](https://github.com/docmh/nlss/blob/main/TRADEMARKS.md).
R packages are installed separately under their own licenses.

Third-party product names and trademarks mentioned here belong to their
respective owners. They identify compatible tools and formats. NLSS is an
independently developed project.

R runs locally, but **your chosen agent/model provider may process the context
you share**. Review its privacy, retention and permission settings before using
sensitive data. R execution and the agent's data-processing arrangements are
separate considerations.
The software is provided without warranties; researchers remain responsible
for methodological decisions and validation of conclusions.

For publications, cite Mike Hammes, *NLSS — Natural Language Statistics Suite*,
with the **version actually used** and its release URL. Machine-readable metadata
are in [CITATION.cff](https://github.com/docmh/nlss/blob/main/CITATION.cff).
The existing [Zenodo concept DOI](https://doi.org/10.5281/zenodo.18173833)
identifies the software across archived versions. Cite the specific archived
version when a corresponding version DOI is available.

Maintainer: Mike Hammes — mike.hammes@mikehammes.name
