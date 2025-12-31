# NLSS™ — Natural Language Statistics Suite

NLSS is a workspace-first set of R scripts designed to help **researchers** run common analyses quickly—primarily **through a coding agent in an IDE** (e.g., Codex or Claude Code). These third-party tools are mentioned as recognizable examples, not endorsements or recommendations.

NLSS is packaged as an **Agent Skill** following the open **Agent Skills** standard: https://agentskills.io/specification. This repo’s `SKILL.md` is the entry point that agents use to discover what NLSS can do.

NLSS produces:

- `report_canonical.md`: human-readable, NLSS format-ready tables + narrative (append-only audit trail)
- `analysis_log.jsonl`: machine-readable JSONL run log (append-only)
- `<dataset>.parquet`: a stateful workspace copy of your dataset (used by all modules)

You can run NLSS either:

- **via an Agent IDE (recommended):** prompt your agent to use the `nlss` skill and work inside an NLSS workspace, or
- **via terminal (secondary):** run the R scripts directly with `Rscript`.

---

## Part I — For Researchers (Get Running Quickly)

### 0) Install NLSS as a skill in your agent (Codex or Claude Code)

Agent Skills are installed by placing the skill folder in the tool’s skill directory (the folder must contain `SKILL.md`, and the folder name should match `name: nlss` in `SKILL.md`). Codex and Claude Code are examples; any Agent Skills–compatible agent host can work.

**Codex (OpenAI; example agent host)**

- **User-wide (recommended):** clone NLSS into your Codex skills folder, e.g. `~/.codex/skills/nlss/` (macOS/Linux/WSL).
- **Per-repo (team/shared):** put NLSS under your project’s `.codex/skills/nlss/`.
- See Codex skills docs for supported locations and discovery behavior: https://developers.openai.com/codex/skills
- In Codex, run `/skills` to confirm NLSS is installed, and use `$nlss` for explicit invocation when needed.
- In Codex, you can also install skills from inside the chat via `$skill-installer` (restart required after installing).

**Claude Code (Anthropic; example agent host)**

- **Personal skills:** `~/.claude/skills/nlss/` (available across projects).
- **Project skills:** `.claude/skills/nlss/` (committed to your repo and shared with your team).
- **Plugin skills:** bundled with installed Claude Code plugins.
- Docs: https://docs.claude.com/en/docs/claude-code/skills (overview: https://www.claude.com/blog/skills)

If you want the quickest path: ask your agent to install NLSS from your repo URL into its skills directory.
After installing, restart Codex / Claude Code so the new skill is loaded.

<details>
<summary>Example installs (macOS/Linux/WSL)</summary>

Codex user-wide:

```bash
mkdir -p ~/.codex/skills
git clone <repo-url> ~/.codex/skills/nlss
```

Claude Code manual install:

```bash
mkdir -p ~/.claude/skills
git clone <repo-url> ~/.claude/skills/nlss
```

Claude Code project skill (run in your analysis repo root):

```bash
mkdir -p .claude/skills
git clone <repo-url> .claude/skills/nlss
```

</details>

### 1) Install an Agent IDE + plugin (Codex or Claude Code)

Pick one setup:

- Install an IDE that can host agentic tooling (for example Visual Studio Code or Cursor). These are examples only—use whatever IDE/agent setup your environment supports.
- Install either **Codex** (OpenAI) or **Claude Code** (Anthropic) and make sure it can run shell commands in your project.

### 2) Install R so `Rscript` works in the agent terminal

NLSS runs `.R` scripts directly with `Rscript`, even when invoked by an agent. Verify it in the *same* shell your agent uses:

- Windows PowerShell: `Get-Command Rscript` or `Rscript --version`
- WSL/Linux (bash): `which Rscript` or `Rscript --version`

Install R (examples):

- Windows (PowerShell): install R (CRAN or `winget install --id RProject.R -e`) and ensure it is on `PATH`.
- WSL (Ubuntu): `sudo apt update && sudo apt install r-base`

### 3) Install R package dependencies (full list, once)

NLSS uses base R packages (`base`, `stats`, `utils`, `graphics`, `grDevices`, `tools`) plus the following CRAN packages. Install all to avoid missing features:

`arrow`, `car`, `curl`, `DHARMa`, `emmeans`, `foreign`, `ggplot2`, `haven`, `influence.ME`, `jsonlite`, `lavaan`, `lme4`, `lmerTest`, `mice`, `MVN`, `performance`, `psych`, `pwr`, `semPower`, `VIM`, `viridisLite`, `yaml`

```bash
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('arrow','car','curl','DHARMa','emmeans','foreign','ggplot2','haven','influence.ME','jsonlite','lavaan','lme4','lmerTest','mice','MVN','performance','psych','pwr','semPower','VIM','viridisLite','yaml'))"
```

### 4) Fastest onboarding: ask for the `run-demo` metaskill

NLSS includes a guided onboarding metaskill called `run-demo`. It explains the workflow, then (with your permission) sets up a demo workspace using the bundled sample dataset.

Try a natural language prompt like:

- "Run the `run-demo` metaskill to onboard me to NLSS."
- "Show me what NLSS can do and set up a demo workspace."
- "Give me a quick NLSS walkthrough, then prepare the demo."

The demo uses `assets/sample-data/golden_dataset.csv` by default and will ask before creating any workspace files.

### 5) First example (agent-first): analyze the bundled golden dataset

This repo ships a small demo dataset: [tests/data/golden_dataset.csv](tests/data/golden_dataset.csv).

1) From the NLSS repo folder (the one containing `SKILL.md`), create a working folder and copy the dataset (you can do this yourself, or ask your agent to do it):

```bash
mkdir -p demo
cp tests/data/golden_dataset.csv demo/golden_dataset.csv
```

2) Prompt your agent with natural language, for example:

- "Use `nlss` to initialize a workspace for `demo/golden_dataset.csv`, then run descriptive stats for `age` and `score` grouped by `condition`."
- "Run a `gender` × `condition` crosstab with chi-square + effect sizes, then interpret."
- "Run Spearman correlations among `age`, `score`, and `stress`, and summarize the strongest relationships."

3) Open the outputs in:

- `nlss-workspace/golden_dataset/report_canonical.md`
- `nlss-workspace/golden_dataset/analysis_log.jsonl`

<details>
<summary>Terminal fallback (run the scripts yourself)</summary>

From the repo root:

```bash
cd demo
Rscript ../scripts/R/init_workspace.R --csv golden_dataset.csv
Rscript ../scripts/R/descriptive_stats.R --csv golden_dataset.csv --vars age,score --group condition
Rscript ../scripts/R/frequencies.R --csv golden_dataset.csv --vars gender,condition
Rscript ../scripts/R/crosstabs.R --csv golden_dataset.csv --row gender --col condition
Rscript ../scripts/R/correlations.R --csv golden_dataset.csv --vars age,score,stress --method spearman
```

</details>

### 6) The workspace structure (what NLSS creates)

By default, NLSS writes into `./nlss-workspace/` (see Part II to change this). A typical first-run folder looks like:

```text
demo/
  golden_dataset.csv
  nlss-workspace/
    nlss-workspace.yml
    golden_dataset/
      golden_dataset.parquet
      scratchpad.md
      report_canonical.md
      analysis_log.jsonl
      plots/
      backup/
      report_YYYYMMDD_<metaskill>_<intent>.md  (only when a metaskill produces a full report)
```

What the files mean:

- `nlss-workspace.yml`: workspace manifest (datasets, active dataset, log sequence counters).
- `<dataset>.parquet`: the working copy *every module reads*; some modules update it in place (with backups).
- `scratchpad.md`: planning, decisions, assumptions (especially for agent-run metaskills).
- `report_canonical.md`: append-only “lab notebook” of NLSS tables + narrative for each run.
- `analysis_log.jsonl`: append-only JSONL log entries for audit/reproducibility.
- `backup/`: timestamped parquet backups created before destructive updates.
- `plots/`: saved figures (from `plot` and some model modules).

### 7) How output + logging works

- **Subskills** append sections to `report_canonical.md` and (by default) append one JSON line to `analysis_log.jsonl`.
- **Metaskills** (agent-run workflows) may additionally create a standalone, journal-ready file `report_<YYYYMMDD>_<metaskill>_<intent>.md` in the dataset folder.
- Output directories are intentionally not a per-run CLI flag; choose a workspace root by where you run, or by changing `defaults.output_dir` (Part II).

### 8) Easy prompts to give your agent (copy/paste)

Try these in Codex / Claude Code:

1. "Use `nlss` to initialize a workspace for `demo/golden_dataset.csv` and tell me which files were created."
2. "Run descriptive stats for `age` and `score`, grouped by `condition`, and interpret the main differences."
3. "Run a `gender` × `condition` crosstab with chi-square + effect sizes; interpret."
4. "Run Spearman correlations among `age`, `score`, and `stress`; summarize and note caveats."

---

## Part II — For Developers / Maintainers

### Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-ready report.

### Agent Skills standard + installation (framework details)

NLSS follows the **Agent Skills** standard (https://agentskills.io/specification). In practice, that means:

- A skill is a **folder** containing `SKILL.md` (YAML front matter with `name`/`description` + human-readable docs).
- The skill may also ship `references/` (how-to docs), `scripts/` (runnables), and `assets/` (templates).

How you install skills depends on the agent. Examples:

- **Codex (OpenAI):** Codex discovers skills by scanning these locations (in order): `./.codex/skills`, `../.codex/skills`, `$REPO_ROOT/.codex/skills`, `$CODEX_HOME/skills`, `/etc/codex/skills`, `/usr/share/codex/skills`. Install NLSS by placing it in one of those locations under a folder named `nlss/` (matching `name: nlss`). Docs: https://developers.openai.com/codex/skills
- **Claude Code (Anthropic):** Claude Code loads skills from project skills (`.claude/skills/`), personal skills (`~/.claude/skills/`), and plugin-bundled skills. Docs: https://docs.claude.com/en/docs/claude-code/skills

### Requirements and system support

- R 4.4+.
- `Rscript` available on `PATH` in the shell where you run NLSS.
- Base R packages used: `base`, `stats`, `utils`, `graphics`, `grDevices`, `tools`.
- CRAN dependencies used across modules: `arrow`, `car`, `curl`, `DHARMa`, `emmeans`, `foreign`, `ggplot2`, `haven`, `influence.ME`, `jsonlite`, `lavaan`, `lme4`, `lmerTest`, `mice`, `MVN`, `performance`, `psych`, `pwr`, `semPower`, `VIM`, `viridisLite`, `yaml`.
- Windows, WSL (Ubuntu), or Linux.

### Path handling

- Displayed paths in console output and reports default to workspace-relative when the target is inside the workspace root; absolute paths are used only for locations outside the workspace.
- Mask workspace-external paths in `scratchpad.md`, `report_canonical.md`, and `analysis_log.jsonl` as `<external>/<filename>`.
- Prefer workspace-relative paths in examples and configs; use absolute paths only when referencing files outside the workspace root.

### Stateful workspace architecture

NLSS is stateful. The workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml`; if no manifest is present, scripts fall back to `defaults.output_dir` in `scripts/config.yml` (default: `./nlss-workspace`).

- `scripts/R/init_workspace.R` creates `<workspace-root>/<dataset-name>/scratchpad.md`, `report_canonical.md`, `analysis_log.jsonl`, a parquet workspace copy, and `nlss-workspace.yml` in the workspace root.
- For any input dataset (CSV/SAV/RDS/RData/Parquet), the workspace copy lives at `<workspace-root>/<dataset-name>/<dataset-name>.parquet`.
- All subskills read from the workspace parquet copy (prefer `--parquet` pointing to the workspace file or rely on auto-copy).
- When running from a dataset folder without input flags, scripts select that dataset; otherwise they load `active_dataset` from the manifest.
- Workspaces must be non-nested and unique per parent folder; scripts stop if nested or sibling workspace manifests are detected.
- `data-transform` and `missings` update the workspace parquet copy in place and save a backup to `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting.

### Available modules (subskills)

Each subskill has a reference file describing inputs, flags, and outputs. Template-driven modules can be customized via `assets/<subskill>/` and `templates.*` in `scripts/config.yml`.

| Subskill | Script | Purpose | NLSS format template |
| --- | --- | --- | --- |
| `descriptive-stats` | `scripts/R/descriptive_stats.R` | Descriptive statistics with NLSS format tables/text. | Yes (`descriptive-stats/default-template.md`, `descriptive-stats/robust-template.md`, `descriptive-stats/distribution-template.md`) |
| `frequencies` | `scripts/R/frequencies.R` | Frequency tables for categorical variables. | Yes (`frequencies/default-template.md`, `frequencies/grouped-template.md`) |
| `crosstabs` | `scripts/R/crosstabs.R` | Cross-tabulations with chi-square/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
| `correlations` | `scripts/R/correlations.R` | Correlations, partial correlations, diagnostics, bootstrap CIs, Fisher r-to-z comparisons. | Yes (`correlations/default-template.md`, `correlations/cross-correlation-template.md`, `correlations/matrix-template.md`, `correlations/comparison-template.md`) |
| `scale` | `scripts/R/scale.R` | Item analysis and reliability (alpha/omega) for scales. | Yes (`scale/default-template.md`) |
| `efa` | `scripts/R/efa.R` | Exploratory factor analysis / PCA with rotation, KMO/Bartlett diagnostics, and loadings tables. | Yes (`efa/default-template.md`) |
| `reliability` | `scripts/R/reliability.R` | Inter-rater/test-retest reliability (ICC, kappa, correlations). | Yes (`reliability/default-template.md`) |
| `data-explorer` | `scripts/R/data_explorer.R` | Data dictionary exploration with missingness and level summaries. | Yes (`data-explorer/default-template.md`) |
| `plot` | `scripts/R/plot.R` | NLSS format-ready figures (histogram, bar, box/violin, scatter/line, QQ, correlation heatmaps). | Yes (`plot/default-template.md`) |
| `data-transform` | `scripts/R/data_transform.R` | Derived variables, recoding, binning, renaming, and drop operations. | Yes (`data-transform/default-template.md`) |
| `missings` | `scripts/R/missings.R` | Missing-data patterns, handling decisions, and transformed datasets. | Yes (`missings/default-template.md`) |
| `impute` | `scripts/R/impute.R` | Imputation into new columns with optional mice/VIM engines. | Yes (`impute/default-template.md`) |
| `assumptions` | `scripts/R/assumptions.R` | Assumption checks for t-tests, ANOVA, regression, mixed models, and SEM. | Yes (`assumptions/ttest-template.md`, `assumptions/anova-template.md`, `assumptions/regression-template.md`, `assumptions/mixed-models-template.md`, `assumptions/sem-template.md`) |
| `regression` | `scripts/R/regression.R` | Multiple and hierarchical regression (OLS/GLM) with interactions and bootstrap CIs. | Yes (`regression/default-template.md`) |
| `power` | `scripts/R/power.R` | Power analysis for t-tests, ANOVA, correlations, regression, and SEM. | Yes (`power/default-template.md`) |
| `mixed-models` | `scripts/R/mixed_models.R` | Linear mixed-effects models with random effects, marginal means, and planned contrasts. | Yes (`mixed-models/default-template.md`, `mixed-models/emmeans-template.md`) |
| `sem` | `scripts/R/sem.R` | Structural equation modeling (SEM), CFA, mediation, path analysis, invariance. | Yes (`sem/default-template.md`, `sem/cfa-template.md`, `sem/mediation-template.md`, `sem/invariance-template.md`) |
| `anova` | `scripts/R/anova.R` | Between-, within-, and mixed ANOVA with post-hoc comparisons and planned contrasts. | Yes (`anova/default-template.md`, `anova/posthoc-template.md`, `anova/contrasts-template.md`) |
| `t-test` | `scripts/R/t_test.R` | One-sample, independent-samples, and paired-samples t-tests. | Yes (`t-test/default-template.md`) |
| `nonparametric` | `scripts/R/nonparametric.R` | Wilcoxon, Mann-Whitney, Kruskal-Wallis, and Friedman tests. | Yes (`nonparametric/default-template.md`, `nonparametric/posthoc-template.md`) |
| `init-workspace` | `scripts/R/init_workspace.R` | Initialize workspace folder with scratchpad.md, NLSS format report, and .parquet copies. | Yes (`init-workspace/default-template.md`) |
| `metaskill-runner` | `scripts/R/metaskill_runner.R` | Log metaskill activations (intent + dataset) for traceability. | Yes (`metaskill-runner/default-template.md`, `metaskill-runner/finalization-template.md`) |

Metaskill specs live under `references/metaskills/` and are executed by the agent; use `metaskill-runner` to log activations and finalizations. Exception: `explain-statistics` and `explain-results` are conversational and do not require `metaskill-runner` or report outputs unless explicitly requested. Metaskill completion writes `report_<YYYYMMDD>_<metaskill>_<intent>.md` first, then logs finalization with `metaskill-runner --synopsis` to append a `# Synopsis` to `report_canonical.md` (the runner fails if the report is missing).

Default metaskill reports should follow `assets/metaskills/report-template.md` (NLSS format paper sections). Omit Introduction and Keywords when no theoretical context is available, adjust subsections as needed, and design tables/figures specifically for the report rather than copying from `report_canonical.md`.

Available metaskills:

- `explain-statistics`: `references/metaskills/explain-statistics.md`
- `explain-results`: `references/metaskills/explain-results.md`
- `run-demo`: `references/metaskills/run-demo.md`
- `format-document`: `references/metaskills/format-document.md`
- `describe-sample`: `references/metaskills/describe-sample.md`
- `generate-r-script`: `references/metaskills/generate-r-script.md`
- `check-instruments`: `references/metaskills/check-instruments.md`
- `plan-power`: `references/metaskills/plan-power.md`
- `explore-data`: `references/metaskills/explore-data.md`
- `prepare-data`: `references/metaskills/prepare-data.md`
- `screen-data`: `references/metaskills/screen-data.md`
- `check-assumptions`: `references/metaskills/check-assumptions.md`
- `test-hypotheses`: `references/metaskills/test-hypotheses.md`
- `write-full-report`: `references/metaskills/write-full-report.md`

Utilities:

- `calc`: `references/utilities/calc.md`
- `check-integrity`: `references/utilities/check-integrity.md`
- `reconstruct-reports`: `references/utilities/reconstruct-reports.md`
- `research-academia`: `references/utilities/research-academia.md`

Reference docs:

- `references/subskills/descriptive-stats.md`
- `references/subskills/frequencies.md`
- `references/subskills/crosstabs.md`
- `references/subskills/correlations.md`
- `references/subskills/scale.md`
- `references/subskills/efa.md`
- `references/subskills/reliability.md`
- `references/subskills/data-explorer.md`
- `references/subskills/plot.md`
- `references/subskills/data-transform.md`
- `references/subskills/missings.md`
- `references/subskills/impute.md`
- `references/subskills/assumptions.md`
- `references/subskills/regression.md`
- `references/subskills/power.md`
- `references/subskills/mixed-models.md`
- `references/subskills/sem.md`
- `references/subskills/anova.md`
- `references/subskills/t-test.md`
- `references/subskills/nonparametric.md`
- `references/subskills/init-workspace.md`
- `references/subskills/metaskill-runner.md`
- `references/metaskills/describe-sample.md`
- `references/metaskills/explain-statistics.md`
- `references/metaskills/explain-results.md`
- `references/metaskills/run-demo.md`
- `references/metaskills/format-document.md`
- `references/metaskills/generate-r-script.md`
- `references/metaskills/check-instruments.md`
- `references/metaskills/plan-power.md`
- `references/metaskills/explore-data.md`
- `references/metaskills/prepare-data.md`
- `references/metaskills/screen-data.md`
- `references/metaskills/check-assumptions.md`
- `references/metaskills/test-hypotheses.md`
- `references/metaskills/write-full-report.md`
- `references/metaskills/formatting/*.md`
- `references/utilities/calc.md`
- `references/utilities/check-integrity.md`
- `references/utilities/reconstruct-reports.md`
- `references/utilities/research-academia.md`

### Basic usage by module

Each run writes `report_canonical.md` in the output directory and appends to `analysis_log.jsonl` when logging is enabled.

### Descriptive Statistics

```bash
Rscript scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

### Frequencies

```bash
Rscript scripts/R/frequencies.R \
  --csv data.csv --vars gender,condition --group condition
```

### Cross-Tabulations

```bash
Rscript scripts/R/crosstabs.R \
  --csv data.csv --row gender --col condition --group site
```

### Correlations

```bash
Rscript scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

### Scale Analysis

```bash
Rscript scripts/R/scale.R \
  --csv data.csv --vars item1,item2,item3 --group condition
```

### Exploratory Factor Analysis

```bash
Rscript scripts/R/efa.R \
  --csv data.csv --vars item1,item2,item3 --method pca --rotation varimax
```

### Reliability Analysis

```bash
Rscript scripts/R/reliability.R \
  --csv data.csv --analysis icc --vars rater1,rater2,rater3
```

### Data Exploration

```bash
Rscript scripts/R/data_explorer.R \
  --csv data.csv --vars age,score --max-levels 15 --top-n 8
```

### Plots

```bash
Rscript scripts/R/plot.R \
  --csv data.csv --type scatter --x age --y score --group condition
```

### Data Transformation

```bash
Rscript scripts/R/data_transform.R \
  --csv data.csv --standardize age,score
```

### Missing Data Handling

```bash
Rscript scripts/R/missings.R \
  --csv data.csv --vars age,score,stress
```

### Metaskill Activation Logging

```bash
Rscript scripts/R/metaskill_runner.R \
  --csv data.csv --meta describe-sample --intent "describe the sample"
```

### Assumptions Checks

```bash
Rscript scripts/R/assumptions.R \
  --csv data.csv --analysis ttest --vars score --group condition
Rscript scripts/R/assumptions.R \
  --csv data.csv --analysis mixed_models --formula "score ~ time + (1|id)"
Rscript scripts/R/assumptions.R \
  --csv data.csv --analysis sem --factors "F1=item1,item2;F2=item3,item4"
```

### Nonparametric Tests

```bash
Rscript scripts/R/nonparametric.R \
  --csv data.csv --vars score --group condition --test mann_whitney
```

### Regression

```bash
Rscript scripts/R/regression.R \
  --csv data.csv --dv outcome --blocks "age,gender;stress,trait" --interactions stress:trait --center mean
```

### Power Analysis

```bash
Rscript scripts/R/power.R \
  --csv data.csv --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8
```

### Mixed Models

```bash
Rscript scripts/R/mixed_models.R \
  --csv data.csv --formula "score ~ time + (1|id)"
```

### SEM (Lavaan)

```bash
Rscript scripts/R/sem.R \
  --csv data.csv --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

### ANOVA

```bash
Rscript scripts/R/anova.R \
  --csv data.csv --dv outcome --between group
```

### T-Tests

```bash
Rscript scripts/R/t_test.R \
  --csv data.csv --vars score --group condition
```

### Workspace Initialization

```bash
Rscript scripts/R/init_workspace.R \
  --csv data.csv
```

### Where outputs go

All scripts write to the dataset workspace at `<workspace-root>/<dataset-name>/` and do not accept a custom output directory. Workspace root is the current directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir` in `scripts/config.yml`). Subskills only extend `report_canonical.md` and never create standalone report files; standalone `report_<YYYYMMDD>_<metaskill>_<intent>.md` files are created only by metaskills.
Workspace dataset copies are stored as `<workspace-root>/<dataset-name>/<dataset-name>.parquet`; `data_transform` and `missings` update these copies in place and create backups in `<workspace-root>/<dataset-name>/backup/`.
Plots are saved under `<workspace-root>/<dataset-name>/plots/` with figure-numbered filenames.

### Configuration

Defaults live in `scripts/config.yml` and are loaded via `scripts/R/lib/config.R`.

- `defaults.*` apply across all modules (for example `defaults.output_dir`, `defaults.workspace_manifest`).
- `modules.<subskill>.*` holds per-module defaults (for example `modules.crosstabs.percent`).
- `templates.<subskill>.*` controls the template file used for NLSS format outputs (see next section).
- CLI flags always override config values at runtime (for example `--digits`, module-specific flags).
- When `config.yml` is missing or unreadable, built-in defaults in `config.R` are used.

### Templates (NLSS format, YAML)

Templates are Markdown files under `assets/<subskill>/` with YAML front matter. They drive `report_canonical.md` output for the subskills that ship with templates (descriptive stats, frequencies, crosstabs, correlations, scale, data exploration, plotting, data transformation, missingness handling, imputation, assumptions, regression, power, SEM, ANOVA, and t-tests).

Key YAML fields:

- `tokens`: static strings you can reference as `{{token}}` in the template body.
- `table.columns`: ordered column specs with `key`, `label`, and optional `drop_if_empty`.
- `note.template`: controls `{{note_body}}` rendering (defaults to `{{note_default}}`).
- `narrative.template` or `narrative.row_template`: controls `{{narrative}}` rendering; `row_template` repeats over `narrative_rows` and can be joined with `narrative.join`.

Template paths can be overridden in `scripts/config.yml` under `templates.<subskill>.<name>` (for example `templates.crosstabs.grouped`). Edit the template files or point to your own to change NLSS format output without touching the R scripts.
You can also pass `--template <name|path>` to any subskill to select a configured template reference (for example `default` or `grouped`) or a direct template path; unresolved references fall back to the default template selection.

### Using with Codex

Once NLSS is installed as a Codex skill (see Part I / Part II), open Codex in your analysis repo.

- Run `/skills` to confirm NLSS is available.
- Use `$nlss` for explicit invocation when you want to force Codex to use this skill.

Example prompt:

```
Use nlss to run correlations (Pearson) on data.csv for age, score, and stress.
Use the default `nlss-workspace/` workspace output and summarize the NLSS format text.
```

### Using with Claude Code

Once NLSS is installed as a Claude skill (see Part I / Part II), restart Claude Code and open it in your analysis repo.

- Ask: "What Skills are available?" to confirm NLSS is loaded.
- When needed, explicitly tell Claude: "Use the `nlss` skill."

Example prompt:

```
Use the nlss repo. Run descriptive_stats on data.csv for age and score.
Use the default `nlss-workspace/` workspace output and report the NLSS format narrative and table file names.
```

### Tests

Test scripts, plans, and fixtures live under `tests/`, and the harness reads `tests/tests.yml`.

- Unix/WSL: `bash cmdscripts/tests.sh smoke`
- Windows PowerShell: `.\cmdscripts\tests.ps1 smoke`

Runs write to `outputs/test-runs/<timestamp>/` by default.

## Disclaimer and intended use

- Provided "AS IS" under Apache-2.0; no warranties or conditions of any kind.
- Users are responsible for validating results and decisions made from them; outputs are aids, not a substitute for expert review.
- Source availability helps transparency, but does not guarantee correctness or fitness for a particular purpose.
- Not intended for safety-critical, medical, legal, or regulatory decision making without independent verification.
- Modified versions may behave differently; anyone distributing changes should review and test their modifications.

## Open source notices

### License

NLSS is licensed under the Apache License, Version 2.0. See the `LICENSE` file for details.

If a `NOTICE` file is present, redistributions must include the attribution notices as described in the license.

### Trademark

NLSS™ is a trademark of Mike Hammes. 

The Apache License 2.0 does not grant permission to use the NLSS™ name or branding beyond reasonable and customary use to describe the origin of the Work and to reproduce the content of the `NOTICE` file. See `TRADEMARKS.md`.

### Dependencies

NLSS relies on R packages from CRAN which are installed by the user. No third-party source code is bundled in this repository.

### Maintainer

Prof. Dr. Mike Hammes (mike.hammes@mikehammes.name)
