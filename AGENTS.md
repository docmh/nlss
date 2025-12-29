# AGENTS

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, APA 7-aligned, journal-ready report. When documenting in `scratchpad.md`, `report_canonical.md`, or `analysis_log.jsonl`, mask workspace-external paths as `<external>/<filename>` and keep workspace-internal paths relative.

## Metaskill Implementation Guide

Metaskills are Markdown pseudoscripts executed by the agent (there is no separate runner). Use them to orchestrate subskills based on user intent and to document the plan/decisions in the dataset scratchpad.

### Structure

- Create a new file `nlss/references/metaskills/<metaskill-name>.md`.
- Use the same Markdown structure as subskill references (YAML front matter plus Overview/Workflow/Inputs/Options/Outputs/Template guidance as applicable).
- Include YAML front matter with a concise `description` and a `name` matching the metaskill.
- Keep the metaskill spec readable for humans; the agent is the executor.

### Metaskill Workflow (agent-run)

- Inspect the dataset first and summarize candidate variables in `scratchpad.md`.
- Outline a step-by-step plan in `scratchpad.md` before running subskills; update progress after each step.
- Ask clarifying questions when intent, grouping variables, or variable roles are ambiguous.
- Prefer NLSS subskills whenever they cover the request; only use `generate-r-script` as a last resort when the analysis is out of scope and explicit permission is granted.
- Log metaskill activation using the `metaskill-runner` subskill (pass `--meta <name>` and optional `--intent`).
- Execute the referenced subskills in order, using the workspace parquet copy and standard logging.
- On completion, generate `report_<YYYYMMDD>_<metaskill>_<intent>.md` first (APA 7-ready, journal-ready narrative, tables, and plots when helpful; use `YYYYMMDD` and an ASCII slug for `<intent>`), then log metaskill finalization with `metaskill-runner --phase finalization --synopsis "<text>"` so a `# Synopsis` section is appended to `report_canonical.md`. The runner should fail if the metaskill report is missing.

### Metaskill Spec Content

Include the following sections in each metaskill Markdown file:

- **Overview**: What the metaskill does and when to use it.
- **Intent/Triggers**: Example prompts or cues that should select this metaskill.
- **Inputs/Clarifications**: Required inputs and the clarifying questions to ask.
- **Procedure**: Ordered steps, referencing the subskills to run and any flags.
- **Outputs**: `report_canonical.md`, `analysis_log.jsonl`, and scratchpad updates.
- **Finalization**: Generate the `report_<YYYYMMDD>_<metaskill>_<intent>.md` file first, then log completion with `metaskill-runner --synopsis "<text>"` to add a `# Synopsis` to `report_canonical.md` (finalization should fail if the report is missing).

### <metaskill-name>.md expectations

- YAML front matter with `name` and `description`.
- Describe triggers/intents and the clarifying questions the agent should ask.
- Describe the ordered subskills to run and any default flags/decision rules, using a procedural or pseudocode-style outline when helpful.
- Explain how the metaskill updates `scratchpad.md` (plan + progress + decisions).
- Note that activation should be logged via `metaskill-runner`.
- Mention outputs (`report_canonical.md`, `analysis_log.jsonl`, and `report_<YYYYMMDD>_<metaskill>_<intent>.md`) and workspace location rules (use `YYYYMMDD` and an ASCII slug for `<intent>`), and note that the report must exist before finalization.
- Require a finalization log entry plus a `# Synopsis` appended to `report_canonical.md` via `metaskill-runner --synopsis` after the metaskill report is created.
- Require the final report to be APA 7-ready and suitable for journal submission when possible.

### Add metaskill entries

- Add the new metaskill to `nlss/SKILL.md` under **Metaskills** with a relative link to `references/metaskills/<metaskill-name>.md`.
- Update the YAML front matter `description` in `nlss/SKILL.md` to mention the new metaskill when it becomes part of the core set.
- Keep `README.md` updated to mention new metaskills and their location.

## Subskill Implementation Guide

Use this repo pattern to add new statistic subskills. Treat `nlss/references/subskills/descriptive-stats.md` and `nlss/references/subskills/frequencies.md` as the reference implementations.

### Structure

- Create a new file `nlss/references/subskills/<subskill-name>.md`.
- Put R scripts in `nlss/scripts/R`.
- Create templates in `nlss/assets/<subskill-name>` as needed. 
- Emit outputs to the workspace root identified by `nlss-workspace.yml` in the current working directory, its parent, or a one-level child. If no manifest is present, fall back to `defaults.output_dir` from `nlss/scripts/config.yml` (via `get_config_value`) with a fallback to `./outputs/tmp`.

### Workspace-first architecture (stateful)

- Treat the workspace root as the current working directory, its parent, or a one-level child containing `nlss-workspace.yml` (fallback: `defaults.output_dir` from `config.yml`).
- For any referenced dataset (CSV/SAV/RDS/RData/Parquet), ensure a dataset workspace folder exists at `<workspace-root>/<dataset-name>/` with `scratchpad.md` and `report_canonical.md`; if missing, run `init-workspace` first.
- Ensure a workspace copy exists at `<workspace-root>/<dataset-name>/<dataset-name>.parquet` (dataset name = filename stem or `--df`, sanitized).
- All subskills should operate on the workspace parquet copy (prefer `--parquet` pointing at the workspace file, or rely on `load_dataframe()` to auto-copy).
- `data-transform` and `missings` must update the workspace parquet copy in place and create a backup at `<workspace-root>/<dataset-name>/backup/<dataset-name>-<timestamp>.parquet` before overwriting.
- Direct workspace runs (no input flags) should load `active_dataset` from the manifest in the current directory.
- Parquet support requires the R package `arrow`; document this in new subskills if they read/write data.

### Template logic (YAML)

New subskills should use the YAML template system for `report_canonical.md`:

- Add template paths under `templates.<subskill-name>` in `nlss/scripts/config.yml` (and `nlss/scripts/R/lib/config.R` built-in defaults).
- Templates are Markdown files with optional YAML front matter and `{{token}}` placeholders.
- Support `table.columns` (ordered columns with `key`, optional `label`, optional `drop_if_empty`) and provide a computed `table_body` token from the analysis results.
- Provide `note.template` (defaults to `{{note_default}}`) and `narrative.template` or `narrative.row_template` (row-based rendering).
- Supply module-specific note/narrative tokens and per-row tokens in the script via `template_context` (see `nlss/scripts/R/lib/formatting.R`).
- Document available column keys and tokens in `nlss/references/subskills/<subskill-name>.md`.

### Configuration Defaults

- Source `config.R` in every new script (`source_lib("config.R")`) before `io.R` so defaults are available.
- Load defaults from `nlss/scripts/config.yml` via `get_config_value`, but always let CLI flags override at runtime.
- In interactive prompts, show config-driven defaults (from `config.yml`) instead of hard-coded values.
- Add module-specific defaults under `modules.<subskill-name>` in `nlss/scripts/config.yml` if needed.

### Execution

- Run `.R` scripts directly with `Rscript` on all platforms; ensure `Rscript` is on PATH in the active shell.
- On Windows + WSL, ask the user to check both environments:
  - PowerShell: `Get-Command Rscript` or `Rscript --version`
  - WSL: `which Rscript` or `Rscript --version`
- If `Rscript` is available in WSL but not Windows PowerShell, prefer switching the Codex IDE to WSL; otherwise install R in Windows.
- If `Rscript` is available in Windows PowerShell but not WSL, prefer installing R in WSL and switching Codex to WSL; otherwise stay in Windows PowerShell.
- If `Rscript` is missing in the target environment, instruct the user to install R and expose `Rscript` globally:
  - Windows: install from CRAN or `winget install --id RProject.R -e`, enable "Add R to PATH" or add `C:\\Program Files\\R\\R-x.y.z\\bin` (or `bin\\x64`) to PATH, then restart the terminal.
  - WSL (Ubuntu): `sudo apt update && sudo apt install r-base`.
  - Verify with `Rscript --version`.

### Outputs

- Always provide machine-readable output for `analysis_log.jsonl` plus human-readable APA-ready output for `report_canonical.md` inside each dataset workspace folder.
- Subskills extend `report_canonical.md` only and do not create standalone report files; only metaskills create `report_<YYYYMMDD>_<metaskill>_<intent>.md`.
- Append new logs if the output files already exist in the dataset workspace.
- Include assumptions/diagnostics outputs when applicable (e.g., residual checks, normality tests).
- Default outputs should always land in `<workspace-root>/<dataset-name>/`, including interactive mode (workspace root = current directory, parent, or one-level child with `nlss-workspace.yml`; fallback to `defaults.output_dir`).
- When presenting paths in console output or reports, prefer workspace-relative paths; use absolute paths only when the target is outside the workspace root.
- Workspaces must be non-nested and unique per parent folder; stop if nested or sibling manifests are detected.
- Direct workspace runs (no input flags) should use the current dataset folder if applicable; otherwise use `active_dataset` from the manifest.
- When building APA outputs with optional grouping, avoid string-splitting keys that can introduce `NA` groups; instead, iterate over unique `(variable, group)` pairs directly and normalize missing groups.

### Tests

- Use `outputs/tests/tests.yml` `tests.*` as the source of truth for test planning and execution, including all paths.
- Use `tests.scripts.harness_unix` or `tests.scripts.harness_windows` for suite runs; these harnesses call `tests.scripts.smoke_unix` and `tests.scripts.deliberate_unix` as defined in `outputs/tests/tests.yml`.
- `tests.scripts.harness_windows` (PowerShell) uses Windows `Rscript` and Git Bash to run the Unix harness for smoke/deliberate/all; `-ForceWindows` runs a smoke-only Windows-native path. WSL is not used.
- Test runners require Python available as `python3` or `python` (or set `PYTHON_BIN`); `cmdscripts/tests.ps1` will prepend a detected Python install to `PATH` and set `PYTHON_BIN=python` when available.
- Keep smoke suite behavior and output folder layout aligned between `cmdscripts/tests.ps1` and `cmdscripts/tests.sh` so Windows and Unix runs are comparable.
- Keep test assets and scripts under `tests.data_dir` and use `tests.golden_dataset` as the standard dataset for smoke and module-specific tests.
- Write all test outputs under `tests.output_dir/<timestamp>/` by default, and honor `--root`/`NLSS_TEST_ROOT` plus `--keep`/`NLSS_KEEP_RUNS` (default `tests.keep_runs_default`).
- Use `tests.template_dir` and `tests.template_marker` for template override checks, and `tests.plans.*` for suite plan docs.
- Support `NLSS_TEST_ROOT` (force a run root) and `NLSS_KEEP_RUNS` (retention count) in new test scripts.
- When adding a new subskill, expand the smoke runner in `tests.scripts.smoke_unix` and the plan in `tests.plans.smoke`, and add the module script under `tests.scripts.modules.*` plus `tests.suites.deliberate.modules` coverage (and update `tests.scripts.deliberate_unix` if the deliberate suite runner needs it).
- For expected failures, treat "expected error + informational feedback" as a pass (log-based checks are acceptable when stderr/stdout is not reliable).
- Add output-generation coverage for templates: verify default templates and temporarily altered templates produce the expected changes.
- Ensure tests demonstrate robust, expected, and reliable behavior for the new subskill. Cover all statistic features and options of this new subskill and apply positive, edge, and negative test cases as appropriate.

### <subskill-name>.md expectations

- YAML front matter with `name` and `description`.
- Describe inputs (CSV/RDS/RData/SAV/Parquet, grouping variables, factor handling).
- List CLI flags and defaults, referencing the corresponding `config.yml` keys (and note that CLI flags override config defaults).
- Explain outputs and provide APA 7 narrative guidance.
- Explain how to use templates if applicable and ensure those templates are used for generating `report_canonical.md`.
- Mention how to run via `Rscript` directly and where outputs are written.

### Example scopes

- **Basic**: frequencies, cross-tabs, correlations (Pearson/Spearman), reliability (alpha).
- **Intermediate**: t-tests, ANOVA, regression.
- **Advanced**: general linear model, mixed linear model, repeated measures.

### Add subskill entries

- Add the new subskill to `nlss/SKILL.md` under **Subskills** with a relative link to `references/subskills/<subskill-name>.md`.
- Add a concise, task-focused `description` to the YAML front matter in each new subskill reference file.
- Keep the `description` field and **Subskills** section in `nlss/SKILL.md` updated after adding a new subskill.
- Always update `README.md` to reflect new subskills (module list, templates, reference docs, and example usage).
