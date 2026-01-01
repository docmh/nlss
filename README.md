# NLSS™ — Natural Language Statistics Suite

📁 NLSS is a workspace-first set of R scripts designed to help **researchers** run common analyses quickly—primarily **through a coding agent in an IDE** (e.g., Codex or Claude Code). 

🧑🤝🤖 NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Your requests as a senior researcher may be vague or jargon-heavy; the agent will inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in their scratchpad, and produce detailed, NLSS format-aligned, journal-alike reports based on templates. It also logs all deterministic R scripts used for analysis. 

💪 NLSS is packaged as an **Agent Skill** following the open **Agent Skills** standard: https://agentskills.io/specification. This repo’s `SKILL.md` is the entry point that agents use to discover what NLSS can do.

🤖📊📚 NLSS produces:

- `report_canonical.md`: human-readable, NLSS format-ready tables and narrative from used subskills like `descriptive-stats`, `sem` or `metaskill-runner`
- Plots, if you aks for them through the `plot` subskill
- Detailed, NLSS format-aligned and journal-alike reports if you use metaskills like `describe-sample`, `test-hypotheses` or `write-full-report`
- `analysis_log.jsonl`: machine-readable JSONL run log (append-only)
- `<dataset>.parquet`: a stateful workspace copy of your dataset (used by all modules)

You can run NLSS either:

- **via an Agent IDE (recommended):** prompt your agent to use the `nlss` skill and work inside an NLSS workspace, or
- **via terminal (secondary):** run the R scripts directly with `Rscript`.

NLSS was developed with the assistance of AI tools for drafting and iteration. All changes are curated, reviewed, and tested by the human maintainer. The architecture, design, and implementation decisions are the maintainer’s own.

---

## Part I — For Researchers (Get Running Quickly)

This section is ordered for a smooth, non-technical setup: **IDE → agent → R → verify agent shell → NLSS skill → demo**.

### Overview

1. Install an IDE + an agent (Steps 1–2).
2. Install R from CRAN (Step 3).
3. Open **R** from the Start menu or in bash and run the `install.packages(...)` snippet (Step 3).
4. Install NLSS by downloading the repo as a ZIP and copying it into your skills folder (Step 5).
5. Ask your agent: “Can you run `Rscript --version`?” (Step 4).

### 1) Install an IDE (examples only)

Use any IDE/editor you’re comfortable with. NLSS is commonly used with an IDE that supports agentic coding plugins and an integrated terminal. Examples (not recommendations):

- Visual Studio Code: https://code.visualstudio.com/
  - WSL guide (Windows): https://code.visualstudio.com/docs/remote/wsl
- Cursor: https://www.cursor.com/

### 2) Install a coding agent (examples: Codex or Claude Code)

Install a coding agent that can:

- read/write files in your project, and
- run shell commands (to call `Rscript`).

Examples (not recommendations): **OpenAI Codex** and **Anthropic Claude Code**.

Where to get them / installation guides:

- OpenAI Codex: https://developers.openai.com/codex/ (see also: CLI + Windows notes at https://developers.openai.com/codex/cli/windows)
- Anthropic Claude Code: https://docs.claude.com/en/docs/claude-code/overview

### 2a) Configure Codex IDE for NLSS (mode, reasoning, web, privacy)

Codex IDE exposes key controls in the bottom bar/model switcher (Agent/Chat, model, reasoning effort, Auto context) plus a settings panel. Use these guidelines:

- **Mode/permissions:** Use **Agent** mode (not Chat) so the agent can edit files and run commands. In Codex’s default approval mode, it can read/write inside the working directory and run commands there; it will ask before operating outside the workspace or accessing the network. Docs: https://developers.openai.com/codex/ide and https://developers.openai.com/codex/security
- **Reasoning effort:** If the UI exposes a reasoning‑effort slider (low/medium/high), consider increasing it for statistics‑heavy tasks (higher effort is slower but more thorough). This maps to `model_reasoning_effort` in Codex settings. Docs: https://developers.openai.com/codex/ide and https://developers.openai.com/codex/configure/local-config
- **Personal experience (what worked best):** Agent mode (not Chat), Auto context activated, **GPT‑5.2‑Codex** (not GPT‑5.2), and higher reasoning effort when you want more polished reports. NLSS runs even on Low reasoning effort, but higher effort usually produces clearer narratives and more informed chat answers.
- **Web/network access (for `research-academia`):** `research-academia` calls live scholarly APIs. Codex disables network access by default in `workspace-write`. If your UI has a web/network toggle, enable it; otherwise open Codex Settings (gear icon) → `Open config.toml` and set:

  ```toml
  [sandbox_workspace_write]
  network_access = true
  ```
  Docs: https://developers.openai.com/codex/security and https://developers.openai.com/codex/configure/local-config

- **Privacy controls:** If you prefer not to contribute prompts/results to model training, use OpenAI’s data controls (privacy portal or product settings). OpenAI’s consumer services (including Codex) may use content to improve models unless you opt out; business/API data is not used for training by default. Codex also has separate “full environment” training controls in its settings. See: https://help.openai.com/en/articles/5722486-how-your-data-is-used-to-improve-model-performance/ and https://openai.com/policies/how-your-data-is-used-to-improve-model-performance/

### 2b) Configure Claude Code (UI notes)

Claude Code exposes settings and model selection inside its UI via slash commands:

- **Settings UI:** In the interactive REPL, run `/config` to open a tabbed Settings interface where you can view status and modify options. Settings live in `~/.claude/settings.json` (user) and `.claude/settings.json` / `.claude/settings.local.json` (project). Docs: https://docs.claude.com/en/docs/claude-code/settings
- **Model picker:** Run `/model` to open the interactive model menu; `/status` shows the active model. Docs: https://support.claude.com/en/articles/11940350-claude-code-model-configuration
- **Personal experience (what worked best):** Sonnet 4.5 already runs NLSS; Opus 4.5 tends to produce more polished reports when available. Opus access may require extra usage depending on plan. Docs: https://support.claude.com/en/articles/11940350-claude-code-model-configuration

### 3) Install R (so `Rscript` works) and the NLSS R packages

NLSS executes `.R` scripts via `Rscript`. Install R in the environment you plan to use (macOS Terminal, Linux shell, Windows PowerShell, or WSL bash).

#### macOS

- Install R from CRAN (recommended for most users): https://cran.r-project.org/bin/macosx/
- Verify in Terminal: `which Rscript` and `Rscript --version`
- Note: the macOS installer places `R`/`Rscript` under `/usr/local/bin/` (symlinks). If your IDE/agent terminal can’t find `Rscript`, restart the IDE, or ensure `/usr/local/bin` is on `PATH`. See: https://mac.r-project.org/doc/manuals/R-admin.html

#### Linux (Ubuntu example)

```bash
sudo apt update
sudo apt install r-base
Rscript --version
```

#### Windows (choose one; this affects “bash”)

- **Windows native (PowerShell; easiest for most users):**
  - Install R from CRAN (download + installer): https://cran.r-project.org/bin/windows/base/
  - If you’ve never used PowerShell: that’s fine—your IDE will usually open it for you as the integrated terminal. You mostly just copy/paste.
  - Important caveat: the R for Windows installer does **not** always add `Rscript` to your `PATH` (and `winget` installs may not either). If your agent can’t find `Rscript`, follow the “PATH fix” in Step 4 below.

- **Windows + WSL (best if you want Linux bash):**
  - Install WSL, open an Ubuntu terminal, then install R inside WSL:

    ```bash
    sudo apt update
    sudo apt install r-base
    Rscript --version
    ```

  - Make sure your IDE/agent is running in the WSL environment (for example, open your repo from WSL so the agent terminal is Linux bash). Codex Windows guidance: https://developers.openai.com/codex/cli/windows

#### Install the R packages NLSS depends on (full list, once)

NLSS uses base R packages (`base`, `stats`, `utils`, `graphics`, `grDevices`, `tools`) plus these CRAN packages:

`arrow`, `car`, `curl`, `DHARMa`, `emmeans`, `foreign`, `ggplot2`, `haven`, `influence.ME`, `jsonlite`, `lavaan`, `lme4`, `lmerTest`, `mice`, `MVN`, `performance`, `psych`, `pwr`, `semPower`, `VIM`, `viridisLite`, `yaml`

If you prefer to install them without any terminal (beginner-friendly on Windows), open **R x64** from the Start menu and paste:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(c("arrow","car","curl","DHARMa","emmeans","foreign","ggplot2","haven","influence.ME","jsonlite","lavaan","lme4","lmerTest","mice","MVN","performance","psych","pwr","semPower","VIM","viridisLite","yaml"))
```

If you are comfortable using terminal, you can also run:

```bash
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('arrow','car','curl','DHARMa','emmeans','foreign','ggplot2','haven','influence.ME','jsonlite','lavaan','lme4','lmerTest','mice','MVN','performance','psych','pwr','semPower','VIM','viridisLite','yaml'))"
```

<details>
<summary>If package installation fails on Linux (common system dependencies)</summary>

Some packages may need system libraries when binaries are not available. For Apache Arrow (`arrow`) on Ubuntu, the Arrow project recommends:

```bash
sudo apt update
sudo apt install -y libcurl4-openssl-dev libssl-dev
```

Then rerun the `install.packages(...)` command. See: https://arrow.apache.org/docs/r/articles/install.html

</details>

### 4) Verify your coding agent can run `Rscript` (agent terminal check)

Easiest option: ask your coding agent to run the checks and paste back the output (for example: “Please run `Rscript --version` in your terminal and tell me what you see.”).

If you need to run commands yourself: open your IDE’s integrated terminal (in VS Code: **Terminal → New Terminal**).

**PowerShell (Windows native):**

```powershell
Get-Command Rscript
Rscript --version
Rscript -e "cat('Rscript OK\n')"
```

**bash (macOS/Linux/WSL):**

```bash
which Rscript
Rscript --version
Rscript -e "cat('Rscript OK\\n')"
```

#### PATH fix (Windows) if `Rscript` is “not recognized”

If `Rscript` is not found, add R’s `bin` folder to your Windows `PATH`:

1. Open Start → search “environment variables” → click **Edit the system environment variables**
2. Click **Environment Variables…**
3. Under “User variables”, select **Path** → **Edit** → **New**
4. Add your R `bin` path, typically one of:
   - `C:\\Program Files\\R\\R-<version>\\bin`
   - `C:\\Program Files\\R\\R-<version>\\bin\\x64`
5. Click OK/OK/OK and restart your IDE (so the agent terminal reloads `PATH`).

Tip: if you’re unsure which path you have, open *R* (Start → “R x64 …”) and run `R.home("bin")` to print the folder.

### 5) Install NLSS as a skill (so the agent can invoke it by name)

NLSS is an Agent Skill (it ships with `SKILL.md`). Install it into your agent’s skills folder, then restart the agent so it reloads skills.

**Codex (OpenAI; example agent host)**

Codex allows to install Agent Skills conversationally. Just prompt Codex: "$skill-intaller Install NLSS from https://github.com/docmh/nlss.git" and follow the instructions. 

Manual installation:

- Skill locations and scopes: https://developers.openai.com/codex/skills
- Typical user-wide install (macOS/Linux/WSL):

  ```bash
  mkdir -p ~/.codex/skills
  git clone https://github.com/docmh/nlss.git ~/.codex/skills/nlss
  ```

<details>
<summary>No-terminal / no-git install (Windows/macOS/Linux)</summary>

1. Download the NLSS repo as a ZIP from wherever it is hosted (look for a “Download ZIP” / “Download source” button).
2. Extract it and rename the extracted folder to `nlss` (important: the folder must contain `SKILL.md` directly inside it).
3. Move the `nlss` folder into a Codex skills folder:
   - Windows (File Explorer address bar): `%USERPROFILE%\.codex\skills\nlss`
   - macOS/Linux: `~/.codex/skills/nlss`
   - Per-project (shared): `<your-project>/.codex/skills/nlss`
4. Restart Codex and run `/skills` to confirm it’s loaded.

`SKILL.md` must have the path `<your-skills-location>/.codex/skills/nlss/SKILL.md`. 

</details>

- Team/shared install (per repo): `.codex/skills/nlss/`
- Verify in Codex: run `/skills`, and use `$nlss` for explicit invocation when needed.

<details>
<summary>Windows notes (PowerShell vs WSL)</summary>

- If your agent runs in **WSL bash**, use the macOS/Linux/WSL install commands inside WSL.
- If your agent runs in **Windows PowerShell**, install into your Windows home directory (PowerShell `$HOME`):

  ```powershell
  New-Item -ItemType Directory -Force -Path "$HOME\.codex\skills" | Out-Null
  git clone https://github.com/docmh/nlss.git "$HOME\.codex\skills\nlss"
  ```

</details>

**Claude Code (Anthropic; example agent host)**

- Skill locations: https://docs.claude.com/en/docs/claude-code/skills
- Personal skills (macOS/Linux/WSL): `~/.claude/skills/nlss/`
- Project skills (shared with your team): `.claude/skills/nlss/`

<details>
<summary>No-terminal / no-git install (Windows/macOS/Linux)</summary>

1. Download the NLSS repo as a ZIP from wherever it is hosted.
2. Extract it and rename the extracted folder to `nlss` (important: `SKILL.md` must be directly inside the `nlss` folder).
3. Move the `nlss` folder into a Claude skills folder:
   - Windows: `%USERPROFILE%\.claude\skills\nlss`
   - macOS/Linux (personal): `~/.claude/skills/nlss`
   - Per-project (shared): `<your-project>/.claude/skills/nlss`
4. Restart Claude Code and ask: “What Skills are available?”

`SKILL.md` must have the path `<your-skills-location>/.codex/skills/nlss/SKILL.md`. 

</details>

<details>
<summary>Windows notes (PowerShell vs WSL)</summary>

- If your agent runs in **WSL bash**, use the macOS/Linux/WSL paths inside WSL.
- If your agent runs in **Windows PowerShell**, your personal skills folder is under your Windows home directory (PowerShell `$HOME`):

  ```powershell
  New-Item -ItemType Directory -Force -Path "$HOME\.claude\skills" | Out-Null
  git clone https://github.com/docmh/nlss.git "$HOME\.claude\skills\nlss"
  ```

</details>

### 6) Fastest onboarding: ask for the `run-demo` metaskill

NLSS includes a guided onboarding metaskill called `run-demo`. It explains the workflow, then (with your permission) sets up a demo workspace using the bundled sample dataset.

Try a natural language prompt like:

- "Run the `run-demo` metaskill to onboard me to NLSS."
- "Show me what NLSS can do and set up a demo workspace."
- "Give me a quick NLSS walkthrough, then prepare the demo."

The demo uses `assets/sample-data/golden_dataset.csv` by default and will ask before creating any workspace files.

### 7) First example (agent-first): analyze the bundled golden dataset

This repo ships a small demo dataset at `assets/sample-data/golden_dataset.csv` (and a test fixture copy at [tests/data/golden_dataset.csv](tests/data/golden_dataset.csv)).

1) From the NLSS repo folder (the one containing `SKILL.md`), create a working folder and copy the dataset (you can do this yourself, or ask your agent to do it).

- **No terminal (Windows/macOS):**
  - Inside your NLSS repo folder, create a folder named `demo`.
  - Copy `assets/sample-data/golden_dataset.csv` into `demo/` and keep the name `golden_dataset.csv`.

<details>
<summary>Terminal option (PowerShell or bash)</summary>

PowerShell:

```powershell
New-Item -ItemType Directory -Force -Path demo | Out-Null
Copy-Item assets\sample-data\golden_dataset.csv demo\golden_dataset.csv
```

bash:

```bash
mkdir -p demo
cp assets/sample-data/golden_dataset.csv demo/golden_dataset.csv
```

</details>

2) Prompt your agent with natural language, for example:

- "Use `nlss` to initialize a workspace for `demo/golden_dataset.csv`, then run descriptive stats for `age` and `score` grouped by `condition`."
- "Run a `gender` × `condition` crosstab with chi² + effect sizes, then interpret."
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

### 8) The workspace structure (what NLSS creates)

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

### 9) How output + logging works

- **Subskills** append sections to `report_canonical.md` and (by default) append one JSON line to `analysis_log.jsonl`.
- **Metaskills** (agent-run workflows) may additionally create a standalone, journal-alike file `report_<YYYYMMDD>_<metaskill>_<intent>.md` in the dataset folder.
- Output directories are intentionally not a per-run CLI flag; choose a workspace root by where you run, or by changing `defaults.output_dir` (Part II).

### 10) Easy prompts to give your agent (copy/paste)

Try these in Codex / Claude Code:

1. "Use `nlss` to initialize a workspace for `demo/golden_dataset.csv` and tell me which files were created."
2. "Run descriptive stats for `age` and `score`, grouped by `condition`, and interpret the main differences."
3. "Run a `gender` × `condition` crosstab with chi² + effect sizes; interpret."
4. "Run Spearman correlations among `age`, `score`, and `stress`; summarize and note caveats."

---

## Part II — How to Use NLSS (Workspaces, Logs, and Customization)

This section is a practical guide for day-to-day use after installation. It explains where NLSS puts files, how to watch progress, what the different module types mean, how logging works, and how to customize behavior.

### 1) Workspaces and the files inside them

NLSS is workspace-first: every dataset gets a dedicated folder under a workspace root. The workspace root is detected by `nlss-workspace.yml` (current directory, parent, or one-level child). If no manifest exists, NLSS uses `defaults.output_dir` from `scripts/config.yml` (default: `./nlss-workspace`).

Typical layout:

```text
nlss-workspace/
  nlss-workspace.yml
  <dataset>/
    <dataset>.parquet
    scratchpad.md
    report_canonical.md
    analysis_log.jsonl
    plots/
    backup/
    report_YYYYMMDD_<metaskill>_<intent>.md
```

What these files do:

- `<dataset>.parquet`: the working copy every module reads (some modules update it with backups).
- `scratchpad.md`: planning + decisions (especially for metaskills).
- `report_canonical.md`: append-only NLSS format output log.
- `analysis_log.jsonl`: append-only machine log for audit and reconstruction.
- `plots/`: saved figures (when modules emit plots).
- `backup/`: timestamped parquet backups before destructive updates.
- `report_YYYYMMDD_<metaskill>_<intent>.md`: full metaskill report (only when a metaskill produces one).

### 2) Supported input file formats

NLSS accepts the following dataset formats as inputs (via the standard CLI flags or agent prompts):

- **CSV (`.csv`)**: use `--csv <path>` for comma-separated text data.
- **SPSS (`.sav`)**: use `--sav <path>` for SPSS data files.
- **RDS (`.rds`)**: use `--rds <path>` and ensure the file contains a data frame.
- **RData (`.RData` / `.rdata`)**: use `--rdata <path>` plus `--df <data_frame_name>` to select the data frame when multiple objects exist.
- **Parquet (`.parquet`)**: use `--parquet <path>`; this is the preferred workspace format and requires the `arrow` R package for I/O.

Regardless of input type, NLSS creates (or reuses) a workspace copy at `<workspace-root>/<dataset-name>/<dataset-name>.parquet` and reads from that copy for all analyses. If your data is in another format (e.g., Excel), export it to CSV or Parquet first.

### 3) Keep `report_canonical.md` open while you run analyses

`report_canonical.md` is your live lab notebook. NLSS appends new sections after every run, so keeping it open lets you:

- watch progress in real time,
- validate that the right tables/narratives were produced,
- copy/share results immediately without hunting through logs.

It’s normal for this file to grow; it’s meant to be an audit trail, not a single clean report.

### 4) NLSS format (the formatting approach)

NLSS outputs are written in **NLSS format**: a consistent, APA‑inspired Markdown style used for tables, notes, and narrative. It’s designed to be readable in plain text and consistent across modules.

Where it is used:

- **`report_canonical.md`**: every subskill appends an NLSS format section (table + narrative + notes).
- **Metaskill reports** (`report_YYYYMMDD_<metaskill>_<intent>.md`): journal-alike, NLSS format sections assembled into a full report.
- **Templates**: NLSS format is implemented via templates under `assets/` and can be customized without changing R scripts.

If you want the formatting rules, see `references/metaskills/format-document.md`. NLSS format is close to APA 7 but not a strict substitute for final publication formatting.

### 5) Skills, metaskills, and utilities (what they are)

NLSS uses three kinds of modules:

- **Subskills**: single-purpose analysis modules (descriptive stats, t-tests, regression, etc.).
- **Metaskills**: agent-run workflows that chain subskills into a research task.
- **Utilities**: helper tools (integrity checking, report reconstruction, quick calculations).

Where to find documentation:

- `SKILL.md` is the index (names + short descriptions).
- `references/subskills/` has one doc per analysis module.
- `references/metaskills/` has one doc per workflow.
- `references/utilities/` has one doc per helper tool.

The full list of available modules also appears later in this README (Part III).

### 6) Convenient extras: research-academia, explain-statistics, explain-results

These are frequently useful even outside full workflows:

- **`research-academia` (utility):** used by several metaskills, but also valuable as a standalone literature scan in NLSS chat or an IDE coding agent. It calls live scholarly APIs, so **network access is required**; some IDE agents require enabling web/network access before it will run successfully. See `references/utilities/research-academia.md`.
- **`explain-statistics` (metaskill):** plain-language explanations of statistical concepts, assumptions, and test choices. It’s conversational by default and doesn’t require `metaskill-runner` unless you explicitly request a logged report. See `references/metaskills/explain-statistics.md`.
- **`explain-results` (metaskill):** helps interpret NLSS output sections (tables, diagnostics, model results) in researcher-friendly language. Like `explain-statistics`, it can be purely conversational unless you ask for a formal report. See `references/metaskills/explain-results.md`.

### 7) Logging: what it captures and why it matters

Every run can record two complementary logs:

- `report_canonical.md`: human-readable NLSS format output.
- `analysis_log.jsonl`: machine-readable JSONL entries with metadata and (optionally) report blocks.

Why this matters:

- **Integrity checks**: `check-integrity` can recover checksums from `analysis_log.jsonl` to spot inconsistent or tampered logs.
- **Report recovery**: `reconstruct-reports` can rebuild `report_canonical.md` (and metaskill reports) from log entries *when outputs are logged*.

Logging is controlled by `logging.*` in `scripts/config.yml`. Set `logging.enabled: "false"` to disable logging. If you want report reconstruction, keep `logging.include_outputs` enabled so report blocks are stored.

### 8) Configuration: `scripts/config.yml`

`scripts/config.yml` is the main configuration file. The key sections are:

- `defaults.*`: global defaults (e.g., `defaults.output_dir`, `defaults.workspace_manifest`, `defaults.csv.sep`, `defaults.digits`, `defaults.interactive`).
- `logging.*`: what gets recorded in `analysis_log.jsonl` (e.g., `enabled`, `include_timestamps`, `include_versions`, `include_inputs`, `include_outputs`, `include_checksum`).
- `modules.<subskill>.*`: per-module defaults (for example, `modules.crosstabs.percent`, `modules.regression.bootstrap`).
- `templates.<subskill>.*`: which template file a module uses by default.

CLI flags always override config values for a specific run. If `config.yml` is missing or unreadable, built-in defaults from `scripts/R/lib/config.R` are used.

### 9) Templates: customize or add your own

Most NLSS outputs are template-driven. Templates live under `assets/<subskill>/` and are Markdown files with YAML front matter (tokens, column definitions, narrative settings).

Common ways to customize:

- **Edit an existing template**: tweak wording, table columns, or narrative.
- **Add a new template**: copy an existing template, save it under `assets/<subskill>/`, then register it in `scripts/config.yml` under `templates.<subskill>.<name>`.
- **Select per run**: pass `--template <name>` (from config) or `--template <path>` (direct path). Relative paths are resolved under `assets/`.

Each subskill reference file lists the available template tokens and column keys (see `references/subskills/<subskill>.md`). Metaskill reports use `assets/metaskills/report-template.md` by default and can be overridden with `templates.metaskill_report.default` in `scripts/config.yml`.

---

## Part III — For Developers / Maintainers

### Agent Skills standard + installation (framework details)

NLSS follows the **Agent Skills** standard (https://agentskills.io/specification). In practice, that means:

- A skill is a **folder** containing `SKILL.md` (YAML front matter with `name`/`description` + human-readable docs).
- The skill may also ship `references/` (how-to docs), `scripts/` (runnables), and `assets/` (templates).

How you install skills depends on the agent. Examples:

- **Codex (OpenAI):** Codex discovers skills by scanning (in order): `$PWD/.codex/skills`, `$PWD/../.codex/skills`, `$REPO_ROOT/.codex/skills`, `$CODEX_HOME/skills`, `/etc/codex/skills`, and skills bundled with Codex. Install NLSS by placing it in one of those locations under a folder named `nlss/` (matching `name: nlss`). Docs: https://developers.openai.com/codex/skills
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
| `crosstabs` | `scripts/R/crosstabs.R` | Cross-tabulations with chi²/Fisher tests. | Yes (`crosstabs/default-template.md`, `crosstabs/grouped-template.md`) |
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
- `references/metaskills/format-document.md`
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
- Test harness requires Python 3 available as `python3` or `python` (or set `PYTHON_BIN`).

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

This repository includes a NOTICE file. Redistributions must include it as described by the license.

### Trademark

NLSS™ is a trademark of Mike Hammes. 

The Apache License 2.0 does not grant permission to use the NLSS™ name or other branding beyond reasonable and customary use to describe the origin of the Work and to reproduce the content of the `NOTICE` file. See `TRADEMARKS.md`.

### Dependencies

NLSS relies on R packages from CRAN which are installed by the user. No third-party code is vendored/bundled in this repository.

### Maintainer

Mike Hammes (mike.hammes@mikehammes.name)
