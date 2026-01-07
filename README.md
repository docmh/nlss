# NLSS — Natural Language Statistics Suite

**NLSS helps researchers run statistical analyses through natural language conversations with an AI coding agent.**

You describe what you want in plain English; NLSS handles the R scripts, produces well-formatted tables, and logs everything for reproducibility.

---

## Table of Contents

- [Quick Start (5 minutes)](#quick-start-5-minutes)
- [What NLSS Does](#what-nlss-does)
- [Glossary](#glossary)
- **Part I — Installation**
  - [Step 1: Install an IDE](#step-1-install-an-ide)
  - [Step 2: Install a Coding Agent](#step-2-install-a-coding-agent)
  - [Step 3: Install R](#step-3-install-r)
  - [Step 4: Install R Packages](#step-4-install-r-packages)
  - [Step 5: Install NLSS](#step-5-install-nlss)
  - [Step 6: Verify Everything Works](#step-6-verify-everything-works)
- **Part II — Using NLSS**
  - [Your First Analysis](#your-first-analysis)
  - [Understanding the Workspace](#understanding-the-workspace)
  - [Example Prompts](#example-prompts)
  - [Available Analyses](#available-analyses)
  - [Tips for Best Results](#tips-for-best-results)
- **Part III — Configuration & Customization**
  - [Configuration File](#configuration-file)
  - [Templates](#templates)
  - [Logging](#logging)
- **Part IV — For Developers**
  - [Architecture](#architecture)
  - [Module Reference](#module-reference)
  - [Testing](#testing)
  - [Contributing](#contributing)
- [Troubleshooting](#troubleshooting)
- [License & Legal](#license--legal)

---

## Quick Start (5 minutes)

Already have VS Code, a coding agent (Codex or Claude Code), and R installed? Here's the fastest path:

### 1. Install R packages (one time)

Open **R** — from your Start menu (Windows), Applications folder (macOS), or terminal (Linux: type `R`) — and paste:

```r
install.packages(c("arrow","car","curl","DHARMa","emmeans","foreign","ggplot2","haven","influence.ME","jsonlite","lavaan","lme4","lmerTest","mice","MVN","performance","psych","pwr","semPower","VIM","viridisLite","yaml"))
```

### 2. Install NLSS

**Codex users:** Tell your agent:
> "Install NLSS from https://github.com/docmh/nlss.git using $skill-installer"

**Claude Code users:** Download the [NLSS ZIP](https://github.com/docmh/nlss), extract it, and move the `nlss` folder to:
- Windows: `%USERPROFILE%\.claude\skills\nlss`
- macOS/Linux: `~/.claude/skills/nlss`

### 3. Try it

Restart your agent, then say:
> "Run the NLSS demo to show me what it can do."

That's it! For detailed instructions, continue reading below.

---

## What NLSS Does

```
┌─────────────────────────────────────────────────────────────────────┐
│  YOU (Senior Researcher)                                            │
│  "Run descriptive stats for age and score, grouped by condition"    │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  AI AGENT (Assistant Researcher)                                    │
│  • Understands your request                                         │
│  • Runs the appropriate R scripts                                   │
│  • Asks clarifying questions if needed                              │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  NLSS OUTPUTS                                                       │
│  • report_canonical.md    → Human-readable tables & narrative       │
│  • analysis_log.jsonl     → Machine-readable audit log              │
│  • plots/                 → Well-formatted figures               │
│  • Full reports           → Journal-style write-ups                 │
└─────────────────────────────────────────────────────────────────────┘
```

**NLSS is packaged as an Agent Skill** following the open [Agent Skills standard](https://agentskills.io/specification). Your AI agent reads `SKILL.md` to discover what NLSS can do.

---

## Glossary

New to NLSS? Here are the key terms:

| Term | What it means |
|------|---------------|
| **Workspace** | A folder where NLSS stores your data, reports, and logs. Created automatically when you analyze a dataset. |
| **Parquet** | A fast, efficient file format. NLSS converts your data (CSV, SPSS, etc.) to Parquet for faster analysis. |
| **Subskill** | A single analysis module (e.g., `descriptive-stats`, `t-test`, `regression`). |
| **Metaskill** | A multi-step workflow that chains subskills together (e.g., `write-full-report`). |
| **Agent** | The AI assistant (Codex or Claude Code) that interprets your requests and runs NLSS. |
| **IDE** | Integrated Development Environment — the app where you write code and talk to the agent (e.g., VS Code, Cursor). |
| **Skill** | A plugin that teaches an AI agent new capabilities. NLSS is a skill. |

---

# Part I — Installation

This guide walks you through setup step by step. Each step has **GUI instructions** (point-and-click) with terminal alternatives for those who prefer them.

## Installation Overview

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│   Step 1     │    │   Step 2     │    │   Step 3     │    │   Step 4     │    │   Step 5     │
│  Install     │  → │  Install     │  → │  Install     │  → │  Install R   │  → │  Install     │
│  an IDE      │    │  an Agent    │    │  R           │    │  Packages    │    │  NLSS        │
└──────────────┘    └──────────────┘    └──────────────┘    └──────────────┘    └──────────────┘
       ↓                   ↓                   ↓                   ↓                   ↓
   VS Code or          Codex or           Download            Open R and         Copy folder
   Cursor              Claude Code        from CRAN           paste command      to skills dir
```

---

## Step 1: Install an IDE

An IDE is where you'll interact with the AI agent. Choose one:

### Option A: Visual Studio Code (Recommended for beginners)

**Windows / macOS:**
1. Go to [code.visualstudio.com](https://code.visualstudio.com/)
2. Click the big **Download** button
3. Run the installer and follow the prompts
4. Launch VS Code when done

**Linux:**
1. Go to [code.visualstudio.com](https://code.visualstudio.com/)
2. Download the `.deb` (Ubuntu/Debian) or `.rpm` (Fedora/RHEL) package
3. Install via your package manager or double-click the file
4. Or use Snap: `sudo snap install code --classic`

### Option B: Cursor

1. Go to [cursor.com](https://www.cursor.com/)
2. Download the installer for your platform (Windows, macOS, or Linux AppImage)
3. Run the installer or make the AppImage executable and run it
4. Cursor has AI built-in, but you'll still need to configure it for NLSS

---

## Step 2: Install a Coding Agent

The coding agent is the AI that understands your requests and runs NLSS. Choose one:

### Option A: OpenAI Codex

1. Go to [Codex download page](https://developers.openai.com/codex/)
2. Follow the installation instructions for your platform
3. Open VS Code — Codex will appear in the sidebar

**Recommended settings for NLSS:**
- Use **Agent mode** (not Chat mode)
- Enable **Auto context**
- Use **GPT-5.2-Codex** model
- Set reasoning effort to **Medium** or **High** for better reports

<details>
<summary>Detailed Codex configuration</summary>

Codex exposes controls in the bottom bar. For NLSS:

- **Mode:** Agent (so it can edit files and run commands)
- **Reasoning effort:** Medium or High for statistics-heavy tasks
- **Network access:** Required for the `research-academia` utility. Enable via UI toggle, or add to your `config.toml`:

  ```toml
  [sandbox_workspace_write]
  network_access = true
  ```

See [Codex docs](https://developers.openai.com/codex/ide) for details.

</details>

### Option B: Anthropic Claude Code

1. Go to [Claude Code overview](https://docs.claude.com/en/docs/claude-code/overview)
2. Follow the installation instructions
3. Claude Code runs in your terminal or integrates with VS Code

**Recommended settings for NLSS:**
- Use **Opus 4.5** for best results (Sonnet 4.5 also works)
- Run `/config` to see settings, `/model` to switch models

<details>
<summary>Detailed Claude Code configuration</summary>

In Claude Code's interactive mode:
- `/config` — Opens settings interface
- `/model` — Switch between models
- `/status` — Shows current model

Settings are stored in `~/.claude/settings.json` (user) and `.claude/settings.json` (project).

See [Claude Code settings docs](https://docs.claude.com/en/docs/claude-code/settings) for details.

</details>

---

## Step 3: Install R

R is the statistical engine that powers NLSS. You don't need to know R — the agent handles it.

### Windows

1. Go to [CRAN R for Windows](https://cran.r-project.org/bin/windows/base/)
2. Click **"Download R-4.x.x for Windows"**
3. Run the installer
4. **Important:** When prompted, choose **"Yes"** to modify the PATH (this lets NLSS find R)

> **Note:** If the installer doesn't offer a PATH option, you may need to add it manually. See [Troubleshooting](#rscript-is-not-recognized-windows).

### macOS

1. Go to [CRAN R for macOS](https://cran.r-project.org/bin/macosx/)
2. Download the `.pkg` file for your Mac (Apple Silicon or Intel)
3. Double-click to install
4. R is automatically added to your PATH

### Linux

**Ubuntu / Debian:**
```bash
sudo apt update
sudo apt install r-base
```

**Fedora:**
```bash
sudo dnf install R
```

**Arch Linux:**
```bash
sudo pacman -S r
```

After installation, verify with: `Rscript --version`

---

## Step 4: Install R Packages

NLSS needs several R packages for statistical analyses. Install them once and you're set.

### The Easy Way (Recommended)

1. Open **R**:
   - **Windows:** Start menu → search for "R" or "R x64"
   - **macOS:** Applications folder → R
   - **Linux:** Open a terminal and type `R`, or find "R" in your applications menu
2. The R Console will open
3. Paste this command and press Enter:

```r
install.packages(c("arrow","car","curl","DHARMa","emmeans","foreign","ggplot2","haven","influence.ME","jsonlite","lavaan","lme4","lmerTest","mice","MVN","performance","psych","pwr","semPower","VIM","viridisLite","yaml"))
```

4. Wait for installation to complete (may take a few minutes)
5. You can close R when done

<details>
<summary>Alternative: Install via terminal</summary>

If you prefer using the terminal:

```bash
Rscript -e "install.packages(c('arrow','car','curl','DHARMa','emmeans','foreign','ggplot2','haven','influence.ME','jsonlite','lavaan','lme4','lmerTest','mice','MVN','performance','psych','pwr','semPower','VIM','viridisLite','yaml'), repos='https://cloud.r-project.org')"
```

</details>

<details>
<summary>Troubleshooting: Package installation fails on Linux</summary>

Some R packages need system libraries. Install the dependencies for your distribution:

**Ubuntu / Debian:**
```bash
sudo apt update
sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
```

**Fedora:**
```bash
sudo dnf install libcurl-devel openssl-devel libxml2-devel fontconfig-devel harfbuzz-devel fribidi-devel freetype-devel libpng-devel libtiff-devel libjpeg-devel
```

**Arch Linux:**
```bash
sudo pacman -S curl openssl libxml2 fontconfig harfbuzz fribidi freetype2 libpng libtiff libjpeg-turbo
```

Then retry the R package installation.

</details>

---

## Step 5: Install NLSS

NLSS is installed as a "skill" that your AI agent can use.

### For Codex Users

**Easiest method:** Ask your agent to install it:

> "$skill-installer Install NLSS from https://github.com/docmh/nlss.git"

**Manual method:**

1. Download NLSS:
   - Go to [github.com/docmh/nlss](https://github.com/docmh/nlss)
   - Click the green **Code** button → **Download ZIP**

2. Extract the ZIP file

3. Rename the extracted folder to exactly `nlss`

4. Move the `nlss` folder to your Codex skills directory:

   **Windows:**
   - Open File Explorer
   - Type `%USERPROFILE%\.codex\skills` in the address bar and press Enter
   - If the `skills` folder doesn't exist, create it
   - Move the `nlss` folder here

   **macOS:**
   - Open Finder
   - Press Cmd+Shift+G and type `~/.codex/skills`
   - If the folder doesn't exist, create it
   - Move the `nlss` folder here

   **Linux:**
   - Open your file manager (Files, Nautilus, Dolphin, etc.)
   - Press Ctrl+L to show the address bar, then type `~/.codex/skills`
   - Or navigate to your home folder, show hidden files (Ctrl+H), and find/create `.codex/skills`
   - Move the `nlss` folder here

5. Restart Codex

6. Verify: Type `/skills` in Codex — you should see `nlss` listed

### For Claude Code Users

1. Download NLSS:
   - Go to [github.com/docmh/nlss](https://github.com/docmh/nlss)
   - Click the green **Code** button → **Download ZIP**

2. Extract the ZIP file

3. Rename the extracted folder to exactly `nlss`

4. Move the `nlss` folder to your Claude skills directory:

   **Windows:**
   - Open File Explorer
   - Type `%USERPROFILE%\.claude\skills` in the address bar and press Enter
   - If the `skills` folder doesn't exist, create it
   - Move the `nlss` folder here

   **macOS:**
   - Open Finder
   - Press Cmd+Shift+G and type `~/.claude/skills`
   - If the folder doesn't exist, create it
   - Move the `nlss` folder here

   **Linux:**
   - Open your file manager (Files, Nautilus, Dolphin, etc.)
   - Press Ctrl+L to show the address bar, then type `~/.claude/skills`
   - Or navigate to your home folder, show hidden files (Ctrl+H), and find/create `.claude/skills`
   - Move the `nlss` folder here

5. Restart Claude Code

6. Verify: Ask "What skills are available?" — you should see `nlss`

<details>
<summary>Alternative: Install via terminal (git clone)</summary>

**Codex (macOS/Linux/WSL):**
```bash
mkdir -p ~/.codex/skills
git clone https://github.com/docmh/nlss.git ~/.codex/skills/nlss
```

**Codex (Windows PowerShell):**
```powershell
New-Item -ItemType Directory -Force -Path "$HOME\.codex\skills" | Out-Null
git clone https://github.com/docmh/nlss.git "$HOME\.codex\skills\nlss"
```

**Claude Code (macOS/Linux):**
```bash
mkdir -p ~/.claude/skills
git clone https://github.com/docmh/nlss.git ~/.claude/skills/nlss
```

**Claude Code (Windows PowerShell):**
```powershell
New-Item -ItemType Directory -Force -Path "$HOME\.claude\skills" | Out-Null
git clone https://github.com/docmh/nlss.git "$HOME\.claude\skills\nlss"
```

</details>

---

## Step 6: Verify Everything Works

Let's make sure NLSS is ready to go.

### Quick Check

Ask your agent:

> "Can you run `Rscript --version` and tell me what you see?"

You should get a response showing R version 4.x.x.

### Run the Demo

The best way to verify everything works is to run the built-in demo:

> "Run the NLSS run-demo metaskill to show me what NLSS can do."

The agent will:
1. Explain the NLSS workflow
2. Set up a demo workspace with sample data
3. Run some example analyses
4. Show you the outputs

---

# Part II — Using NLSS

## Your First Analysis

Once NLSS is installed, analyzing data is simple:

### 1. Point to your data

Tell the agent where your data is:

> "Use NLSS to analyze `C:\Users\Me\Documents\my_study.csv`"

Or for SPSS files:

> "Analyze `data/experiment1.sav` with NLSS"

**Supported formats:** CSV, SPSS (.sav), RDS, RData, Parquet

### 2. Describe what you want

Use natural language:

> "Run descriptive statistics for age and income, grouped by gender"

> "Is there a correlation between stress and performance?"

> "Compare test scores between the treatment and control groups"

### 3. Find your results

NLSS creates a workspace folder with your results:

```
your-project/
  nlss-workspace/
    my_study/
      report_canonical.md    ← Your results are here!
      analysis_log.jsonl     ← Audit trail
      plots/                 ← Any figures
```

**Tip:** Keep `report_canonical.md` open in your editor to watch results appear in real time.

---

## Understanding the Workspace

When you first analyze a dataset, NLSS creates a **workspace** — a dedicated folder for that dataset's analyses.

```
nlss-workspace/                    ← Workspace root
  nlss-workspace.yml               ← Manifest (tracks all datasets)
  my_study/                        ← One folder per dataset
    my_study.parquet               ← Fast copy of your data
    scratchpad.md                  ← Agent's planning notes
    report_canonical.md            ← All results (keeps growing)
    analysis_log.jsonl             ← Machine-readable log
    plots/                         ← Saved figures
    backup/                        ← Data backups before changes
    report_20240115_describe-sample_demographics.md  ← Full reports from metaskills
```

### Key Files

| File | Purpose |
|------|---------|
| `report_canonical.md` | Your main results file. Every analysis appends a new section. Think of it as a lab notebook. |
| `analysis_log.jsonl` | Machine-readable log of every analysis. Used for reproducibility and integrity checks. |
| `scratchpad.md` | The agent's working notes. Useful for seeing its reasoning. |
| `*.parquet` | Your data in a fast format. All analyses read from this copy. |

---

## Example Prompts

Copy and paste these to try NLSS:

### Descriptive Statistics
> "Run descriptive stats for age, income, and satisfaction, grouped by region"

### Comparing Groups
> "Compare anxiety scores between the treatment and control groups using a t-test"

> "Is there a significant difference in performance across the three training conditions? Use ANOVA."

### Correlations
> "What's the correlation between hours_studied and exam_score? Use Spearman."

> "Run a correlation matrix for all the personality variables"

### Regression
> "Predict job_satisfaction from salary, work_hours, and commute_time"

> "Run a hierarchical regression: first demographics, then add personality traits"

### Frequencies and Cross-tabs
> "Show me the frequency distribution for education_level"

> "Create a crosstab of gender by department with chi-square test"

### Factor Analysis
> "Run exploratory factor analysis on items q1 through q20"

### Full Workflows
> "Describe my sample demographics for a methods section"

> "Check all assumptions for running a regression predicting outcome from predictors A, B, and C"

> "Write a full report testing whether condition affects performance, controlling for age"

---

## Available Analyses

### Subskills (Single Analyses)

| Analysis | What it does | Example prompt |
|----------|--------------|----------------|
| `descriptive-stats` | Means, SDs, distributions | "Descriptive stats for age and score" |
| `frequencies` | Frequency tables | "Frequencies for gender and education" |
| `crosstabs` | Cross-tabulations with chi² | "Crosstab of gender by condition" |
| `correlations` | Pearson, Spearman, partial | "Correlate stress with performance" |
| `t-test` | Group comparisons | "Compare scores between groups" |
| `anova` | Multi-group comparisons | "ANOVA for outcome by condition" |
| `nonparametric` | Mann-Whitney, Kruskal-Wallis | "Non-parametric comparison" |
| `regression` | Linear/logistic regression | "Predict Y from X1, X2, X3" |
| `mixed-models` | Multilevel/repeated measures | "Mixed model with random intercepts" |
| `sem` | SEM, CFA, mediation | "CFA for my scale items" |
| `efa` | Exploratory factor analysis | "Factor analysis on survey items" |
| `scale` | Reliability (alpha, omega) | "Reliability for scale items" |
| `reliability` | ICC, kappa | "Inter-rater reliability" |
| `power` | Power analysis | "Power for detecting medium effect" |
| `assumptions` | Check statistical assumptions | "Check regression assumptions" |
| `plot` | Create figures | "Scatter plot of X vs Y" |
| `missings` | Missing data analysis | "Analyze missing data patterns" |
| `impute` | Imputation | "Impute missing values" |
| `data-transform` | Recode, compute, standardize | "Create a mean score variable" |
| `data-explorer` | Data dictionary | "Show me what's in this dataset" |

### Metaskills (Workflows)

| Workflow | What it does |
|----------|--------------|
| `run-demo` | Guided onboarding with sample data |
| `describe-sample` | Write a sample description for methods section |
| `explore-data` | Comprehensive data exploration |
| `screen-data` | Data quality checks and diagnostics |
| `prepare-data` | Data cleaning and transformation workflow |
| `check-assumptions` | Verify assumptions for planned analyses |
| `test-hypotheses` | Run and interpret hypothesis tests |
| `write-full-report` | Complete analysis with journal-style report |
| `explain-statistics` | Plain-language stats explanations |
| `explain-results` | Help interpreting NLSS output |
| `check-instruments` | Psychometric analysis of scales |
| `plan-power` | Power analysis planning |
| `format-document` | Format text in NLSS style |
| `generate-r-script` | Create standalone R script from analyses |

### Utilities

| Utility | What it does |
|---------|--------------|
| `calc` | Quick statistical calculations |
| `research-academia` | Search scholarly literature (requires network) |
| `check-integrity` | Verify log integrity |
| `reconstruct-reports` | Rebuild reports from logs |

---

## Tips for Best Results

### Be Specific
Instead of: "Analyze my data"
Say: "Run descriptive statistics for age, income, and satisfaction, grouped by gender"

### Name Your Variables
Instead of: "Compare the groups"
Say: "Compare anxiety_score between the treatment and control conditions"

### Ask for Interpretation
Add: "...and interpret the results" to get plain-language explanations.

### Use Metaskills for Complex Tasks
Instead of running analyses one by one, use:
> "Use the write-full-report metaskill to test whether training_type affects performance"

### Keep report_canonical.md Open
Watch results appear in real time and catch any issues immediately.

### Ask Questions
The agent can explain what it's doing:
> "Explain why you chose that test"
> "What assumptions should I check?"

---

# Part III — Configuration & Customization

## Configuration File

NLSS settings live in `scripts/config.yml`. Key sections:

```yaml
defaults:
  output_dir: "./nlss-workspace"    # Where workspaces are created
  digits: 2                          # Decimal places in output

logging:
  enabled: true                      # Log all analyses
  include_outputs: true              # Store output in logs (enables recovery)

modules:
  crosstabs:
    percent: "column"                # Default percentage type
  regression:
    bootstrap: false                 # Bootstrap CIs off by default
```

CLI flags override config settings for individual runs.

## Templates

Output formatting is controlled by templates in `assets/<subskill>/`. Each template is a Markdown file with YAML front matter.

To customize output:
1. Copy an existing template (e.g., `assets/descriptive-stats/default-template.md`)
2. Modify the formatting
3. Either replace the original or register your template in `config.yml`

## Logging

Every analysis is logged to `analysis_log.jsonl` with:
- Timestamp
- Parameters used
- Results (if `include_outputs: true`)
- Checksums for integrity verification

Use `check-integrity` to verify logs haven't been modified.
Use `reconstruct-reports` to rebuild `report_canonical.md` from logs.

---

# Part IV — For Developers

## Architecture

NLSS follows the [Agent Skills standard](https://agentskills.io/specification):

```
nlss/
  SKILL.md                 ← Entry point for agents
  scripts/
    R/                     ← R analysis scripts
    config.yml             ← Configuration
  assets/                  ← Templates and sample data
  references/
    subskills/             ← Documentation for each analysis
    metaskills/            ← Documentation for workflows
    utilities/             ← Documentation for utilities
  tests/                   ← Test suite
```

### Stateful Workspace Architecture

- Workspace root detected by `nlss-workspace.yml` (current dir, parent, or child)
- All data converted to Parquet for fast I/O
- `data-transform` and `missings` update data in place with automatic backups
- Non-nested workspaces enforced

### Path Handling

- Paths inside workspace: shown as relative
- Paths outside workspace: masked as `<external>/<filename>`

## Module Reference

### Subskills (R Scripts)

Each subskill has:
- **Script:** `scripts/R/<name>.R`
- **Reference:** `references/subskills/<name>.md`
- **Template(s):** `assets/<name>/*.md`

| Subskill | Script | Templates |
|----------|--------|-----------|
| `descriptive-stats` | `descriptive_stats.R` | default, robust, distribution |
| `frequencies` | `frequencies.R` | default, grouped |
| `crosstabs` | `crosstabs.R` | default, grouped |
| `correlations` | `correlations.R` | default, cross-correlation, matrix, comparison |
| `scale` | `scale.R` | default |
| `efa` | `efa.R` | default |
| `reliability` | `reliability.R` | default |
| `data-explorer` | `data_explorer.R` | default |
| `plot` | `plot.R` | default |
| `data-transform` | `data_transform.R` | default |
| `missings` | `missings.R` | default |
| `impute` | `impute.R` | default |
| `assumptions` | `assumptions.R` | ttest, anova, regression, mixed-models, sem |
| `regression` | `regression.R` | default |
| `power` | `power.R` | default |
| `mixed-models` | `mixed_models.R` | default, emmeans |
| `sem` | `sem.R` | default, cfa, mediation, invariance |
| `anova` | `anova.R` | default, posthoc, contrasts |
| `t-test` | `t_test.R` | default |
| `nonparametric` | `nonparametric.R` | default, posthoc |
| `init-workspace` | `init_workspace.R` | default |
| `metaskill-runner` | `metaskill_runner.R` | default, finalization |

### Metaskills

Metaskills are agent-run workflows documented in `references/metaskills/`. They chain subskills and produce comprehensive reports.

Metaskill completion writes:
1. `report_<YYYYMMDD>_<metaskill>_<intent>.md` — Full report
2. Synopsis appended to `report_canonical.md` via `metaskill-runner --synopsis`

### CLI Usage Examples

<details>
<summary>Descriptive Statistics</summary>

```bash
Rscript scripts/R/descriptive_stats.R \
  --csv data.csv --vars age,score --group condition
```

</details>

<details>
<summary>Correlations</summary>

```bash
Rscript scripts/R/correlations.R \
  --csv data.csv --vars age,score,stress --method spearman
```

</details>

<details>
<summary>Regression</summary>

```bash
Rscript scripts/R/regression.R \
  --csv data.csv --dv outcome --blocks "age,gender;stress,trait"
```

</details>

<details>
<summary>ANOVA</summary>

```bash
Rscript scripts/R/anova.R \
  --csv data.csv --dv outcome --between group
```

</details>

<details>
<summary>SEM/CFA</summary>

```bash
Rscript scripts/R/sem.R \
  --csv data.csv --analysis cfa --factors "F1=item1,item2;F2=item3,item4"
```

</details>

<details>
<summary>Mixed Models</summary>

```bash
Rscript scripts/R/mixed_models.R \
  --csv data.csv --formula "score ~ time + (1|id)"
```

</details>

See individual reference docs in `references/subskills/` for full CLI options.

## Testing

### Smoke Tests

```bash
# Unix/WSL
bash cmdscripts/tests.sh smoke

# Windows PowerShell
.\cmdscripts\tests.ps1 smoke
```

Tests read from `tests/tests.yml` and output to `outputs/test-runs/<timestamp>/`.

### Value Tests (Golden Files)

Statistical modules include golden-value tests for numerical correctness:

1. Generate goldens with independent R scripts in `tests/values/`
2. Compare against `analysis_log.jsonl` outputs
3. Python checkers in `tests/values/check_<module>_golden.py`

### Prompt Robustness Testing

For batch testing prompts through Codex CLI:

```bash
# WSL/bash
./tests/prompt-robustness/run_prompts.sh --cd "/path/to/workspace" --effort medium

# PowerShell
.\tests\prompt-robustness\run_prompts.ps1 --cd "C:\path\to\workspace" --effort medium
```

## Contributing

NLSS was developed with AI assistance for drafting and iteration. All changes are curated, reviewed, and tested by the human maintainer.

### Requirements

- R 4.4+
- `Rscript` on PATH
- Base R packages: `base`, `stats`, `utils`, `graphics`, `grDevices`, `tools`
- CRAN packages: `arrow`, `car`, `curl`, `DHARMa`, `emmeans`, `foreign`, `ggplot2`, `haven`, `influence.ME`, `jsonlite`, `lavaan`, `lme4`, `lmerTest`, `mice`, `MVN`, `performance`, `psych`, `pwr`, `semPower`, `VIM`, `viridisLite`, `yaml`

---

# Troubleshooting

## "Rscript is not recognized" (Windows)

R's installer doesn't always add `Rscript` to your PATH. Fix it:

1. Find your R installation:
   - Open **R** from the Start menu
   - Type `R.home("bin")` and press Enter
   - Note the path shown (e.g., `C:\Program Files\R\R-4.4.0\bin`)

2. Add to PATH:
   - Press Windows key, type "environment variables"
   - Click **Edit the system environment variables**
   - Click **Environment Variables...**
   - Under "User variables", select **Path** → **Edit** → **New**
   - Paste the path from step 1
   - Click OK three times

3. Restart your IDE

## "Rscript not found" (macOS)

After installing R, restart your terminal or IDE. If still not found:

```bash
echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

## "Rscript not found" (Linux)

R should be on your PATH after installation. If not:

1. Check if R is installed: `which R` or `whereis R`
2. If installed but not found, add to your shell config:

   **bash (~/.bashrc):**
   ```bash
   echo 'export PATH="/usr/bin:$PATH"' >> ~/.bashrc
   source ~/.bashrc
   ```

   **zsh (~/.zshrc):**
   ```bash
   echo 'export PATH="/usr/bin:$PATH"' >> ~/.zshrc
   source ~/.zshrc
   ```

3. Restart your terminal or IDE

## NLSS skill not showing up

1. Verify the folder structure:
   ```
   ~/.codex/skills/nlss/SKILL.md      # Codex
   ~/.claude/skills/nlss/SKILL.md     # Claude Code
   ```
   The `SKILL.md` file must be directly inside the `nlss` folder.

2. Restart your agent completely (not just the conversation)

3. Check for typos in the folder name — it must be exactly `nlss`

## Package installation fails

### Windows
- Make sure you're running R as Administrator for system-wide installs
- Or install to user library (R will prompt you)

### macOS
- Install Xcode Command Line Tools: `xcode-select --install`

### Linux
- Install system dependencies first (see Step 4)

## Agent can't find my data file

- Use absolute paths:
  - **Windows:** `C:\Users\Me\Documents\data.csv`
  - **macOS:** `/Users/me/Documents/data.csv`
  - **Linux:** `/home/me/Documents/data.csv`
- Or use relative paths from where the agent is running
- Check that the file actually exists at that path

## "Network access required" for research-academia

The `research-academia` utility needs internet access. In Codex:
1. Open settings
2. Enable network access, or add to `config.toml`:
   ```toml
   [sandbox_workspace_write]
   network_access = true
   ```

## Results look wrong

1. Check `scratchpad.md` to see the agent's reasoning
2. Look at `analysis_log.jsonl` for exact parameters used
3. Ask the agent: "Explain what analysis you ran and why"

## Still stuck?

- Check the detailed reference docs in `references/`
- Ask the agent: "Help me troubleshoot NLSS"
- Report issues at [github.com/docmh/nlss/issues](https://github.com/docmh/nlss/issues)

---

# License & Legal

## License

NLSS is licensed under the **Apache License, Version 2.0**. See `LICENSE` for details.

## Trademark

NLSS™ is a trademark of Mike Hammes. The Apache License 2.0 does not grant permission to use the NLSS™ name beyond reasonable use to describe origin. See `TRADEMARKS.md`.

## Dependencies

NLSS uses R packages from CRAN installed by the user. No third-party code is bundled.

## Disclaimer

- Provided "AS IS" under Apache-2.0; no warranties
- Users are responsible for validating results
- Not intended for safety-critical decisions without independent verification
- Modified versions may behave differently

## Maintainer

Mike Hammes (mike.hammes@mikehammes.name)

## Cite

If you use NLSS in published research, please cite:

Hammes, M. (2026). docmh/nlss: NLSS [Software]. Zenodo. https://doi.org/10.5281/zenodo.18173833

---

**Find detailed testing information at [github.com/docmh/nlss-demo](https://github.com/docmh/nlss-demo)**
