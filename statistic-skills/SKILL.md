---
name: statistic-skills
description: Use when statistical analyses are requested or when Codex should route to a specific statistic skill in this repo.
---

# Statistic Skills (Shared Workflow)

## Overview

Central guidance for all statistic skills in this repo, plus shared conventions for running R scripts and placing outputs.

## Shared wrapper: `run_rscript.ps1`

On Windows, the wrapper prefers WSL (Ubuntu/Linux) and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File statistic-skills\run_rscript.ps1 statistic-skills\<skill-name>\scripts\<script>.R --csv data.csv --vars age,score --out statistic-skills\outputs\tmp
```

If the script path is omitted, the wrapper falls back to the default configured inside the wrapper script.

## WSL/Linux direct usage (optional)

Inside WSL or Linux, run `Rscript` directly with the same arguments.

```bash
Rscript statistic-skills/<skill-name>/scripts/<script>.R --csv data.csv --vars age,score --out statistic-skills/outputs/tmp
```

## Output conventions

- Use `statistic-skills/outputs/tmp` for scratch outputs.
- Each skill should accept `--out` and write all generated files there.
- Keep outputs as plain text, Markdown, or CSV so Codex can summarize them.

## Skills

- descriptive-stats-r: APA 7 style descriptive statistics using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts. (file: statistic-skills/descriptive-stats-r/SKILL.md)
