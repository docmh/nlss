---
name: statistic-skills
description: Use when statistical analyses are requested or when Codex should route to a specific statistic skill in this repo.
license: Apache-2.0
compatibility: R 4.0+, Windows, WSL (Ubuntu), Linux
metadata:
  author: Prof. Dr. Mike Hammes, ISM International School of Management, Germany (mike.hammes@ism.de)
  version: 0.1.0
  created: 2025-12-20
  backend: R
  style: APA 7
---

# Statistic Skills (Shared Workflow)

## Overview

Central guidance for all statistic skills in this repo, plus shared conventions for running R scripts and placing outputs.

## Shared wrapper: `scripts/run_rscript.ps1`

On Windows, the wrapper prefers WSL (Ubuntu/Linux) and falls back to Windows `Rscript.exe` if WSL fails. Pass the `.R` script path as the first argument.

```powershell
powershell -ExecutionPolicy Bypass -File <path to scripts/run_rscript.ps1> <path to scripts/<script>.R> --csv <path to CSV file> --vars age,score --out <working directory>\outputs\tmp
```

If the script path is omitted, the wrapper falls back to the default configured inside the wrapper script.

## WSL/Linux direct usage (optional)

Inside WSL or Linux, run `Rscript` directly with the same arguments.

```bash
Rscript <path to scripts/<script>.R> --csv <path to CSV file> --vars age,score --out <working directory>/outputs/tmp
```

## Output conventions

- Use `<working directory>/outputs/tmp` for scratch outputs (relative to the working directory where the script runs).
- Each skill should accept `--out` and write all generated files there.
- Keep outputs as plain text, Markdown, or CSV so Codex can summarize them.

## Subskills

- [descriptive-stats-r](references/descriptive-stats-r.md): APA 7 style descriptive statistics using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts.
- [frequencies-r](references/frequencies-r.md): APA 7 style frequency tables for categorical variables using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts.
- [crosstabs-r](references/crosstabs-r.md): APA 7 style cross-tabulations with chi-square or Fisher tests and association measures using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts.
- [correlations-r](references/correlations-r.md): APA 7 style correlation analyses (Pearson/Spearman/Kendall), cross-correlations, partial correlations, and p-value adjustments using base R for CSV inputs, R data frames (RDS/RData), or interactive prompts.
