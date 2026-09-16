---
name: install-nlss
description: Preview and explicitly approve standalone NLSS skill installation, update or removal without changing R packages or research projects.
---

# Install NLSS (standalone utility)

## Overview and intent

Use for an explicitly selected standalone skill installation. Native plugins
use their harness manager. Read [installation and maintenance](../installation.md)
for supported routes, commands, update/reinstall behavior and current limits.
This utility is not called on skill activation or for each research folder.

## Inputs, script and options

`scripts/R/install_nlss.R` accepts:

- `--action status|install|update|remove` (default `status`).
- `--source DIR`: selected unpacked release skill, required for install/update.
- `--harness codex|vibe|claude`: documented user skill destination; **or**
  `--destination DIR`: explicit final folder, named `nlss`.
- `--approve`: write only after the researcher has approved the action.
- `--help`: base-R usage, without touching the filesystem.

## Behavior and outputs

No approval flag means validation/preview only. Stdout identifies the action,
version, location and reload requirement; exit 0 means success/preview, exit 1
means refusal/failure. Same payload/version repeats are no-ops. Sources and
existing destinations must have their complete generated release inventory.
Changed/extra/missing files or linked trees are refused, not overwritten.
Update stages the selected release beside the destination, checks copied bytes,
then switches directories with temporary rollback on activation failure. Normal
completion removes that temporary old copy. A process interruption can leave a
named temporary folder; inspect it before manual recovery, do not blindly rerun
cleanup. Removal affects only the recognized selected NLSS folder.

## Example

```text
Rscript "/unpacked/nlss/scripts/R/install_nlss.R" --action install --source "/unpacked/nlss" --harness vibe
```

Review the preview, obtain approval, and repeat with `--approve`.

## Non-goals and implementation dependencies

Base/recommended R and the existing shared CLI only. No package download,
archive extraction, dependency installation, native-manager/configuration edits,
project bootstrap, scientific run/output, old-checkout adoption or force deletion.
The inventory detects copy errors and accidental edits; it is not a signed
trust boundary. Updates do not preserve historical R/code replay environments.
