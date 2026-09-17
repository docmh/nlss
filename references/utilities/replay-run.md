---
name: replay-run
description: Verify and repeat a completed saved NLSS analysis request against its immutable input and recorded execution environment, producing a new run without an AI model.
license: Apache-2.0
---

# Replay run

## Overview and intent

Use `replay-run` through the shared `run_nlss.R` launcher when the researcher requests an exact repeat of a
completed migrated analysis. Explaining an explicitly selected historical result
does not itself require replay. See [run contract](../run-contract.md) for scope,
verification gates and failure handling.

## Inputs, options and execution

```bash
Rscript "<skill>/scripts/R/run_nlss.R" replay-run --request /path/to/project/.nlss/runs/<run-id>/request.json
```

`--request` is required; `--help` lists usage. No network access, arbitrary R code,
module switch, changed data, changed scientific options or environment overrides
are accepted. For a revised analysis, run its normal module with the new choices.

## Behavior and outputs

Verify the completed request, input and environment; invoke the corresponding
registered R entrypoint; create a new run in that project's `.nlss/runs/`,
referencing the original run ID. Dataset-backed and parameter-only Power records
share this destination without sharing their input schemas. The selected request,
not the invocation directory's active dataset, identifies the replay project.
The original request/result and current working Parquet remain unchanged; the
new run automatically appears in the root `report_canonical.md`.
Missing artifacts or incompatible
code/packages cause an explicit error rather than an approximate silent rerun.

The current project marker and `.nlss/runs/` layout are required. There is no
old dataset-local/planning-directory reader, conversion, or cross-version replay.
Moving the complete project preserves relative input/template/artifact references;
moving just a request file does not. Ordinary new standalone analyses remain
available, but an unmarked run is not eligible for this project replay interface.

For data transformations, replay also verifies the preserved output data and
dictionary, never activates the recalculated output or rewrites the current
preview/backups. General `--calc` expressions outside the explicit replay subset
are refused before evaluation; use the normal module only for an authorized new
change. See [data-transform](../subskills/data-transform.md).

Missingness and imputation also replay without activating their old output.
Imputation additionally verifies the preserved mids object and metadata, compares
recomputed scientific content and reuses the existing artifact; this does not
claim identical serialization of runtime call/date fields. See
[impute](../subskills/impute.md) for seeded engines and inference limits.
Its `.nlss/imputations/<id>/` bytes are reused, not copied per replay or pooled
model. Plot images, numerical layers and saved model fits remain in their owning
run; the root protocol links those artifacts directly. Pixel-identical rendering
across graphics environments is not promised.

## Non-goals and dependencies

This utility is not a dependency installer, project migration, arbitrary-code
executor or new report-writing agent. It requires R and the packages recorded by
the selected supported module. No template is needed for the utility itself;
the analysis reuses its frozen templates and remains the source of numeric output.
