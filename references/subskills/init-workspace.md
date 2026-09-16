---
name: init-workspace
description: Retired project initializer; current NLSS projects are created explicitly with project-create.
license: Apache-2.0
---

# Project initialization

Do not use `init_workspace.R` to create or reinitialize a current NLSS project.
Its older marker writer is unsupported by the common project boundary; there
is no adoption, old-version reader or conversion workflow.

The agent uses [project-create](../utilities/project-create.md) as part of NLSS
work in the user's chosen unmarked folder; no separate initialization request
or manual conversion is required from the researcher or agent. Registration takes
CSV/SAV/RDS/RData/Parquet directly, supplies a visible working Parquet and returns
structured setup information. It preserves the original and publishes the marker
explicitly; matching repeated calls reuse existing data without resetting edits.

For an existing current project, run the requested analysis using explicit
`--project`/`--dataset` selection or nearest-ancestor discovery. The common
publisher automatically extends the root `report_canonical.md`; initialization
is not a prerequisite for each analysis. Ordinary CSV/SAV/RDS/RData/Parquet data
inputs remain supported by the common loader.

Use [project-inspect](../utilities/project-inspect.md) for read-only navigation.
Do not delete or rename an older project's files merely to make creation work.
