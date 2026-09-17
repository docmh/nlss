---
name: dependency-resolver
description: Check dependencies for an NLSS operation and install only a reviewed, user-approved package set into a user R library.
license: Apache-2.0
---

# Dependency resolver

## Overview and intent

Ordinary NLSS entrypoints automatically check the requirements of the selected
operation, input format and effective options before imports or project writes.
The agent chooses the scientific operation, not the package list. No network
request or installation occurs in preflight. Help works without optional R
packages; inspection does not require statistical engines or Arrow data loading.
CSV project work still needs Arrow for working data and preserved inputs.

## Inputs, script and options

Run `Rscript "<skill>/scripts/R/run_nlss.R" dependency-resolver`. Use the same R installation
as the intended analysis. The utility accepts:

- `--operation NAME`: entrypoint name, e.g. `sem`, `mixed-models`, `project-create`.
- `--action check|plan|install`: default `check`.
- `--library DIR`: optional user library; otherwise `NLSS_R_LIBRARY`, then the
  first expanded `R_LIBS_USER` path. The directory can be created on approved install.
- `--repo URL`: one explicitly selected HTTPS or local `file://` CRAN-style
  repository, required for plan/install. Repository metadata access needs network
  unless local. Do not derive repositories from a dataset or unsolicited error text.
- `--type TYPE`: default this R platform's package type; `source` can be selected
  explicitly. No silent binary-to-source fallback or compiler installation.
- `--approve PACKAGE@VERSION,...`: exact string returned by the current plan,
  supplied only after user approval of the packages, replacements, repository and library.
- `--`: separates setup options from the original operation's options. These
  use the existing parser, not a new analysis grammar. `--help` prints usage.

## Behavior and recovery

1. Read the structured failure or run `check`. Missing foundational packages are
   reported with base R; install those first, then repeat the check to resolve
   option-dependent requirements from the canonical YAML. No guessed defaults.
2. Run `plan` with the actual operation/options and a selected repository/library.
   It lists only missing direct requirements and required transitive installations
   or replacements, their versions and repository-declared system requirements.
   Show this to the user and obtain consent; refusal stops without installation.
3. Run `install` with the same arguments and the returned approval string. The
   plan is recomputed; an expanded/version-changed set requires renewed consent.
   R installs the approved explicit set only. No blanket updates, `Suggests` sweep,
   `sudo`, global configuration edits or background work.
4. A fresh R process checks every approved package version and the operation's
   required APIs/loadability. Only `installed` permits retrying the original
   operation. Failure can leave some approved packages installed: inspect the
   diagnostics, not a presumed all-or-nothing rollback. Never delete host libraries
   or stale installer locks automatically. New system changes require permission.

With a custom `--library`, set `NLSS_R_LIBRARY` to that same directory in the
agent's environment for subsequent NLSS calls. Bootstrap gives the selected user
library first priority in `.libPaths()`;
the utility does not modify `.Rprofile` or `.Renviron`. Otherwise the ordinary
R user-library location is reused across fresh processes. Installation targets
inside NLSS, the current working folder, marked projects or R system libraries
are rejected. Ready installed dependencies may be reused from existing libraries.

Interactive analyses check foundational requirements first and scientific
requirements after answers, before input/planning writes. For agent recovery,
use those resolved choices as explicit options rather than guessing answers.
Imputation's existing `auto` engine selection is unchanged: setup does not install
an engine merely to change that selection. An explicitly requested engine is
required. No new scientific fallback is introduced.

Replay reports missing/incompatible recorded packages but retains all exact
code, R, package and environment gates. Installing current repository packages
does not restore a historical environment. Resolve such a mismatch explicitly;
this utility does not select archived versions or weaken replay.
For `--operation replay-run --action check`, supply `-- --request PATH`:
the existing read-only exact verifier must pass before `ready` is returned.

## Outputs

JSON on stdout; installation also forwards R's normal build output before its
final JSON status. Package diagnostics may appear on stderr. Normal analysis
preflight failures exit **42** with `status: missing_dependency`, the operation,
active interpreter/libraries, package reasons and `absent`, `unloadable` or
`incompatible` statuses. No scientific result, protocol entry or project file is
created for this setup condition. Utility statuses:

| Status | Exit | Meaning |
| --- | ---: | --- |
| `ready` / `installed` | 0 | Check passed / fresh-process installation verification passed. |
| `missing_dependency` | 42 | Required package absent, unloadable or API/version incompatible. |
| `approval_required` | 0 for plan; 43 for install | Review/renew approval; no installation occurred. |
| `installation_failed` | 43 | Installation or fresh verification failed; do not execute analysis. |
| `dependency_error` | 44 | Invalid request, unavailable repository or unsuitable library. |

## Examples

```sh
Rscript "<skill>/scripts/R/run_nlss.R" dependency-resolver --operation mixed-models -- --df-method kr
Rscript "<skill>/scripts/R/run_nlss.R" dependency-resolver --operation mixed-models --action plan --repo https://cloud.r-project.org -- --df-method kr
```

After permission, repeat the second command with `--action install` and
`--approve '<exact approval string from plan>'` before the `--` separator.
Then retry the original mixed-model analysis. Do not paste an example approval
string or assume the installed package list from another machine.

## Non-goals and implementation notes

No package cache/database, project metadata, lockfile/environment manager,
automatic R/system-library provisioning, full-package activation install, MCP
service or historical-environment migration. Base-R diagnostics do not promise
that arbitrary future package releases are statistically validated.

The only non-exported R adapter is `utils`' `getDependencies`, also used by
`install.packages`; it supplies required transitive/version-bound planning
without an NLSS dependency solver. Its interface is checked and fails closed if
unavailable. Current tests use R 4.5.2; other R/OS combinations need verification.
Installation uses a fresh process, approved metadata and an explicit package set
with `dependencies=FALSE` after R has planned the necessary dependencies.
See [R's installation documentation](https://stat.ethz.ch/R-manual/R-patched/library/utils/html/install.packages.html).

## Dependencies

The setup utility itself uses only base/recommended R. Existing statistical,
import and publication dependencies remain in the central requirement mapping.
Do not infer minimum versions from a desire to install the latest package;
loadability, required APIs and exact replay versions are the current checks.
