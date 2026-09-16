# Phase 3: current project management acceptance

Registered in `tests/tests.yml`. Run one consolidated selection after sources
are stable:

```bash
bash cmdscripts/tests.sh phase3 --root /tmp/nlss-project-check --keep 0
```

The existing shell harness runs the inspection/location runner, the
[persistence runner](PERSISTENCE.md), and the [report runner](REPORTS.md)
sequentially. Do not run these stateful harnesses concurrently in one checkout.
For affected cases during development, call the relevant R runner directly:

```bash
Rscript tests/phase3/run_project_tests.R --root /tmp/nlss-location-check --keep 0 --match '^task_a_'
```

All three support `--match REGEX`, `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and
`NLSS_TESTS_CONFIG`. Explicit roots are never pruned. Native Windows validation
is claimed only if actually run, using the existing PowerShell/Git Bash harness
or the R runners directly.

## Fixed acceptance coverage

- All 21 registered statistical entrypoints are parsed for shared bootstrap,
  execution, input-binding and result calls, without a per-module storage writer.
  Source identities are recorded; this is interface/routing acceptance, not
  exhaustive scientific revalidation of unchanged methods.
- Locations: current creator, explicit/nearest ancestor, no child/sibling search,
  standalone/parameter-only inputs, missing selections and whole-root moves.
- Inspection: current descriptors, honest missing/malformed/mismatched states,
  optional hashes, inert YAML, contained paths, symlink/FIFO boundaries,
  configured CLI defaults and byte/mtime preservation.
- Publication and numerics: regression versus independent lm, all existing source
  formats, Power versus pwr, utility output, visible transformations and conflict
  handling, plot data/links, imputation-to-pooling and current-layout replay.
- Root protocol: automatic successive entries, tables, warnings/status, evidence
  and figure links, explicit reconstruction without recalculation/duplicates,
  and failure without overwriting authored prose.
- Reports: ordinary delivery, current-layout revisions, shared objects, read-only
  discovery, real optional lifecycle utility, missing evidence and safe writes.

The complete runners now target the agreed layout, not the retired pilot.
Removed expectations: legacy manifest-shape/import compatibility, per-dataset
run inventories, child-marker discovery, two-module storage restrictions and
dedicated note capture. Equivalent current-layout safety checks remain;
statistical value/golden suites are unchanged.

The ordinary `tests.phase3.study_document` fixture is synthetic input and a file
whose bytes must remain user-owned. Its example filename is not a feature.
The acceptance handoff also performs one manual agent-driven document → analysis
→ authored Markdown exercise; no deterministic report-generation framework.

Configuration, faults and fixture edits stay in private runner directories or
local R environments. Existing projects and repository configuration/templates
are never rewritten. Each `results.json` records command, source/fixture hashes,
environment, timestamps, assertions, skips and actual subprocess exits. A source
drift gate prevents treating a mixed-revision run as passing evidence.
