# Independent study-planning boundary acceptance

Run `Rscript tests/phase2/run_planning_tests.R --root /tmp/nlss-planning-check --keep 0`.
The runner honors `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS` and `NLSS_TESTS_CONFIG`.
`--match REGEX` selects named groups; `_smoke$` is a four-group subset. An empty
selection exits nonzero. Do not report subset results as a complete acceptance run.

This suite independently exercises the public `power.R`, `replay_run.R`,
`init_workspace.R` and `descriptive_stats.R` commands in private offline projects. It does not source
NLSS internal functions. Scientific numerical equivalence across every Power
family/mode is covered by the separate Power acceptance suite; this suite tests
the input, ownership, provenance, publication, privacy and replay boundary.

Coverage includes:

- Parameter-only schema-v2 requests/results, explicit null dataset, owned
  `planning/runs/<id>` artifacts and SHA-256 associations; no fabricated Parquet,
  dictionaries, source snapshots or dataset-manifest entries.
- Empty projects, configured-root fallback without a manifest, project and
  dataset working directories, and independence from an unrelated active dataset.
- Explicit dataset inputs and `--planning FALSE` retain verified dataset-backed
  schema-v1 execution and replay. Conflicting source/estimation/planning choices
  fail rather than silently bind or discard data. A source-import rename through
  `--dataset-name` cannot silently select the unrelated active dataset.
- Explicit inapplicable scientific/import flags and surplus estimation roles
  fail, while unrelated canonical family defaults remain harmless. Variable
  names without effect estimation are disclosed context, not inferred data use.
- Canonical `--sem-df` and the legacy numeric `--df` alias survive replay.
  RData object selection remains distinct from SEM degrees of freedom.
- Frozen configuration/templates, raw results despite legacy logging opt-out,
  disabled-prompt privacy, enabled-prompt/effect-basis path masking and exclusion
  of unrelated module configuration.
- A private subprocess library proves parameter-only runs/replay work without
  `arrow` or `haven`; missing `pwr`/`semPower` fail explicitly without fake output.
- Invalid calculations cannot overwrite protected report/log/manifest bytes;
  failed bundles cannot be replayed or expose normal completed output. Competing
  analysis/publication locks retain the other owner's files.
- Planning-path and dataset-name collisions in both directions, directory/file
  ownership and symlink escapes. Symlink tests require platform permission to
  create symlinks; an unsupported capability is an explicit test failure.
- Tampered requests, templates and deterministic output; code/R/platform,
  package/system-library and locale/timezone mismatches; malformed parameter
  input, forged dataset bindings, unsupported module, traversal/absolute artifact
  paths, renamed/pending runs and missing/invalid ownership markers.

The resigned-request negative fixtures deliberately update a local request hash
to reach deeper validation gates. Hashes are integrity/association checks, not
external signatures or protection from coordinated malicious rewriting.

`phase2-planning/run-*/results.json` records the exact selected cases, runtimes,
failures, test hash, R-source inventory and R/package versions. Run replay checks
on a frozen R tree: concurrent source edits correctly make exact replay fail.
Passing these execution checks does not validate study-design assumptions, an
effect-size rationale or the adequacy of a planned study. Deterministic output
remains evidence for a freely structured semantic research/planning report.
