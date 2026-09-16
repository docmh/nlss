# Workspace and metaskill lifecycle acceptance

Run the focused public-CLI suite:

```sh
Rscript tests/phase2/run_lifecycle_tests.R --root /tmp/nlss-lifecycle-tests --keep 0
Rscript tests/phase2/run_lifecycle_tests.R --root /tmp/nlss-lifecycle-tests --keep 0 --match 'metaskill|late_log_failure'
```

`--root` / `NLSS_TEST_ROOT`, `--keep` / `NLSS_KEEP_RUNS`, and registry defaults are supported. `--keep 0` retains all runs; `--match` selects case names. The runner copies the installation into its disposable run folder, uses per-case `NLSS_CONFIG_PATH`, and records initial source hashes, exact child CLI arguments/exit codes/timestamps, assertion counts, and final source stability. It never changes the working installation's config/templates.

Fixtures independently assert CSV/SAV/RDS/RData/Parquet imports, SPSS label preservation, multiple sources/objects, regional CSV options and explicit templates/agents, empty workspace initialization, exact active input/version references, unchanged-source reuse of edited working data, preserved scratchpads, and explicit source-version transitions. Negative cases cover missing/invalid sources, colliding names, missing templates, publication locks, and symlink projections.

Metaskill cases verify missing-report preflight, dataset discovery, custom phases, synopsis, optional legacy logging, exact authored Markdown bytes/hash, and the legacy reconstruction payload. Semantic report quality is deliberately **not** judged by a fixed reporting template or a statistical numerical golden.

Module-level fault cases copy the already frozen installation again and inject an exception or a silent `FALSE` after report/log append and checksum-counter update. They verify byte restoration of the manifest, canonical report, log, scratchpad, working data and source binding; the authored report stays untouched. Failed utility evidence remains, and the owned publication lock is released. Shared helper fault coverage is maintained separately by the utility contract tests.

One representative orchestration case follows labelled SPSS user-missing input through initialization, descriptives, data transformation, MICE, pooled Gaussian MI regression, frozen statistical/MI replay, semantic finalization, report reconstruction, and integrity checking. It checks exact source/version/artifact identities and UTF-8/CRLF semantic reconstruction under a C locale. It complements the modules' independent numerical goldens; it does not itself certify model choice or interpretation quality.

Initialization imports commit per dataset before the protected lifecycle publication. The suite does not claim atomic rollback of all imported datasets or resilience to abrupt process termination. No statistical replay or automatic semantic regeneration is claimed for either utility event.
