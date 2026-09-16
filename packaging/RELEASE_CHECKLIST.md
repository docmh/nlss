# Maintainer release checklist

`CHANGELOG.md` in the repository root is the single public change history;
derive GitHub release notes from its selected version section. Do not maintain
a second `RELEASE_NOTES.md` or substitute a raw commit list. README introduces
the product and links the history; the installation guide owns setup details.

1. Select the exact working-tree payload and the single version in
   `scripts/config.yml`. Review dirty/untracked files; never assume Git HEAD
   contains the completed runtime. Preserve local edits.
2. Select tests from `tests/tests.yml`. Reuse unchanged dependency/scientific
   evidence by source hash; run affected checks, not the whole model matrix for
   documentation changes. Retain failures and actual exit statuses.
3. Build with `python3 scripts/build_release.py --out /new/empty/output` outside
   the source tree. Verify `SHA256SUMS`, `release.json`, license/notices, demo
   inputs, manifests and internal runtime links. Exclude private projects,
   credentials, review files, caches and test outputs. Verify both archives
   contain the same scientific payload.
4. Exercise packaged invocation in spaces/Unicode paths, standalone lifecycle
   and the selected native managers. Use temporary installs/profiles; never
   replace personal installations just for a test. Removal must not touch R,
   other skills/settings or research projects.
5. For a new runtime/route, complete `tests/phase5/acceptance.md`: native discovery, bounded research
   journey, numerical/semantic/artifact review, no-write follow-up and historical
   project preservation after update. Record exact client surface/version/model
   and OS. A plugin listing alone is not successful research execution. For
   documentation/version-only releases, reuse unchanged runtime acceptance by
   source identity, rerun packaging/link checks and the actual public-download
   installation probe; do not repeat every harness or statistical module.
6. Freeze the tested artifact checksums. If only delivery documentation changes,
   rebuild, enumerate that delta and confirm executable/config/template/semantic
   instruction hashes are unchanged; rerun package/link checks as appropriate.
   Do not call earlier archive checksums the final release's identity.
7. Hand off a support matrix and evidence summary, limitations and measured
   resource use. Obtain an explicit support-scope decision for missing required
   live checks. Keep selected prior releases for rollback, not a permanent
   automatic backup stack.
8. Publishing is a separate approval: repository commit/push, tag, release upload
   and marketplace submission are not part of local assembly/acceptance. Agree
   the distribution route before doing any of them.

## Public GitHub release

1. Obtain human approval of the README and selected release scope. Confirm the
   version/tag and the explicit files to commit, including previously untracked
   runtime/tests needed by the release. Exclude private data, review material,
   caches, test/build outputs and credentials from the **commit** too: GitHub
   publishes source archives in addition to the allowlisted runtime ZIPs.
   Preserve unrelated/unselected local changes. Never blindly stage everything.
2. Finalize `Unreleased` as the selected version and actual publication date in
   `CHANGELOG.md`; add its previous-tag-to-new-tag comparison link and leave a
   new `Unreleased` section. Keep historical tag names unchanged. Replace the
   preparation notices in README/installation and finalize the citation message
   and release date. Do not assign an invented version DOI. The existing
   `10.5281/zenodo.18173833` is a concept DOI; its older archived version is not
   the new release. Check whether existing repository integrations would also
   archive publication; an independent new deposit is not part of this checklist.
3. After explicit commit authorization, commit the reviewed files. Build the
   final archives in an empty directory from a clean, separate checkout of that
   exact commit. Require its identity and clean status in `release.json`.
   Compare against the accepted candidate, allowing only reviewed changes;
   do not relabel old archives or call old checksums final.
4. With explicit push/tag/upload authorization, publish the commit and a new
   tag pointing to it; never move an existing release tag or force-push.
   Publish a GitHub release with the matching Changelog section, installation
   link, `nlss-<version>-plugin.zip`, `nlss-<version>-skill.zip`, `release.json`
   and `SHA256SUMS`. Do not call it published before the public record exists.
5. Download the public assets anew and compare all final SHA-256 values. Check
   the README → guide → release route. Perform one isolated fresh-agent
   installation/activation and a small raw-data project/analysis check from
   that download; verify the standalone archive in an isolated destination.
   Record exact harness/model/OS and restore only test registrations. Do not
   replace personal installations or repeat the full scientific/harness matrix.
6. Record the release URL, commit/tag, checksums, tests, retained support limits
   and actual effort. If a distributed defect is found, disclose it and agree
   a correction/patch release; do not silently replace bytes under the same version.

End-user install/update/remove/rollback instructions:
[installation](../references/installation.md). No MCP service, platform-specific
statistics tree or new project migration belongs in release maintenance.
