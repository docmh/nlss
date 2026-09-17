# Install and maintain NLSS

Install once for your chosen local agent, then open any research folder and ask,
for example: “Use NLSS to describe the sample in survey.sav.” The agent handles
project setup/import and asks before installing missing R packages. You need R
(`Rscript` on the agent's PATH), not Python or Node just to run NLSS.

**[NLSS 2.0.1](https://github.com/docmh/nlss/releases/tag/v2.0.1)** provides the
plugin and standalone-skill packages described here. The local
runtime acceptance was completed on 16 September 2026 with explicit approval
of the support limitations below. Linux package/maintenance tests
exercise spaces and Unicode paths; all five primary harness routes were
exercised, with additional short Codex IDE/App activation checks. **Antigravity
is functionally tested with documented limitations, not unconditionally
accepted.** Native Windows/macOS are prepared, not verified. A cloud/mobile
plugin listing does not provide access to your local R or data.

## From repository link to installed release

This is the authoritative route for an agent asked to install NLSS from
`https://github.com/docmh/nlss`. Installation does not itself request a demo,
data import, project creation or analysis. Read these instructions, not internal
development/review documents or results from somebody else's test session.

1. Establish the user's actual harness/surface, operating environment and
   intended installation scope. Inspect an existing NLSS registration before
   changing it. Ask only where the choice matters; do not install a new agent
   application merely because its name appears in the support table.
2. Check `Rscript --version` in that environment. If R is absent, obtain
   approval and use the appropriate [CRAN installation](https://cran.r-project.org/)
   or trusted OS package manager, then verify PATH in a fresh process. WSL and
   native Windows have separate R installations/libraries. Do not install all
   analysis packages now; use the [dependency resolver](utilities/dependency-resolver.md)
   when an operation reports its actual missing requirements. User R libraries
   stay outside plugin caches and research projects. No blanket upgrades.
3. Resolve the requested published version, or the newest stable release at
   [GitHub Releases](https://github.com/docmh/nlss/releases/latest).
   For machine-readable discovery use
   `GET https://api.github.com/repos/docmh/nlss/releases/latest`, or
   `GET https://api.github.com/repos/docmh/nlss/releases/tags/<exact-tag>`.
   Read `tag_name`, `draft`, `prerelease` and the returned assets. Default to a
   non-draft, non-prerelease; retain this selected tag/version for all subsequent
   downloads instead of resolving `latest` again halfway through installation.
4. Select **one** route below. Use the returned `browser_download_url` for
   `nlss-<version>-plugin.zip` or `nlss-<version>-skill.zip`, plus `release.json`
   and `SHA256SUMS` from the same release. Tags may have a leading `v`; filenames
   use the version without it. Do not invent a fixed `latest/download` filename
   for versioned assets. If the assets are absent, report that the requested
   distribution is unavailable and stop; do not install the source ZIP as a
   managed package or silently downgrade. An explicitly selected local candidate
   can be tested locally, but must be labelled unpublished.
5. Check SHA-256 for the downloaded archive and `release.json` against their
   entries in `SHA256SUMS`, and check version agreement. Entries for an archive
   you did not download are not verified. Checksums detect changed bytes;
   authenticity still depends on trusting the release source. Extract the
   selected package without overwriting an existing installation.
6. Explain the destination/registration and use the chosen native manager or
   the standalone helper with the user's approval. Keep plugin catalogues at a
   stable user-owned location outside research folders, not in a disposable
   download directory. Do not activate plugin and skill copies simultaneously.
7. Report the installed version/location and how to start a fresh session.
   Verify discovery there. When the user subsequently requests research work,
   select their data folder and use the existing `project-create`/analysis route.

The release is installed once per chosen harness, not once per dataset. Network
or organization-policy restrictions should be reported with the blocked step,
not bypassed through a different provider, new credentials or unapproved host
changes. A human can perform the same download/check/unpack steps manually.

## Choose one route

Download a maintainer-built release, check `SHA256SUMS` against a trusted release
source, and unpack it. A GitHub **Code → Download ZIP** is a development checkout,
not the release described here. Publication is established by the selected
GitHub release, not by finding versioned files in a local folder.

- `nlss-<version>-plugin.zip` expands to `nlss-release/`, with local catalogue
  descriptors and **one** plugin at `plugins/nlss/`. Its skill is `skills/nlss/`.
- `nlss-<version>-skill.zip` expands to the standalone `nlss/` skill folder.
- `release.json` records the canonical version, exact payload/source hashes,
  Git HEAD and dirty status. Checksums detect changed bytes, not publisher trust.

Do not enable both plugin and standalone NLSS in the same harness. If an older
manual copy already exists, preserve any edits and deliberately retire/disable
that copy first; NLSS does not scan or delete other installations automatically.

| Harness | Prepared route | Activate |
| --- | --- | --- |
| Codex CLI / supported local desktop | Register the unpacked local catalogue, then install through the native manager (below). | Start a new session; select NLSS or ask to use NLSS. |
| Codex IDE extension | Standalone skill; `--harness codex` selects `~/.agents/skills/nlss`. Plugins are not currently supported in the extension. | Reload/start a new conversation and select `$nlss`. |
| Claude Code | Native plugin from the included local catalogue (below). | Start a new session; select `/nlss:nlss` or ask to use NLSS. |
| VS Code + GitHub Copilot | Register `nlss-release/plugins/nlss` in the **user** `chat.pluginLocations` setting, enabled (`true`). Requires Agent Plugins support and policy permission. | Reload; use the discovered NLSS skill in agent chat. |
| Google Antigravity desktop | Place the plugin folder `nlss-release/plugins/nlss` at `~/.gemini/config/plugins/nlss` using its documented global discovery route. | Reload/start a new session and ask to use NLSS. |
| Antigravity CLI | Use `agy plugin install "/path/to/nlss-release/plugins/nlss"`, then `agy plugin list`. Its documented CLI profile differs from the desktop route. | Start a new session and select the discovered NLSS skill. |
| Mistral Vibe Code | Standalone skill; `--harness vibe` selects `~/.vibe/skills/nlss`. | Restart Vibe and ask to use NLSS; skill filters must permit `nlss`. |

Paths beginning `~` mean the user's home directory. Use the corresponding native
path on Windows. Run R in the same OS/environment as the agent (WSL is a separate
Linux environment). The helper also offers `--harness claude` for an explicitly
chosen standalone fallback at `~/.claude/skills/nlss`, not alongside the plugin.

### Native local catalogues

Keep the unpacked catalogue at a stable user-owned location outside research
projects. Replace the example absolute path with your selected `nlss-release`.
These commands change your harness installation only when you choose to run them;
the release builder does not execute them.

Codex CLI (command shape checked against 0.154.0):

```text
codex plugin marketplace add "/path/to/nlss-release"
codex plugin add nlss@nlss-local
```

Claude Code:

```text
claude plugin marketplace add "/path/to/nlss-release"
claude plugin install nlss@nlss-local --scope user
```

For a temporary Claude development probe, `claude --plugin-dir
"/path/to/nlss-release/plugins/nlss"` loads the same payload, but is not evidence
of a permanent installation. Copilot and Antigravity use the plugin folder,
not the catalogue root. Their documented local-folder routes do not require an
NLSS-specific extension, MCP server or per-project copies.

### Acceptance status — 16 September 2026

These boundaries are deliberately separate. None imply native Windows/macOS
acceptance or access to local R from a remote/cloud client.

| Exact tested route | Evidence | Limitations retained at handoff |
| --- | --- | --- |
| Linux Codex CLI 0.154.0 | Native marketplace/install/list and skill discovery; packaged CSV/test/figure/report journey with independent numerical/link checks. A subsequent fresh GPT-5.6-Terra/high CLI session loaded the native cached plugin skill without a path in the prompt, gave a sound workflow explanation and left its CSV-only folder unchanged. Temporary plugin/catalogue removed after App check. | Scientific journey and short native activation check are separate evidence; the former used an explicit SKILL path. |
| Linux Claude Code 2.1.267, Claude Sonnet 5 | Native validation/isolated install; authenticated session-only plugin discovery and skill invocation; research workflow, 40 independent checks and no-write follow-up. Two authored-report claims corrected in one editorial review, preserving both revisions. | No Claude desktop/IDE acceptance implied. |
| Linux Antigravity CLI 1.2.4; Gemini, Flash High requested | Native validation, temporary install/discovery/skill loading, user-operated research outputs, 36 numerical/artifact checks, preserved editorial correction and no-write follow-up. Temporary registration removed and empty native listing confirmed. | Scope adherence failed: broad filesystem searches and attempted use of a previous test library during sandbox recovery. User-reported Wave-document exposure remains unresolved; package-only independent acceptance is qualified. Follow-up has a causal-design caveat; actual interactive model identity not independently captured; no desktop acceptance implied. |
| Linux Vibe 2.25.4; GLM 5.2, Mistral-hosted (user-reported) | Standalone installation, native discovery/skill activation via session-only path, user-operated interactive workflow, 48 numerical/artifact checks, preserved editorial correction and no-write follow-up. | Two plot-option errors recovered; model initially confused byte verification with re-execution. Earlier headless permission stops retained; no quality ranking inferred. |
| VS Code 1.137.0, Copilot Chat 0.65.0, GPT-5.6-Terra | Researcher-confirmed native skill visibility; installed-package journey; native model ID; 48 numerical/artifact checks, accepted saved English/German reports and 29-file no-write follow-up. Temporary registration setting removed. Two plot-option errors recovered. | Reviewed explicit file accesses are scoped; memory-listing content and complete context isolation are not established. No remote-client or other-OS acceptance implied. |
| Codex IDE extension 26.908.40401; GPT-5.6-Terra/high | Researcher-operated standalone activation; native VS Code session confirms loading the installed SKILL without a path in the prompt; sound workflow explanation and unchanged CSV-only folder. Temporary standalone copy removed. | Short activation/explanation probe only, not another statistical journey or IDE plugin support claim. |
| Linux Codex App; embedded CLI 0.154.0-alpha.6.2; GPT-5.6-Terra/high | Researcher-operated App response and native `Codex Desktop` session confirm cached plugin skill loading without a supplied path. Sound explanation, original CSV unchanged, no NLSS project/run/report created. Temporary plugin/catalogue removed. | Short activation probe, not another statistical journey. One unnecessary surface-inventory call; three empty directories appeared, so whole-folder identity is not claimed. Outer desktop package version and complete context isolation not independently established. |

These exact local surfaces have now been exercised; the App result is independent
of the CLI activation check. The researcher accepted this bounded support scope
on 16 September 2026, including Antigravity's functional-only qualification.
The recorded scope failure and unresolved exposure are not converted into passes;
no further live retry is part of this release acceptance. Missing checks have
not been accepted as passes. A headless exit
code of zero can still mean an approval stopped the task; inspect artifacts and
denied actions. In Antigravity explicitly select the study directory/project:
its default harness project need not match the shell working directory.

## Standalone installation helper

You can ask your agent to perform these steps with your approval. Advanced/manual
use, after unpacking the **skill** archive:

```text
Rscript "/path/to/unpacked/nlss/scripts/R/install_nlss.R" --action install --source "/path/to/unpacked/nlss" --harness vibe
```

This validates the release and previews the exact destination/version without
writing. Repeat with `--approve` after approving the proposed installation.
Choose `--harness codex` for Codex IDE, or use
`--destination "/chosen/user/skills/nlss"` **instead of** `--harness` for a custom
location. The destination is the final `nlss` folder, not its parent. The helper
needs only base/recommended R; it downloads nothing and edits no harness settings.

The returned path/version and a new harness session are the handoff. Then open
your data folder, select NLSS, and make your research request. For sample data,
ask “Run the NLSS demo”; the installed bundle includes its CSV. Never run the
demo in the installation folder.

## Agent/advanced operation invocation

Resolve the installed skill location once. Use the common base-R launcher for
operations, including dependency recovery and replay:

```text
Rscript "<installed-skill>/scripts/R/run_nlss.R" project-create --project "<existing data folder>" --source "<file inside that folder>"
Rscript "<installed-skill>/scripts/R/run_nlss.R" descriptive-stats --project "<data folder>" --dataset "<returned name>" --vars age,score
Rscript "<installed-skill>/scripts/R/run_nlss.R" dependency-resolver --operation sem -- --model "y ~ x"
Rscript "<installed-skill>/scripts/R/run_nlss.R" replay-run --request "<selected request.json>"
```

Operation names accept hyphens or underscores, without `.R`. All following
options go unchanged to the existing operation parser; `<operation> --help`
shows that operation's own help. Reference examples use this same launch form;
their `<skill>` placeholder denotes the installed skill directory. The launcher handles spaces
in the installation path, keeps the caller's working directory, and retains the
selected operation in dependency diagnostics and saved evidence. It adds no
statistical or project behavior. `install_nlss.R` remains a direct standalone
maintenance entrypoint. Researchers can simply make their natural-language
request; no manual project initialization is required.

## Update, remove, reinstall

Ask the agent to explain the selected release and destination before changing
them. Close active NLSS runs first. Keep research data and custom configuration
outside the installation; use the existing `NLSS_CONFIG_PATH` override instead
of modifying bundled files.

For **native plugins**, use the native manager for the selected installation.
Codex's registered local catalogue uses `codex plugin add nlss@nlss-local` again
after deliberately replacing the catalogue with the selected release; remove
with `codex plugin remove nlss@nlss-local`. Claude uses
`claude plugin update nlss@nlss-local --scope user` and
`claude plugin uninstall nlss@nlss-local --scope user`. Review the manager's
selected source/scope before confirming. Do not rewrite its cache with the R
helper. For Copilot's manual local registration, select the new unpacked plugin
path in the same user setting; remove only NLSS's entry to disable it. For
Antigravity's global folder route, preserve local edits, replace only its `nlss`
folder with the selected release, or remove only that folder to uninstall.
Keep a prior release until the replacement loads; remove the superseded copy
after verification rather than accumulating backups. The exact route limits
are recorded in the acceptance matrix above.

For Antigravity **CLI**, use its native `agy plugin install` with the selected
replacement and `agy plugin uninstall nlss` for removal; inspect `agy plugin list`
and resolve any replacement prompt before proceeding. Do not manage the CLI's
profile by copying into the desktop plugin directory. This maintenance route is
prepared from the CLI documentation, not yet accepted by a live update test.

For **standalone skills**, run the helper from the new unpacked release:

```text
Rscript "/new-release/nlss/scripts/R/install_nlss.R" --action status --harness vibe
Rscript "/new-release/nlss/scripts/R/install_nlss.R" --action update --source "/new-release/nlss" --harness vibe
Rscript "/new-release/nlss/scripts/R/install_nlss.R" --action remove --harness vibe
```

Update/removal need `--approve` to write. A repeated identical install/update is
a no-op. Updates stage and validate the complete replacement before switching;
the old installation is temporarily retained until activation succeeds. No
permanent backup history or watcher. Invalid/edited/unrecognized destinations
are refused, including extra files. Preserve edits and choose another destination
or resolve them deliberately; there is no force-overwrite switch. Research
projects, R packages, unrelated skills and settings are never removed.

Reinstall the selected prior release to roll back (use `update` when the target
still exists, `install` when absent). No project conversion or historical R
environment restoration occurs. Old exact-code replay may reject changed NLSS
code even when ordinary project work succeeds; that gate remains intentional.

## Maintainer build

From a development checkout, use Python 3 (standard library only):

```text
python3 scripts/build_release.py --out "/chosen/empty/release-output"
```

`--source DIR` explicitly selects another checkout. The output must be outside
the source tree and empty/new. The builder includes new working-tree runtime
files, not just Git HEAD. It excludes tests, development outputs, review material,
build tools and caches. `scripts/config.yml` → `nlss_version` supplies archive,
manifest, built skill and generated release README versions. Edit that one value
for a new release; source skill metadata is replaced during assembly.

For public distribution, build from a clean checkout of the reviewed release
commit, not an unidentified working tree. Keep `CHANGELOG.md` as the single
public change history and derive the GitHub release text from its version
section. Those repository/developer files are not required runtime assets;
see the [maintainer checklist](https://github.com/docmh/nlss/blob/main/packaging/RELEASE_CHECKLIST.md).
Before publishing, replace the preparation notices in this guide/README and
finalize the Changelog date and `CITATION.cff` for the actual release. A changed
source requires rebuilding the candidate and recording its final checksums.

Run the registered `tests/phase5/run_packaging_tests.py` with `--root DIR --keep 0`
for bounded package/maintenance validation. This does not register personal
plugins, publish archives or prove native harness acceptance. Installation-file
MD5 inventories support base-R copy/edit detection; release SHA-256 checksums
and trusted distribution remain separate from these non-signature checks.

## Route references

Checked 16 September 2026; documentation is not NLSS live acceptance evidence:

- [Agent Plugins 1.0 specification](https://agent-plugins.org/specification).
- [Codex plugin authoring](https://learn.chatgpt.com/docs/build-plugins),
  [supported surfaces](https://learn.chatgpt.com/docs/plugins) and
  [user skill locations](https://learn.chatgpt.com/docs/build-skills).
- [Claude plugins](https://code.claude.com/docs/en/plugins) and
  [plugin discovery/maintenance](https://code.claude.com/docs/en/discover-plugins).
- [Copilot local plugins](https://code.visualstudio.com/docs/agent-customization/agent-plugins#use-local-plugins).
- [Antigravity global plugin location](https://antigravity.google/docs/plugins).
- [Antigravity CLI plugins and maintenance](https://antigravity.google/docs/cli/plugins/).
- [Vibe user skills](https://docs.mistral.ai/vibe/code/cli/skills).
- [GitHub release links](https://docs.github.com/en/repositories/releasing-projects-on-github/linking-to-releases)
  and [release discovery API](https://docs.github.com/en/rest/releases/releases#get-the-latest-release).
