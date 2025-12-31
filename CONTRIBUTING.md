# Contributing to NLSS™

Thanks for your interest in contributing to NLSS! 

This document explains how to contribute in a way that fits the project’s structure and agent-governed workflow.

---

## Quick Start

1) Open an issue (recommended) to align on scope and approach.  
2) Read `AGENTS.md` (project rules for maintaining, improving, or adding subskills/metaskills/utilities/templates).  
3) Create a branch, make your change, add tests/docs as needed.  
4) Open a PR and follow the checklist.

---

## Governance-First Workflow (Important)

NLSS is **agent-governed**: natural-language governance (`.md`) is a core part of the system behavior, not just documentation.

Before you add or change any of the following, **please read `AGENTS.md`:**

- governance / skills markdown files (the “instructional” `.md` content)
- deterministic R scripts
- templates, schemas, or orchestration logic
- folder structure / naming conventions

PRs that don’t follow `AGENTS.md` may be asked to restructure or rename elements.

### Agent Skills Standard Reference

NLSS follows the **Agent Skills** standard for skill packaging and discoverability.  

Specification reference (for background and structure conventions):

`https://agentskills.io/specification`

For NLSS-specific rules, `AGENTS.md` is the source of truth.

---

## License of Contributions (Apache 2.0)

NLSS is licensed under the Apache License 2.0 (see `LICENSE`).

By submitting a contribution (code, docs, governance text, examples, tests, etc.), you agree that your contribution will be licensed under **Apache-2.0**, consistent with this repository.

---

## Trademarks

“NLSS™” is a trademark of the project maintainer.  

Please see `TRADEMARKS.md` for naming and branding guidance for forks and derivatives.

---

## What to Contribute

Good first contributions:

- small fixes to governance clarity or consistency
- additional tests / examples
- improvements to logging, templates, or error messages
- documentation that helps users run NLSS reliably
- new subskills, metaskills, or utilities that follow `AGENTS.md`

If you plan a larger addition (new subsystem / major refactor), please open an issue first.

---

## Development Setup

NLSS uses R and CRAN packages. No third-party source code is bundled.

Recommended baseline:

- a recent R version (4.5.2 or higher)
- run the project’s checks described in `AGENTS.md` / README

If you’re unsure how to run the checks locally, open a draft PR and ask.

---

## Repository Layout (Current)

Key directories in the repo root:

- `assets/`: NLSS format templates used by subskills.
- `scripts/`: R scripts and shared libraries (`scripts/R/lib`).
- `references/`: subskill/metaskill/utilities reference docs.
- `tests/`: test scripts, plans, and fixtures (plus `tests/tests.yml`).
- `cmdscripts/`: test harness entrypoints and helper scripts.
- `outputs/`: generated artifacts only (e.g., `outputs/test-runs/`).

Please keep generated output in `outputs/` and do not commit it.

---

## Style and Structure

### R Code

- Keep functions small and composable.
- Prefer explicit, readable code over cleverness.
- Add comments for non-obvious statistical decisions or edge cases.
- Don’t silently swallow errors; fail loudly with actionable messages.

### Governance Markdown (`.md`)

Because these files guide LLM behavior, please:

- keep instructions unambiguous and testable
- avoid contradictory rules
- prefer “do X / don’t do Y” over vague language
- maintain the established section order and naming conventions (see `AGENTS.md`)

If the governance files use YAML frontmatter, keep it valid YAML and avoid breaking keys used by tooling.

---

## Tests and Verification

For any behavior change:

- add or update tests / fixtures if the project uses them
- include an example input and expected output if that’s the project’s validation style
- mention what you verified in the PR description

If a full test suite is heavy, at least include a minimal reproducible check.

Common test entrypoints:

- Windows PowerShell: `.\cmdscripts\tests.ps1 smoke`
- WSL/Linux: `bash cmdscripts/tests.sh smoke`

---

## Pull Request Process

1) Create a branch from `main`.
2) Make focused commits (one topic per PR when possible).
3) Update docs/governance/tests as needed.
4) Open a PR with:
   - what changed
   - why it changed
   - how you verified it
   - any compatibility notes

### PR Checklist
- [ ] I read `AGENTS.md` and followed the conventions.
- [ ] I didn’t bundle third-party source code.
- [ ] I added/updated tests or provided a reproducible verification note.
- [ ] I updated docs/governance where needed.
- [ ] I understand my contribution is under Apache-2.0.

---

## Reporting Issues

When filing an issue, please include:
- NLSS version / commit hash (if applicable)
- R version and OS
- minimal reproducible example (input + command)
- actual vs expected behavior
- logs or error output (redact secrets)

---

## Security

If you believe you found a security or privacy issue, please avoid posting details publicly right away.  
Open an issue with minimal detail and request a private channel for disclosure.

---

## Questions

If anything is unclear: open an issue and ask.  
It’s better to align early than to redo a large PR later.
