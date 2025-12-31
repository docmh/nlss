---
name: research-academia
description: Literature search utility that aggregates academic references, outputs a comprehensive list and a ranked top subset, and formats references in NLSS format.
license: Apache-2.0
---

# Research-Academia (Utility)

## Overview

Find academic references for a topic, aggregate results across multiple scholarly sources, and produce an NLSS format-ready report focused on the most relevant items. References are formatted using the NLSS reference list guidance in `references/metaskills/formatting/reference-list.md`. A comprehensive list is still available via the analysis log or by customizing the template.

This utility uses live scholarly APIs (OpenAlex, Crossref, Semantic Scholar), so it requires network access at runtime.

## Assistant Researcher Model

NLSS assumes a senior researcher (user) and assistant researcher (agent) workflow. Requests may be vague or jargon-heavy; the agent should inspect the data, ask clarifying questions before choosing analyses, document decisions and assumptions in `scratchpad.md`, and produce a detailed, NLSS format-aligned, journal-alike report.

## Intent/Triggers

Use this utility when the user asks for academic sources or a literature scan, for example:

- "Find academic references for this topic."
- "Give me scholarly sources to support the introduction."
- "Do a comprehensive literature scan, then highlight the most relevant."

## Inputs

- A search query/topic (`--query`).
- Optional source list (`--sources`).
- Optional year window (`--year-from`, `--year-to`).
- Network access is required for the external APIs.

## Script: `scripts/R/research_academia.R`

### Rscript

```bash
Rscript <path to scripts/R/research_academia.R> --query "topic"
```

## Options

- `--query <text>`: Required search topic or query.
- `--sources <list>`: Comma-separated sources (or space-separated tokens after `--sources`). Supported: `openalex`, `crossref`, `semantic_scholar`. Defaults to `modules.research_academia.sources`.
  - PowerShell: prefer `--sources "openalex,crossref,semantic_scholar"` or `--sources openalex crossref semantic_scholar`.
- `--year-from <YYYY>` / `--year-to <YYYY>`: Optional publication year bounds.
- `--max-per-source <n>`: Max results per source (default: `modules.research_academia.max_per_source`).
- `--max-total <n>`: Max unique results overall after dedupe (default: `modules.research_academia.max_total`).
- `--top-n <n>`: Number of most-relevant items to highlight (default: `modules.research_academia.top_n`).
- `--timeout <n>`: Request timeout seconds (default: `modules.research_academia.timeout`).
- `--template <ref|path>`: Template path or key (optional).
- `--user-prompt <text>`: Original AI user prompt for logging (optional).
- `--semantic-key <text>`: Optional Semantic Scholar API key (or set `NLSS_SEMANTIC_SCHOLAR_API_KEY`). Recommended if you hit HTTP 429.
- `--log TRUE/FALSE`: Write `analysis_log.jsonl` (default: `defaults.log`).
- `--interactive`: Prompt for inputs instead of flags.

## Behavior

- Aggregates results from the selected sources, deduplicates by DOI/title, and computes a heuristic relevance score.
- The default report renders **most relevant items** as full sections; a comprehensive list can be re-enabled via template customization.

## Research Process (Agent Guidance)

When the user needs a defensible, well-grounded response, run **multiple search cycles** and combine sources:

- Start with a semantic search using `research_academia.R` on the user’s phrasing.
- Run 2–4 **query variants** (synonyms, related constructs, population/context terms, narrower vs. broader terms). Optionally add year bounds when the topic is time-sensitive.
- **Curate results** across cycles into a coherent subset (remove duplicates, prioritize peer-reviewed or highly cited items, balance recency vs. foundational).
- **Supplement** with manual web search to capture gaps (e.g., key reviews, guidelines, or landmark papers not returned by APIs).
- Combine and reconcile findings so the final response/report section is **sufficiently grounded**; if gaps remain, explicitly note limitations.
- Formats references using NLSS reference list rules (author list length, sentence case titles, source italics, DOI/URL).
- Does not modify datasets; no workspace parquet is required.

## Outputs

- `report_canonical.md`: NLSS format-ready report containing:
  - Most relevant items as sections with full abstracts.
  - Narrative summary.
  - NLSS-formatted references list.
- `analysis_log.jsonl`: Machine-readable results and run options (when logging is enabled).

Outputs are written to the resolved output directory (workspace root if `nlss-workspace.yml` is found; otherwise `defaults.output_dir` in `scripts/config.yml`).

## Template (YAML)

The default template is `assets/research-academia/default-template.md` and can be overridden via `templates.research_academia.default` in `scripts/config.yml` or with `--template`.

For a comprehensive list included in the report, use `assets/research-academia/comprehensive-template.md` via `--template`.

### Table Column Keys

Available column keys for both tables include:

`rank`, `title`, `year`, `authors`, `source`, `citations`, `link`, `relevance`, `keywords`, `abstract`.

`keywords` and `abstract` are included when available and may be truncated for readability in tables (see module defaults in `modules.research_academia.*`). The default template renders the most relevant items as a sequence of sections with full abstracts.

### Additional Tokens

`comprehensive_table_body`, `top_table_body`, `comprehensive_note_body`, `top_note_body`, `table_number_next`, `references`, `references_top`, `most_relevant_sections`.

The default template uses `most_relevant_sections` only. You can re-enable the comprehensive list by adding `{{comprehensive_table_body}}` and `{{comprehensive_note_body}}` to a custom template.
The default template now uses `{{references_top}}` so the references list is limited to the most relevant items; `{{references}}` still contains the full deduped set.

## Network Access

This utility calls external APIs and will fail without network access. If you need offline operation, skip this utility and use local reference managers instead.

## Rate Limits (Semantic Scholar)

Semantic Scholar may return HTTP 429 (rate limit). If this happens, retry later or provide an API key via `--semantic-key` or `NLSS_SEMANTIC_SCHOLAR_API_KEY`. For best header support, install the R package `curl`.

## NLSS Reference Formatting

Reference formatting follows:

- `references/metaskills/formatting/reference-list.md`

This includes author limits, sentence case for titles, Title Case and italics for source titles, alphabetical ordering by first author, and a centered **References** heading.

## Dependencies

- `jsonlite` (required for parsing API responses).
