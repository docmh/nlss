---
name: research-academia
description: Literature retrieval from OpenAlex, Crossref, and Semantic Scholar with source-status and response-snapshot evidence, heuristic ranking, and NLSS reference formatting.
license: Apache-2.0
---

# Research-Academia (Utility)

## Purpose and research judgment

Use this utility when the user asks for academic references, a literature scan, or scholarly support for an argument. It retrieves metadata and available abstracts from selected scholarly APIs and prepares references using `references/metaskills/format-document.md`.

This is a literature-retrieval utility, **not a statistical analysis or replay request**. Live results can change. A successful request means that the selected API responses were processed; it does not certify exhaustive coverage, study quality, peer review, or support for the user's claim.

NLSS treats the user as senior researcher and the agent as assistant researcher.
The agent owns the literature investigation and synthesis; the R utility supports
retrieval, not source appraisal. Follow the obligations below whenever literature
research is requested or needed. They do not impose a report structure or make
every conversational answer a literature-review task.

## Agentic literature research (required)

**Actively investigate, do not merely accept the ranked list.** Use the agent's
available scholarly/web search and reading capabilities alongside utility
retrieval: refine the question and queries, seek relevant syntheses, follow useful
references/citing work, and inspect original sources. Choose the routes that
actually improve coverage; every route need not be used in every task. Adequate
supplied sources can be reused for a bounded write-up after appraisal, but an
explicit request to search requires actual searching. Verify currency before
calling a literature account current. If search or source access is unavailable,
disclose the limitation rather than claim a completed investigation.

**Quality first, quantity second.** Prioritize well-established, citable academic
sources, current high-quality reviews and meta-analyses (systematic syntheses
where appropriate), and well-executed primary research directly addressing the
question. Retain relevant foundational work even if older. Prefer scholarly
publications and authoritative academic books/methodological sources over popular
summaries; label preprints or other provisional evidence when used. Peer review,
journal reputation, citation count, recency and the label “meta-analysis” are not
by themselves evidence of methodological quality.

Judge relevance to the actual population, constructs and design, and appraise
the methods supporting the claim: sampling, measurement, analysis, uncertainty
and bias as applicable. For reviews/meta-analyses, consider search/selection
quality, the included studies, heterogeneity and publication bias where relevant.
For primary research, consider design appropriateness, transparent reporting and
robustness or replication evidence. These are judgment criteria, not a compulsory
scoring sheet, checklist paragraph or automatic hierarchy that overrides quality.
Seek material contrary findings; do not select only support for the preferred
hypothesis or count overlapping reviews as independent bodies of evidence.

Read the relevant original text before making substantive claims about it; use
full text where the methods or interpretation require it. Check bibliographic
identity and provide a usable citation/DOI or stable scholarly link. Distinguish
full-text appraisal, abstract-only information and a primary study known only
through a review; do not imply direct reading or copy unverified citations from
another bibliography. Disclose access gaps and corrections/retractions encountered
when checking the source's current publication status.

Stop when the question is adequately supported and material disagreements or
gaps are addressed within the agreed scope/resources; state remaining limits.
Do not pad the bibliography, target a citation count or claim systematic-review
coverage from a bounded scan. Keep citations and useful selection/coverage notes
in the report or existing scratchpad where needed, not a new per-hit journal or
duplicate document collection. Sources found through ordinary agent browsing
are cited normally; do not label them as captured utility responses or invent
run IDs. The existing API evidence and report-saving mechanisms remain unchanged.

## Command and options

```bash
Rscript <path to scripts/R/research_academia.R> --query "stress and coping" --sources openalex,crossref
```

- `--query <text>`: Required topic or query; `--topic` remains an alias.
- `--sources <list>`: `openalex`, `crossref`, and/or `semantic_scholar`. Comma-separated or whitespace-separated values are accepted, including `--sources=openalex,crossref`. Source aliases are normalized; unknown sources are rejected. Merely mentioning a source in the query does not select it.
- `--year-from <YYYY>` / `--year-to <YYYY>`: Optional four-digit publication-year bounds. Bounds apply consistently to every source, including Semantic Scholar. Records without a year are excluded when a bound is requested; exclusions are recorded.
- `--max-per-source <n>`: Retrieval cap for each source.
- `--max-total <n>`: Maximum unique results retained after deduplication.
- `--top-n <n>`: Maximum items highlighted in the report.
- `--timeout <n>`: Request timeout in seconds.
- `--template <ref|path>`: Template key or path.
- `--user-prompt <text>`: Original user prompt for the audit/log, subject to credential redaction.
- `--semantic-key <text>`: Optional Semantic Scholar credential. Prefer `NLSS_SEMANTIC_SCHOLAR_API_KEY` (or the supported `SEMANTIC_SCHOLAR_API_KEY` fallback), because command-line arguments may be visible in operating-system process listings.
- `--log TRUE/FALSE`: Control optional standalone logging; current-project evidence and root protocol are always retained without an extra JSONL journal.
- `--interactive`: Prompt for inputs. `--help` prints usage.

Caps and timeout must be positive finite integers; reversed year bounds and malformed options fail before retrieval. Defaults come from `modules.research_academia.*`, `defaults.log`, and the canonical configuration. `NLSS_CONFIG_PATH` supports a private configuration overlay; explicit CLI values take precedence. No dataset or workspace parquet is required or fabricated.

## Retrieval, ranking, and status

The utility retrieves bounded pages, normalizes source records, applies year bounds, and deduplicates by normalized DOI or, without a DOI, title. The retained relevance heuristic combines normalized API score (0.5), literal query-word overlap (0.2), log-normalized citations (0.2), and recency (0.1). Source scores are not calibrated measures of research quality. Citation count and metadata availability can bias ordering. Full normalized records and sanitized source responses remain available for inspection.

Each source records success/partial/failed, error category, received count, response indices, limit attainment, and exhaustion where observable. Earlier valid pages survive a later page failure. Repeated cursors stop with a diagnostic. HTTP 429 retries are bounded and every attempt is recorded.

- **Success:** Every requested source completed its bounded retrieval. A valid empty response is a successful zero-hit search, not a network failure. The outer utility record has status `completed`.
- **Partial:** Some source/page failed while another provided usable retrieval evidence. Available results are published with explicit source failures and outer status `partial`.
- **Failed:** All requested sources failed. The CLI exits nonzero and saves a diagnostic utility record with outer status `failed`; it does not append a normal canonical report.

Mandatory status, source failures, and coverage limitations appear independently of the presentation template and `--log`. Transport, HTTP, invalid JSON, invalid response/item schema, and exhausted fixture queues are distinguishable from genuine zero hits. Console diagnostics do not repeat raw transport exceptions or request headers.

## Project outputs and evidence

Select `--project` or use the nearest current ancestor marker. Outputs use `.nlss/utility-runs/` and the root protocol, independently of active-dataset selection. Unmarked standalone output uses `defaults.output_dir`. Dataset values are not modified; symlinked output-directory paths are rejected before publication.

- `report_canonical.md`: Appended formatted results for successful or partial retrievals.
- `.nlss/utility-runs/<run-id>/request.json`: Sanitized query/resolved options, ranking/year-filter semantics, code identity, environment, and an explicit non-replay declaration. It is not a raw configuration or raw CLI dump.
- `.nlss/utility-runs/<run-id>/result.json`: Status, normalized full records, source diagnostics, and hashes of the evidence artifacts.
- `.nlss/utility-runs/<run-id>/responses.json`: Per-attempt transport, timestamps, endpoint, HTTP status/error category, and sanitized parsed response data. These are inspectable snapshots, not byte-for-byte raw HTTP responses.
- `.nlss/utility-runs/<run-id>/template.md`: Frozen sanitized presentation template.
- `.nlss/utility-runs/<run-id>/output.md`, or `diagnostic-output.md` for failure: The run-specific output.

The shared utility publisher protects the canonical report, log, and detected manifest under the project publication lock and rolls them back on ordinary publication errors. This is not a claim of power-loss-safe transactions. Retained research snapshots can contain research topics, names, abstracts, and user prompts: apply the project's normal access and retention controls.

## Templates and semantic reporting

The default is `assets/research-academia/default-template.md`, configurable through `templates.research_academia.default` or `--template`. It highlights selected items with full abstracts and `references_top`. Use `assets/research-academia/comprehensive-template.md` for the broader retained list.

Table keys: `rank`, `title`, `year`, `authors`, `source`, `citations`, `link`, `relevance`, `keywords`, `abstract`. Table keywords and abstracts may be shortened using module defaults; full normalized values remain in the evidence.

Additional tokens: `comprehensive_table_body`, `top_table_body`, `comprehensive_note_body`, `top_note_body`, `table_number_next`, `references`, `references_top`, `most_relevant_sections`. `references` covers the retained deduplicated set; `references_top` covers the highlighted subset. Custom templates can select these views, but cannot suppress mandatory retrieval diagnostics. Final interpretive writing can go beyond all these views while remaining grounded in inspected sources.

## Network, credentials, and offline tests

Ordinary use calls external APIs and requires network access. An API credential is used only for the relevant request header. Known credential values, credential-shaped response fields, and credential-bearing URL query parameters are redacted before snapshots, parsed records, templates, reports, or logs are saved. Raw configuration, headers, and raw credential-bearing command lines are not persisted. Do not place credentials in queries, contact metadata, or templates; redaction is not a reason to treat arbitrary secret text as safe input.

The `curl` package is required when an authenticated header is needed. Without `curl`, unauthenticated requests can use the base-R transport; credentials are never silently dropped. The base-R fallback does not expose the HTTP status: its snapshot records a null status with `http_status_observed: false`, not an invented HTTP 200. Rate limits and API availability remain external conditions; retry later when appropriate rather than treating missing responses as evidence of no relevant literature.

The explicit test-only `NLSS_RESEARCH_FIXTURES` environment variable selects a JSON response fixture with `schema_version: 1` and per-source packet queues under `sources`. Packets contain `status` and `body`, malformed `text`, or `transport_error`. Every attempt consumes a packet. Malformed or exhausted fixtures never fall back to live networking, and reports/evidence are clearly marked as fixture transport. This seam verifies retrieval handling; it is not an offline literature database.

Normal module tests use deterministic fixtures. `NLSS_RESEARCH_LIVE_TESTS=1` explicitly opts the legacy smoke script into external API checks; those are separate availability checks, not ordinary deterministic regression coverage. See `tests/phase2/research-README.md` for the registered offline runner.

## Dependencies

The mandatory shared bootstrap uses the canonical NLSS dependencies, including `yaml`, `jsonlite`, and `digest`. `jsonlite` parses response/fixture data; `curl` supports HTTP status and authenticated headers. No packages or credentials are installed, created, or rotated by this utility.
