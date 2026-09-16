# Phase 2: literature-retrieval utility tests

Run the registered `tests.scripts.phase2_research_r` entry point:

```bash
Rscript tests/phase2/run_research_tests.R --root /tmp/nlss-research-tests --keep 1
Rscript tests/phase2/run_research_tests.R --root /tmp/nlss-research-focused --keep 1 --match 'ranking|credential|failed'
```

The runner also accepts `NLSS_TEST_ROOT`, `NLSS_KEEP_RUNS`, and `NLSS_TESTS_CONFIG`. It uses private configuration overlays and explicit `NLSS_RESEARCH_FIXTURES` queues for actual public CLI calls. There is no live-network fallback and no real API credential. The fake credential fixture is intentionally public test data. No canonical configuration is rewritten.

Every CLI call records argv, entry-point SHA-256, UTC start/end, and actual exit status. The run records all production-R hashes before/after, per-case assertions, elapsed time, and `results.json`. Dependencies are `yaml`, `jsonlite`, and `digest`; no optional statistical packages are needed.

Coverage includes:

- Independent exact ranking/deduplication references across all three sources; full normalized abstracts, source attribution, and reference formatting.
- Source isolation, aliases, whitespace/equals syntax, caps, year filtering including Semantic Scholar/undated records, and literal query punctuation.
- Genuine zero hits versus transport, HTTP, JSON, response/item-schema, and fixture-exhaustion failures; partial pages, repeated cursors, and bounded rate-limit retries.
- An isolated base-transport unit reads a local JSON file and verifies that unavailable HTTP status is not invented; no network request is made.
- Status propagation to utility evidence, diagnostic-only all-failed output, mandatory notices under custom templates, and artifact hashes/non-statistical replay declarations.
- Credential echoes in response fields, prose, links, and user prompts; scan every published file and console output for the fake credential.
- Mandatory evidence with log disabled; default/comprehensive/custom templates; dataset-free workspace routing; private configuration/CLI precedence; strict option rejection and malformed fixture no-fallback behavior.

The legacy `tests/smoke/run_research_academia_tests.sh` delegates to this runner by default. Set `NLSS_RESEARCH_LIVE_TESTS=1` only for an explicitly requested separate external API smoke check. Offline assertions verify NLSS's handling of known responses; they do not certify current API availability, exhaustive literature coverage, study quality, or the semantic adequacy of a final research synthesis.
