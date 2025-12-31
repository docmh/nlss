# Research-Academia Test Plan

## Scope

- Validate network calls return results when allowed.
- Validate input validation (missing query, invalid sources).
- Validate numeric guardrails (invalid max-per-source/max-total/top-n/timeout).
- Run network queries and verify NLSS report + log output.
- Verify positive single-source runs (openalex-only, crossref-only).
- Verify comprehensive template renders the full list when requested.
- Verify sources parsing variants (`--sources=...`, whitespace tokens).
- Verify year-bound flags render in analysis flags.
- Verify Semantic Scholar path can run when API key is available (may skip if not set).
- Verify template override support and marker rendering.

## Prereqs

- Run from repo root.
- Rscript available.
- R package `jsonlite` installed.
- Network access available for external APIs.

## Run

```bash
bash tests/run_research_academia_tests.sh
```

## Expectations

- Missing `--query` should fail.
- Invalid `--sources` should fail.
- Invalid `--max-per-source`, `--max-total`, `--top-n`, and `--timeout` should fail.
- When network runs are enabled:
  - `report_canonical.md` is created under `defaults.output_dir`.
  - Output includes "Research (Academia)", "Most Relevant", and a References section.
  - `analysis_log.jsonl` includes `"module":"research-academia"`.
- OpenAlex-only and Crossref-only runs should render matching "Sources:" lines.
- Comprehensive template run should include "Comprehensive Results".
- Sources parsing variants (equals syntax, whitespace tokens) should render in "Sources:".
- Year bounds should appear in analysis flags.
- Template override run should render the marker string and still include references.

## Outputs

- `outputs/test-runs/<timestamp>/research_academia_test.log`
- `outputs/test-runs/<timestamp>/<defaults.output_dir>/report_canonical.md`
- `outputs/test-runs/<timestamp>/<defaults.output_dir>/analysis_log.jsonl`
