---
tokens:
  title: "Research (Academia)"
  comprehensive_title: "Comprehensive Results"
  top_title: "Most Relevant"
  note_prefix: "*Note.*"
  references_title: "References"
tables:
  comprehensive:
    columns:
      - key: "rank"
        label: "Rank"
      - key: "title"
        label: "Title"
      - key: "year"
        label: "Year"
      - key: "authors"
        label: "Authors"
      - key: "source"
        label: "Source"
      - key: "citations"
        label: "Cited By"
      - key: "link"
        label: "DOI/URL"
      - key: "relevance"
        label: "Relevance"
      - key: "keywords"
        label: "Keywords"
        drop_if_empty: true
      - key: "abstract"
        label: "Abstract"
        drop_if_empty: true
  top:
    columns:
      - key: "rank"
        label: "Rank"
      - key: "title"
        label: "Title"
      - key: "year"
        label: "Year"
      - key: "authors"
        label: "Authors"
      - key: "source"
        label: "Source"
      - key: "citations"
        label: "Cited By"
      - key: "link"
        label: "DOI/URL"
      - key: "relevance"
        label: "Relevance"
      - key: "keywords"
        label: "Keywords"
        drop_if_empty: true
      - key: "abstract"
        label: "Abstract"
        drop_if_empty: true
narrative:
  template: "{{narrative}}"
---

# {{title}}

## Analysis
{{analysis_flags}}

{{most_relevant_sections}}

{{note_prefix}} {{top_note_body}}

## Narrative

{{narrative}}

<div align="center"><strong>{{references_title}}</strong></div>

{{references_top}}
