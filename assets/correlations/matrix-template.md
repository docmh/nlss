---
tokens:
  title: "Correlation Matrix"
  table_title: "Correlation Matrix (r Below Diagonal, p Values Above Diagonal)"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  layout: "matrix"
note:
  template: "{{note_default}} Values below the diagonal are correlations; values above are p values."
narrative:
  template: "{{narrative_default}}"
---

# {{title}}

## Analysis
{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---