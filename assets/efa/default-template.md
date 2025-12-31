---
tokens:
  title: "Exploratory Factor Analysis"
  table_title: "Factor Loadings"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "item"
      label: "Item"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "factor"
      label: "Factor"
    - key: "loading"
      label: "Loading"
    - key: "h²"
      label: "h²"
      drop_if_empty: true
    - key: "u²"
      label: "u²"
      drop_if_empty: true
    - key: "complexity"
      label: "Complexity"
      drop_if_empty: true
    - key: "cross_loading"
      label: "Cross-loadings"
      drop_if_empty: true
note:
  template: "{{note_default}}"
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