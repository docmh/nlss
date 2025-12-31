---
tokens:
  title: "Cross-Tabulations by Group"
  table_title: "Cross-Tabulation Results Stratified by Group"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "row_var"
      label: "Row Variable"
    - key: "col_var"
      label: "Column Variable"
    - key: "row_level"
      label: "Row Level"
    - key: "col_level"
      label: "Column Level"
    - key: "n"
      label: "*n*"
    - key: "pct_row"
      label: "Row %"
      drop_if_empty: true
    - key: "pct_col"
      label: "Column %"
      drop_if_empty: true
    - key: "pct_total"
      label: "Total %"
      drop_if_empty: true
    - key: "phi"
      label: "phi"
      drop_if_empty: true
    - key: "cramers_v"
      label: "Cramer's V"
      drop_if_empty: true
    - key: "contingency_c"
      label: "C"
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