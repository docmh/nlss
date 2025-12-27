---
tokens:
  title: "Imputation Summary"
  table_title: "Imputation summary."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "type"
      label: "Type"
    - key: "missing_n"
      label: "Missing n"
    - key: "missing_pct"
      label: "Missing %"
    - key: "engine"
      label: "Engine"
    - key: "method"
      label: "Method"
    - key: "impute_value"
      label: "Impute value"
      drop_if_empty: true
    - key: "imputed_n"
      label: "Imputed n"
    - key: "target"
      label: "Imputed column"
    - key: "indicator"
      label: "Indicator"
      drop_if_empty: true
    - key: "note"
      label: "Note"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative_default}}

{{narrative}}

---