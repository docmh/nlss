---
tokens:
  title: "Missing Data Assessment"
  summary_table_title: "Missingness summary."
  patterns_table_title: "Missingness patterns (top {{pattern_limit}})."
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
    - key: "decision"
      label: "Decision"
    - key: "impute_method"
      label: "Impute method"
      drop_if_empty: true
    - key: "impute_value"
      label: "Impute value"
      drop_if_empty: true
    - key: "indicator"
      label: "Indicator"
      drop_if_empty: true
patterns_table:
  columns:
    - key: "pattern"
      label: "Pattern"
    - key: "missing_vars"
      label: "Missing variables"
    - key: "n"
      label: "n"
    - key: "pct_total"
      label: "%"
    - key: "missing_count"
      label: "Missing count"
      drop_if_empty: true
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{summary_table_title}}*

{{summary_table_body}}

{{note_prefix}} {{summary_note_body}}

**Table {{table_number_next}}**

*{{patterns_table_title}}*

{{patterns_table_body}}

{{note_prefix}} {{patterns_note_body}}

{{narrative_heading}}

{{narrative_default}}

{{narrative}}

---