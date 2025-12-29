---
tokens:
  title: "Reliability Analysis"
  table_title: "Reliability Estimates"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "analysis"
      label: "Analysis"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "method_label"
      label: "Measure"
    - key: "estimate"
      label: "Estimate"
    - key: "ci"
      label: "{{ci_label}}"
      drop_if_empty: true
    - key: "p"
      label: "*p*"
      drop_if_empty: true
    - key: "n"
      label: "*n*"
    - key: "n_raters"
      label: "Raters"
      drop_if_empty: true
    - key: "model"
      label: "Model"
      drop_if_empty: true
    - key: "type"
      label: "Type"
      drop_if_empty: true
    - key: "unit"
      label: "Unit"
      drop_if_empty: true
    - key: "weight"
      label: "Weights"
      drop_if_empty: true
    - key: "var1"
      label: "Var 1"
      drop_if_empty: true
    - key: "var2"
      label: "Var 2"
      drop_if_empty: true
    - key: "f"
      label: "*F*"
      drop_if_empty: true
    - key: "df1"
      label: "*df*1"
      drop_if_empty: true
    - key: "df2"
      label: "*df*2"
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
