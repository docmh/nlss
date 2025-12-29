---
tokens:
  title: "Structural Equation Modeling"
  table_title: "SEM Parameter Estimates"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "path"
      label: "Path"
    - key: "label"
      label: "Label"
      drop_if_empty: true
    - key: "est"
      label: "*b*"
    - key: "se"
      label: "*SE*"
    - key: "z"
      label: "*z*"
      drop_if_empty: true
    - key: "p"
      label: "*p*"
    - key: "ci_low"
      label: "CI low"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI high"
      drop_if_empty: true
    - key: "std"
      label: "Std"
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