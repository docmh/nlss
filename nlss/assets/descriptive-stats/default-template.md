---
tokens:
  title: "Descriptive Statistics"
  table_title: "Descriptive analyses."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "n"
      label: "n"
    - key: "mean"
      label: "M"
    - key: "sd"
      label: "SD"
    - key: "min"
      label: "Min"
    - key: "max"
      label: "Max"
note:
  template: "{{note_default}}"
narrative:
  template: "{{narrative_default}}"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---