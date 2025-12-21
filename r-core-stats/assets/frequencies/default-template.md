---
tokens:
  title: "Frequencies"
  table_title: "Frequency distributions."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "level"
      label: "Level"
    - key: "n"
      label: "n"
    - key: "pct_total"
      label: "%"
    - key: "pct_valid"
      label: "Valid %"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  template: "{{narrative_default}}"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

{{table_title}}

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---
