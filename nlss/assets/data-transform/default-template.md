---
tokens:
  title: "Data Transformations"
  table_title: "Transformation log."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "step"
      label: "Step"
    - key: "action"
      label: "Action"
    - key: "variable"
      label: "Variable"
    - key: "new_variable"
      label: "New Variable"
    - key: "details"
      label: "Details"
    - key: "note"
      label: "Note"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

{{table_title}}

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative_default}}

{{narrative}}

---
