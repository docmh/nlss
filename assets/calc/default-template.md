---
tokens:
  title: "Calculator Output"
  table_title: "Calculated Values"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "name"
      label: "Name"
    - key: "expression"
      label: "Expression"
    - key: "value"
      label: "Value"
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
---

# {{title}}

## Analysis
{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative_default}}

{{narrative}}

---