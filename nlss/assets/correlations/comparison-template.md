---
tokens:
  title: "Correlation Comparisons"
  table_title: "Fisher *r*-to-*z* Comparisons Between Groups"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "group1"
      label: "Group 1"
    - key: "group2"
      label: "Group 2"
    - key: "var1"
      label: "Variable 1"
    - key: "var2"
      label: "Variable 2"
    - key: "r1"
      label: "*r*1"
    - key: "r2"
      label: "*r*2"
    - key: "n1"
      label: "*n*1"
    - key: "n2"
      label: "*n*2"
    - key: "z"
      label: "*z*"
    - key: "p"
      label: "*p*"
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
  drop_empty: true
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
