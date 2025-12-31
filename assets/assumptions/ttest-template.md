---
tokens:
  title: "Assumption Checks (t Tests)"
  table_title: "Assumption Checks for t Tests"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "assumption"
      label: "Assumption"
    - key: "test"
      label: "Test"
    - key: "target"
      label: "Variable"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "n"
      label: "n"
    - key: "statistic"
      label: "Statistic"
    - key: "df"
      label: "df"
      drop_if_empty: true
    - key: "p"
      label: "p"
      drop_if_empty: true
    - key: "decision"
      label: "Decision"
      drop_if_empty: true
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