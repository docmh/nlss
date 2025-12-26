---
tokens:
  title: "Scale Analysis"
  table_title: "Item analysis and reliability."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "item"
      label: "Item"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "n"
      label: "n"
    - key: "mean"
      label: "M"
    - key: "sd"
      label: "SD"
    - key: "item_total_r"
      label: "r_it"
      drop_if_empty: true
    - key: "item_rest_r"
      label: "r_drop"
      drop_if_empty: true
    - key: "alpha_if_deleted"
      label: "alpha_if_deleted"
      drop_if_empty: true
    - key: "missing_pct"
      label: "Missing %"
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
