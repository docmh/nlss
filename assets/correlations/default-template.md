---
tokens:
  title: "Correlations"
  table_title: "Correlation Matrix"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "var1"
      label: "Variable 1"
    - key: "var2"
      label: "Variable 2"
    - key: "r"
      label: "r"
    - key: "r0"
      label: "r0"
      drop_if_empty: true
    - key: "z_r0"
      label: "z (r0)"
      drop_if_empty: true
    - key: "p_r0"
      label: "p (r0)"
      drop_if_empty: true
    - key: "ci"
      label: "{{ci_label}}"
      drop_if_empty: true
    - key: "boot_ci"
      label: "Boot CI"
      drop_if_empty: true
    - key: "p"
      label: "p"
    - key: "p_adj"
      label: "p_adj"
      drop_if_empty: true
    - key: "n"
      label: "n"
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