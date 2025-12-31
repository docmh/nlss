---
tokens:
  table_title: "Tests of Effects"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "term"
      label: "Source"
    - key: "df"
      label: "df"
    - key: "ss"
      label: "SS"
      drop_if_empty: true
    - key: "ms"
      label: "MS"
      drop_if_empty: true
    - key: "f"
      label: "F"
      drop_if_empty: true
    - key: "p"
      label: "p"
      drop_if_empty: true
    - key: "partial_eta_sq"
      label: "Partial eta²"
      drop_if_empty: true
    - key: "eta_sq"
      label: "eta²"
      drop_if_empty: true
    - key: "partial_omega_sq"
      label: "Partial omega²"
      drop_if_empty: true
    - key: "omega_sq"
      label: "omega²"
      drop_if_empty: true
    - key: "boot_ci_low"
      label: "Boot CI low"
      drop_if_empty: true
    - key: "boot_ci_high"
      label: "Boot CI high"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
  drop_empty: true
---

# {{analysis_label}}

## Analysis
{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---
