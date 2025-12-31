---
tokens:
  title: "Measurement Invariance (CFA)"
  table_title: "Invariance Model Fit Indices"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "step"
      label: "Step"
    - key: "group_equal"
      label: "Constraints"
      drop_if_empty: true
    - key: "chisq"
      label: "chisq"
    - key: "df"
      label: "*df*"
    - key: "p"
      label: "*p*"
    - key: "cfi"
      label: "CFI"
    - key: "tli"
      label: "TLI"
    - key: "rmsea"
      label: "RMSEA"
    - key: "srmr"
      label: "SRMR"
    - key: "delta_cfi"
      label: "Delta CFI"
      drop_if_empty: true
    - key: "delta_rmsea"
      label: "Delta RMSEA"
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