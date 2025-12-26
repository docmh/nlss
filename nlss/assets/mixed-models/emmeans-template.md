---
tokens:
  title: "Mixed Models: Marginal Means"
  table_title: "Estimated marginal means and contrasts."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "term"
      label: "Term"
    - key: "level"
      label: "Level"
      drop_if_empty: true
    - key: "contrast"
      label: "Contrast"
      drop_if_empty: true
    - key: "emmean"
      label: "EMM"
      drop_if_empty: true
    - key: "estimate"
      label: "Estimate"
      drop_if_empty: true
    - key: "se"
      label: "SE"
    - key: "df"
      label: "df"
      drop_if_empty: true
    - key: "t"
      label: "t"
      drop_if_empty: true
    - key: "p"
      label: "p"
      drop_if_empty: true
    - key: "p_adj"
      label: "p_adj"
      drop_if_empty: true
    - key: "ci_low"
      label: "CI low"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI high"
      drop_if_empty: true
    - key: "method"
      label: "Method"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n"
  drop_empty: true
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
