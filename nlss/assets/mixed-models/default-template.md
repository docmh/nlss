---
tokens:
  title: "Mixed Models"
  table_title: "Fixed effects estimates."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "model"
      label: "Model"
      drop_if_empty: true
    - key: "term"
      label: "Effect"
    - key: "b"
      label: "Estimate"
    - key: "se"
      label: "Std. Error"
    - key: "df"
      label: "df"
      drop_if_empty: true
    - key: "t"
      label: "t"
      drop_if_empty: true
    - key: "p"
      label: "Sig."
      drop_if_empty: true
    - key: "ci_low"
      label: "CI lower"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI upper"
      drop_if_empty: true
    - key: "std_beta"
      label: "Std. beta"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
  drop_empty: true
---
# {{title}}

{{analysis_label}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---
