---
tokens:
  table_title: "Tests of Fixed Effects"
  note_prefix: "*Note.*"
table:
  columns:
    - key: "term"
      label: "Effect"
    - key: "num_df"
      label: "Num *df*"
      drop_if_empty: true
    - key: "den_df"
      label: "Den *df*"
      drop_if_empty: true
    - key: "df"
      label: "*df*"
      drop_if_empty: true
    - key: "f"
      label: "*F*"
      drop_if_empty: true
    - key: "chisq"
      label: "Chi-square"
      drop_if_empty: true
    - key: "p"
      label: "Sig."
      drop_if_empty: true
note:
  template: "{{note_default}}"
---

# {{analysis_label}}

## Analysis
{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

---