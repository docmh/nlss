---
tokens:
  table_title: "Model tests."
  note_prefix: "*Note.*"
table:
  columns:
    - key: "model"
      label: "Model"
      drop_if_empty: true
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "source"
      label: "Source"
    - key: "ss"
      label: "SS"
      drop_if_empty: true
    - key: "df"
      label: "df"
      drop_if_empty: true
    - key: "ms"
      label: "MS"
      drop_if_empty: true
    - key: "f"
      label: "F"
      drop_if_empty: true
    - key: "p"
      label: "Sig."
      drop_if_empty: true
    - key: "deviance"
      label: "Deviance"
      drop_if_empty: true
    - key: "chisq"
      label: "Chi-square"
      drop_if_empty: true
note:
  template: "{{note_default}}"
---
# {{analysis_label}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

---
