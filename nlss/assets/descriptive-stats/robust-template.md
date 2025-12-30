---
tokens:
  title: "Descriptive Statistics (Robust)"
  table_title: "Robust Descriptive Analyses"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "n"
      label: "*n*"
    - key: "median"
      label: "*Mdn*"
    - key: "q1"
      label: "Q1"
    - key: "q3"
      label: "Q3"
    - key: "iqr"
      label: "IQR"
    - key: "mad"
      label: "MAD"
    - key: "trimmed_mean"
      label: "Trimmed *M*"
    - key: "min"
      label: "Min"
    - key: "max"
      label: "Max"
    - key: "missing_n"
      label: "Missing"
    - key: "missing_pct"
      label: "Missing %"
note:
  template: "{{note_abbrev}} {{robust_note}} {{trim_note}} {{missing_note}}"
narrative:
  row_template: "{{label}}: Mdn = {{median}}, IQR = {{iqr}}, MAD = {{mad}}, trimmed M = {{trimmed_mean}}, n = {{n}}, missing = {{missing_n}} ({{missing_pct}}%)."
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