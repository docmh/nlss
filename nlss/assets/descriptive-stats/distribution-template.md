---
tokens:
  title: "Descriptive Statistics (Distribution)"
  table_title: "Distribution-focused descriptive analyses."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "n"
      label: "n"
    - key: "mean"
      label: "M"
    - key: "sd"
      label: "SD"
    - key: "variance"
      label: "Var"
    - key: "cv"
      label: "CV"
    - key: "skewness"
      label: "Skew"
    - key: "kurtosis"
      label: "Kurtosis"
    - key: "p5"
      label: "P5"
    - key: "p10"
      label: "P10"
    - key: "p90"
      label: "P90"
    - key: "p95"
      label: "P95"
    - key: "outliers_tukey"
      label: "Outliers (Tukey)"
    - key: "outliers_z"
      label: "Outliers (z)"
    - key: "n_unique"
      label: "Unique"
    - key: "mode"
      label: "Mode"
note:
  template: "{{note_abbrev}} {{cv_note}} {{percentile_note}} {{outlier_note}} {{mode_note}} {{missing_note}}"
narrative:
  row_template: "{{label}}: M = {{mean}}, SD = {{sd}}, skew = {{skewness}}, kurtosis = {{kurtosis}}, p10/p90 = {{p10}}/{{p90}}, outliers (Tukey/z) = {{outliers_tukey}}/{{outliers_z}}."
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

*{{table_title}}*

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}

---