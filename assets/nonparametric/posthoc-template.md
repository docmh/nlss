---
tokens:
  title: "Nonparametric Post Hoc"
  table_title: "Post Hoc Comparisons"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "variable"
      label: "Variable"
      drop_if_empty: true
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "group_1"
      label: "Group 1"
      drop_if_empty: true
    - key: "group_2"
      label: "Group 2"
      drop_if_empty: true
    - key: "n_1"
      label: "*n*1"
      drop_if_empty: true
    - key: "n_2"
      label: "*n*2"
      drop_if_empty: true
    - key: "median_1"
      label: "*Mdn*1"
      drop_if_empty: true
    - key: "median_2"
      label: "*Mdn*2"
      drop_if_empty: true
    - key: "iqr_1"
      label: "IQR1"
      drop_if_empty: true
    - key: "iqr_2"
      label: "IQR2"
      drop_if_empty: true
    - key: "stat_label"
      label: "Stat"
      drop_if_empty: true
    - key: "statistic"
      label: "Value"
    - key: "p"
      label: "*p*"
    - key: "p_adj"
      label: "p_adj"
      drop_if_empty: true
    - key: "effect_size_value"
      label: "Effect size"
      drop_if_empty: true
    - key: "ci_low"
      label: "CI low"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI high"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
  drop_empty: true
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