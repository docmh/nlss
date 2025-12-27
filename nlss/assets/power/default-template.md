---
tokens:
  title: "Power analysis"
  table_title: "Power analysis results."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "analysis"
      label: "Analysis"
    - key: "mode"
      label: "Mode"
    - key: "effect_metric"
      label: "Effect"
    - key: "effect_size"
      label: "Effect size"
    - key: "alpha"
      label: "alpha"
    - key: "power"
      label: "Power"
    - key: "n_total"
      label: "N"
      drop_if_empty: true
    - key: "n_per_group"
      label: "n/group"
      drop_if_empty: true
    - key: "n1"
      label: "n1"
      drop_if_empty: true
    - key: "n2"
      label: "n2"
      drop_if_empty: true
    - key: "groups"
      label: "k"
      drop_if_empty: true
    - key: "ratio"
      label: "Ratio"
      drop_if_empty: true
    - key: "u"
      label: "u"
      drop_if_empty: true
    - key: "df"
      label: "df"
      drop_if_empty: true
    - key: "r2"
      label: "R2"
      drop_if_empty: true
    - key: "rmsea0"
      label: "RMSEA0"
      drop_if_empty: true
    - key: "rmsea1"
      label: "RMSEA1"
      drop_if_empty: true
    - key: "t_type"
      label: "t type"
      drop_if_empty: true
    - key: "alternative"
      label: "Alternative"
      drop_if_empty: true
    - key: "effect_source"
      label: "Effect source"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n\n"
  drop_empty: true
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