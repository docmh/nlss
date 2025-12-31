---
tokens:
  title: "ANOVA Post Hoc"
  table_title: "Post Hoc Comparisons"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "term"
      label: "Effect"
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
    - key: "contrast"
      label: "Contrast"
      drop_if_empty: true
    - key: "mean_diff"
      label: "Mean diff"
      drop_if_empty: true
    - key: "se"
      label: "*SE*"
      drop_if_empty: true
    - key: "t"
      label: "*t*"
      drop_if_empty: true
    - key: "df"
      label: "*df*"
      drop_if_empty: true
    - key: "p"
      label: "*p*"
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