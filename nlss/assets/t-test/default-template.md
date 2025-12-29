---
tokens:
  title: "*t* Tests"
  table_title: "*t* Test Results"
  note_prefix: "*Note.*"
  narrative_heading: "## Narrative"
table:
  columns:
    - key: "test_type"
      label: "Test"
    - key: "variable"
      label: "Variable"
    - key: "measure_1"
      label: "Measure 1"
      drop_if_empty: true
    - key: "measure_2"
      label: "Measure 2"
      drop_if_empty: true
    - key: "group_1"
      label: "Group 1"
      drop_if_empty: true
    - key: "group_2"
      label: "Group 2"
      drop_if_empty: true
    - key: "n_1"
      label: "*n*1"
    - key: "n_2"
      label: "*n*2"
      drop_if_empty: true
    - key: "mean_1"
      label: "*M*1"
    - key: "mean_2"
      label: "*M*2"
      drop_if_empty: true
    - key: "sd_1"
      label: "*SD*1"
    - key: "sd_2"
      label: "*SD*2"
      drop_if_empty: true
    - key: "mean_diff"
      label: "Mean diff"
    - key: "t"
      label: "*t*"
    - key: "df"
      label: "*df*"
    - key: "p"
      label: "*p*"
    - key: "d"
      label: "*d*"
    - key: "ci_low"
      label: "CI low"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI high"
      drop_if_empty: true
    - key: "boot_ci_low"
      label: "Boot CI low"
      drop_if_empty: true
    - key: "boot_ci_high"
      label: "Boot CI high"
      drop_if_empty: true
    - key: "boot_d_ci_low"
      label: "Boot *d* CI low"
      drop_if_empty: true
    - key: "boot_d_ci_high"
      label: "Boot *d* CI high"
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
