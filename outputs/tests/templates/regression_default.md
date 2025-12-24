---
tokens:
  title: "Regression"
  table_title: "Regression coefficients."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "model"
      label: "Model"
      drop_if_empty: true
    - key: "group"
      label: "Group"
      drop_if_empty: true
    - key: "term"
      label: "Predictor"
    - key: "b"
      label: "b"
    - key: "se"
      label: "SE"
    - key: "t"
      label: "t"
      drop_if_empty: true
    - key: "z"
      label: "z"
      drop_if_empty: true
    - key: "p"
      label: "p"
    - key: "ci_low"
      label: "CI low"
      drop_if_empty: true
    - key: "ci_high"
      label: "CI high"
      drop_if_empty: true
    - key: "beta"
      label: "beta"
      drop_if_empty: true
    - key: "exp_b"
      label: "exp(b)"
      drop_if_empty: true
    - key: "exp_ci_low"
      label: "exp CI low"
      drop_if_empty: true
    - key: "exp_ci_high"
      label: "exp CI high"
      drop_if_empty: true
    - key: "boot_ci_low"
      label: "Boot CI low"
      drop_if_empty: true
    - key: "boot_ci_high"
      label: "Boot CI high"
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


TEMPLATE_SMOKE_TEST_20251223104724_regression_default
