---
tokens:
  title: "ANOVA"
  table_title: "ANOVA results."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "model"
      label: "Model"
      drop_if_empty: true
    - key: "term"
      label: "Effect"
    - key: "df1"
      label: "df1"
    - key: "df2"
      label: "df2"
    - key: "f"
      label: "F"
    - key: "p"
      label: "p"
    - key: "partial_eta_sq"
      label: "eta_p2"
    - key: "eta_sq"
      label: "eta_sq"
      drop_if_empty: true
    - key: "boot_ci_low"
      label: "Boot CI low"
      drop_if_empty: true
    - key: "boot_ci_high"
      label: "Boot CI high"
      drop_if_empty: true
    - key: "ss"
      label: "SS"
      drop_if_empty: true
    - key: "ms"
      label: "MS"
      drop_if_empty: true
    - key: "df1_gg"
      label: "df1_GG"
      drop_if_empty: true
    - key: "df2_gg"
      label: "df2_GG"
      drop_if_empty: true
    - key: "p_gg"
      label: "p_GG"
      drop_if_empty: true
    - key: "df1_hf"
      label: "df1_HF"
      drop_if_empty: true
    - key: "df2_hf"
      label: "df2_HF"
      drop_if_empty: true
    - key: "p_hf"
      label: "p_HF"
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
