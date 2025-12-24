---
tokens:
  title: "Data Exploration"
  overview_table_title: "Variable overview."
  levels_table_title: "Value levels."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "class"
      label: "Class"
    - key: "measurement_level"
      label: "Scale"
    - key: "valid_n"
      label: "n"
    - key: "missing_pct"
      label: "Missing %"
    - key: "unique_n"
      label: "Unique"
    - key: "mean"
      label: "M"
    - key: "sd"
      label: "SD"
    - key: "min"
      label: "Min"
    - key: "max"
      label: "Max"
levels_table:
  columns:
    - key: "variable"
      label: "Variable"
    - key: "level"
      label: "Level"
    - key: "n"
      label: "n"
    - key: "pct_total"
      label: "%"
    - key: "pct_valid"
      label: "Valid %"
narrative:
  row_template: "{{full_sentence}}"
  join: "\n"
---
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

{{overview_table_title}}

{{overview_table_body}}

{{note_prefix}} {{overview_note_body}}

**Table {{table_number_next}}**

{{levels_table_title}}

{{levels_table_body}}

{{note_prefix}} {{levels_note_body}}

{{narrative_heading}}

{{narrative}}

---


TEMPLATE_SMOKE_TEST_20251223104724_data_explorer_default
