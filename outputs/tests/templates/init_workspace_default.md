---
tokens:
  title: "Workspace Initialization"
  table_title: "Dataset workspace summary."
  note_prefix: "*Note.*"
  narrative_heading: "**Narrative**"
table:
  columns:
    - key: "dataset"
      label: "Dataset"
    - key: "type"
      label: "Type"
    - key: "rows"
      label: "Rows"
    - key: "columns"
      label: "Columns"
    - key: "source_path"
      label: "Source"
      drop_if_empty: true
    - key: "copy_path"
      label: "Parquet copy"
      drop_if_empty: true
note:
  template: "{{note_default}}"
narrative:
  template: "{{narrative_default}}"
---
{{yaml_front_matter}}
# {{title}}

Analysis:

{{analysis_flags}}

**Table {{table_number}}**

{{table_title}}

{{table_body}}

{{note_prefix}} {{note_body}}

{{narrative_heading}}

{{narrative}}


TEMPLATE_SMOKE_TEST_20251223104724_init_workspace_default
