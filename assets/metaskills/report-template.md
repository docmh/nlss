---
name: metaskill-report
description: Optional NLSS Markdown manuscript example with standard paper sections and reusable table/figure block tokens; not a required report structure.
tokens:
  table_block: |
    **Table {{table_number}}**

    *{{table_title}}*
    
    {{table_header}}
    
    {{table_body}}
    
    *Note.* {{table_note}}
  figure_block: |
    **Figure {{figure_number}}**
    
    *{{figure_title}}*
    
    ![Figure {{figure_number}}. {{figure_title}}]({{figure_path}})
    
    *Note.* {{figure_note}}
  references: ""
  nlss_footer: |
    ---

    Created with [NLSS](https://github.com/docmh/nlss).
---

<!--
Optional manuscript example, not a default obligation or report-saving schema.

- Use only if helpful for the requested manuscript or explicitly selected by the user; conversational answers and selected sections need no scaffold.
- Choose, rename, reorder or omit sections according to the research question and user requirements. Never invent study details or literature to fill placeholders.
- If a title page is used, supply known author details, title and date; omit unknown fields or leave clearly marked placeholders only in an intended draft.
- Tables/figures support the synthesis when useful; the block tokens are optional presentation examples, not a required numerical or semantic schema.
- Preserve source/model identity, values and uncertainty; link existing run-local figures instead of copying artifacts. The automatic protocol remains separate from an authored report.
- Scientific content and evidence do not depend on this layout. Future output formats must not turn the example into mandatory report structure.

-->

# {{title}}

`<user-placeholder>`  

`<affiliation-placeholder>`

`<email-placeholder>`

{{date-today}}

## Abstract

{{abstract}}

*Keywords:* {{keywords}}

# {{title}}

{{introduction}}

## Method

### Participants

{{participants}}

### Measures

{{measures}}

### Procedure

{{procedure}}

### Analytic Strategy

{{analytic_strategy}}

## Results

### Descriptive Statistics

{{results_descriptive}}

### Preliminary Analyses

{{results_preliminary}}

### Hypothesis Testing

{{results_hypothesis}}

### Exploratory Analyses

{{results_exploratory}}

## Discussion

### Summary of Findings

{{discussion_summary}}

### Limitations

{{discussion_limitations}}

### Implications

{{discussion_implications}}

### Future Directions

{{discussion_future}}

## Conclusion

{{conclusion}}

## References

{{references}}

{{nlss_footer}}
