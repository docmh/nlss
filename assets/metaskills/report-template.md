---
name: metaskill-report
description: NLSS format report scaffold for metaskill outputs with standard paper sections and reusable table/figure block tokens.
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

    Created with [NLSS](https://github.com/docmh/nlss-demo).
---

<!--
Default metaskill report template.

- Omit Keywords and Introduction if theoretical context is not available.
- Do not replace title-page placeholders (user, affiliation, email).
- Replace {{date-today}} with actual date.
- Copy {{title}} to the second title header.
- Adjust, replace or remove elements or subsections when warranted by the study design or metaskill.
- Tables and figures should be purpose-built for the report (no copy/paste from report_canonical.md).
- Tables and figures can be added anywhere in the report body as needed using the table_block and figure_block tokens.
- Use tables to display repetitive information patterns (e.g., descriptive statistics, matrices, hypothesis test results).
- Use figures to visualize key results (e.g., distributions, model fits, effect sizes) or to illustrate concepts or models.

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