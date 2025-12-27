---
name: metaskill-report
description: Default metaskill report template aligned to APA 7 paper structure with optional tables and figures.
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
---
<!--
Default metaskill report template.
- Omit Keywords and Introduction if theoretical context is not available.
- Adjust or replace subsections when warranted by the study design or metaskill.
- Tables and figures should be purpose-built for the report (no copy/paste from report_canonical.md).
- Tables and figures can be added anywhere in the report body as needed using the table_block and figure_block tokens.
-->
# {{title}}

## Abstract
{{abstract}}

*Keywords:* {{keywords}}

## Introduction
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
