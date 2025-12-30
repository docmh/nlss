---
name: metaskill-report
description: APA 7 report scaffold for metaskill outputs with standard paper sections and reusable table/figure block tokens.
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
---

<!--
Default metaskill report template.

- Omit Keywords and Introduction if theoretical context is not available.
- Adjust or replace subsections when warranted by the study design or metaskill.
- Tables and figures should be purpose-built for the report (no copy/paste from report_canonical.md).
- Tables and figures can be added anywhere in the report body as needed using the table_block and figure_block tokens.

-->

# {{title}}

**Abstract**
{{abstract}}

*Keywords:* {{keywords}}

{{introduction}}

## Method

### **_Participants_**
{{participants}}

### **_Measures_**
{{measures}}

### **_Procedure_**
{{procedure}}

### **_Analytic Strategy_**
{{analytic_strategy}}

## Results

### **_Descriptive Statistics_**
{{results_descriptive}}

### **_Preliminary Analyses_**
{{results_preliminary}}

### **_Hypothesis Testing_**
{{results_hypothesis}}

### **_Exploratory Analyses_**
{{results_exploratory}}

## Discussion

### **_Summary of Findings_**
{{discussion_summary}}

### **_Limitations_**
{{discussion_limitations}}

### **_Implications_**
{{discussion_implications}}

### **_Future Directions_**
{{discussion_future}}

## Conclusion
{{conclusion}}

**References**
{{references}}