---
name: apa7-markdown
description: APA 7 formatting rules that can be expressed in Markdown, with mappings for headings, citations, references, tables, and figures.
---

# APA 7 Formatting in Markdown (Reference)

## Overview

This reference maps APA 7 formatting rules to Markdown elements for NLSS reports and templates. It focuses on rules that can be represented in Markdown and flags items that require renderer or template support.

Markdown Parsing Note: Use a blank line (two consecutive `\n` characters) to separate block elements. This is especially important around tables and figures so titles, images, and notes render as distinct blocks.

## Scope and Renderer Limits

APA specifies page layout and typography (margins, double spacing, fonts, running head, page numbers, title page layout). These are controlled by the renderer or document template and cannot be enforced by plain Markdown alone. Use your Markdown renderer or export pipeline to apply those settings. APA papers are organized into Title Page, Abstract, Main Body, and References.

## General APA Manuscript Structure

APA manuscripts follow a standard order, with sections that may be omitted depending on the genre (e.g., empirical article vs. student paper). In Markdown, use top-level headings to reflect these blocks.

```md
Title Page

Abstract
Keywords

Main Body
- Introduction
- Method (Participants, Materials/Measures, Procedure)
- Results
- Discussion

References

Footnotes (if any)
Tables (if not embedded in text)
Figures (if not embedded in text)
Appendices (if any)
```

Notes:

- For student papers, include a title page and omit running head unless required.
- For professional manuscripts, include a running head and page numbers per APA guidance.
- NLSS renders tables and figures inline; do not move them to the end of the manuscript.
- The Introduction section does not use a heading; the paper title serves as the Level 1 heading.

## Headings and Section Labels

APA uses five heading levels and they must be used in order. The "Introduction" section does not use a heading; the paper title functions as the Level 1 heading. Section labels such as Abstract and References appear on their own line, centered and bold.

### Markdown Mapping

```md
| APA Level | APA Format | Markdown Mapping (Rendering Notes) |
| --- | --- | --- |
| 1 | Centered, bold, Title Case | `# Heading` (centering depends on renderer) |
| 2 | Flush left, bold, Title Case | `## Heading` (formatting depends on renderer) |
| 3 | Flush left, bold italic, Title Case | `### **_Heading_**` (formatting depends on renderer) |
| 4 | Indented, bold, Title Case, period (run-in) | `**Heading.**` Paragraph text continues. |
| 5 | Indented, bold italic, Title Case, period (run-in) | `**_Heading._**` Paragraph text continues. |
```

Note: Markdown does not natively center headings or create run-in indentation. Exact alignment or indentation is renderer-dependent.

## Seriation (Lists)

Use numbered lists for ordered procedures or steps, and bulleted lists for unordered items. When list items are full sentences, capitalize and punctuate them like sentences. When list items are short phrases, keep a consistent grammar and omit end punctuation unless the list item completes a sentence. In-text seriation can use letters in parentheses (a), (b), (c) when needed.

## In-Text Citations

APA uses the author-date system. Every in-text citation must correspond to a reference list entry.

Common patterns (Markdown text is identical to APA text):

```md
- One author: (Smith, 2020) or Smith (2020).
- Two authors: use "and" in narrative citations and "&" in parenthetical citations.
- Three or more authors: use first author + "et al." for all citations unless ambiguity requires more names.
- Group authors with abbreviations: spell out the full name on first citation and include the abbreviation; use the abbreviation on subsequent citations.
- Multiple works in the same parentheses: order alphabetically and separate with semicolons.
- Same author, same year: add letter suffixes (2020a, 2020b).
- Unknown author: use a shortened title in place of the author; italicize if the source is a book/report and use quotation marks if it is an article/chapter.
- No date: use "n.d.".
- Direct quotes: include page or locator (p. 10, pp. 10-12). For sources without page numbers, cite a section heading or paragraph number instead.
```

## Title Case and Sentence Case

APA uses two capitalization styles for titles and headings: Title Case and Sentence Case. Use the rules below when rendering titles, headings, and reference-list entries in Markdown.

### Title Case Rules

- Capitalize major words and lowercase most minor words.
- Minor words include short conjunctions, articles, and short prepositions (three letters or fewer).

Minor Word List (APA Examples):

```md
Short conjunctions: and, as, but, for, if, nor, or, so, yet
Articles: a, an, the
Short prepositions: as, at, by, for, in, of, off, on, per, to, up, via
```

Title Case Example:

```md
Effects of Sleep Deprivation on Working Memory in Adolescents
Self-Report Measures and Post-Test Outcomes
```

### Sentence Case Rules

- Capitalize the first word of a title or heading and the first word of any subtitle.
- Capitalize the first word after a colon, em dash, or end punctuation in a heading.
- Capitalize nouns followed by numerals or letters.
- Capitalize proper nouns and words that are capitalized in their original form (e.g., acronyms).
- Lowercase all other words.

Sentence Case Example:

```md
Effects of sleep deprivation on working memory in adolescents
Self-report measures and post-test outcomes
```

### Where to Use Each

```md
Title Case:
- Paper title and all headings
- Titles of works in text (articles, books, reports, webpages)
- Titles of tests or measures
- Titles of journals, websites, and magazines
- Table titles and figure titles
- Named subsections (e.g., Results, Discussion, Conclusion)

Sentence Case:
- Titles of works in the reference list (articles, books, reports, webpages)
```

## Italics and Quotation Marks in Text

Use italics and quotation marks to distinguish titles, words-as-words, and quoted material. Avoid using either for emphasis.

### Italics

Use italics for:

- Titles of standalone works (books, reports, theses/dissertations, films, albums, websites, software).
- Titles of periodicals (journals, magazines, newspapers).
- Words or letters used as linguistic examples.
- Genus, species, and varietal names.
- New or technical terms on first use (only if clarity requires it).

Examples:

```md
Participants read *The Great Gatsby*.
The article "Sleep and memory" appeared in *Journal of Health Psychology*.
The word *and* is a conjunction.
```

### Quotation Marks

Use double quotation marks for:

- Short direct quotations (fewer than 40 words) with citations.
- Titles of works that are part of a larger work (journal articles, book chapters, web pages, episodes).
- Words or phrases used in a special or ironic sense on first use only.

Use single quotation marks only for quotations within quotations. Do not use quotation marks for block quotations (see Quotations section).

Examples:

```md
The authors described the findings as "unexpected" (Smith, 2020, p. 14).
The chapter "Measuring sleep" appears in *Handbook of Sleep Research*.
The so-called "gold standard" was not feasible in this setting.
```

## Numbers, Numerals, and Ordinals

General rule: spell out numbers zero through nine and use numerals for 10 and above, unless an exception applies.

Use numerals for:

- Numbers that immediately precede a unit of measurement (e.g., 5 mg, 10.5 cm).
- Statistical or mathematical functions, fractions or decimals, percentages, ratios, and percentiles.
- Time, dates, ages, scores/points on a scale, exact sums of money, and numerals used as numerals (e.g., the 6 key).
- Numbers that denote a specific place in a series or part of a table/figure after the noun (e.g., Table 2, Figure 5, Item 4). When the number comes before the noun, follow the general rule.

Use words for:

- Numbers that begin a sentence, title, or heading (reword when possible).
- Common fractions (e.g., one fifth).
- Universally accepted phrases (e.g., Twelve Apostles).

Back-to-back numerical modifiers: combine numerals and words (e.g., 2 two-way interactions, ten 7-point scales) or reword for clarity.

Ordinal numbers follow the same rules as cardinal numbers. Superscripted ordinal suffixes are allowed in APA, but pure Markdown has no superscript; use plain text forms (e.g., 4th) consistently.

## Decimal Fractions, Rounding, and *p* Values

- Use a leading zero for numbers less than 1 when the value can exceed 1 (e.g., 0.86, 0.48 cm).
- Omit the leading zero when the value cannot exceed 1 (e.g., correlations, proportions, significance levels).
- Round as much as possible while preserving meaning; two decimal places is typical for correlations, proportions, and inferential test statistics.
- Report means and standard deviations from integer scales to one decimal place.
- Report exact *p* values to two or three decimals when possible; use *p* < .001 for smaller values.

## Statistical Symbols and Variables (Typeface in Markdown)

- Use words in narrative text (e.g., "the means were"); use symbols when paired with mathematical operators (e.g., *M* = 7.74).
- Population parameters are typically Greek letters; sample estimates use italic Latin letters (e.g., rho vs. *r*).
- Use italic uppercase *N* for total sample size and italic lowercase *n* for subsamples.
- Use percent and currency symbols only with numerals; use the word "percentage" or the currency name when no numeral is given.
- Typeface rules: standard (roman) type for Greek letters and nonvariable abbreviations (e.g., log, GLM), bold for vectors and matrices, italic for other statistical symbols (e.g., *t*, *F*, *M*, *SD*).
- Spacing: add spaces around operators (a + b = c). For subtraction, use spaces on both sides (z - y = x); for negative values, use a space before but not after the minus sign (-8.25).

Pure Markdown does not support subscripts or superscripts. Use plain-text stand-ins such as `M_control` (escape underscores if needed) or write the identifier in parentheses (e.g., *M* (control)).

## Equations and Formulas

- Punctuate equations to fit the surrounding sentence whether inline or displayed.
- Keep short, simple equations inline; use a slash for fractions.
- Use parentheses, then brackets, then braces to clarify order of operations: first ( ), then [( )], then {[( )]}.
- Display equations that are complex or need numbering; number displayed equations consecutively with the number in parentheses near the right margin.
- When referring to a numbered equation in text, write "Equation 3" or "the third equation" rather than abbreviating.

Markdown Example (Inline and Displayed Equations):

```md
The effect was significant, *t*(45) = 2.14, *p* = .037.

r = (x - x_bar) / s
(1)
```

## Reporting Empirical Results (APA 7-Aligned Best Practices)

This section summarizes APA 7-aligned reporting practices for empirical results, aligned to the statistical procedures supported by NLSS. The examples are illustrative rather than exhaustive; adapt them to the exact model, design, and outputs in your analysis. Apply the general principles first, then the procedure-specific templates as needed.

### General Principles (All Procedures)

- Identify the analysis and the variables tested in text, then provide the statistical evidence.
- Report the test statistic, degrees of freedom (when applicable), and the exact *p* value (use *p* < .001 for smaller values).
- Report effect sizes and confidence intervals for key estimates where possible.
- Use parentheses for test statistics and degrees of freedom; use brackets for confidence interval limits.
- Do not repeat descriptive statistics in text if they are already in a table or figure.
- If you used complex models (e.g., factor analysis, SEM, multivariate models), describe the model specification, estimation, and software used, and note any assumption violations or estimation problems.

### Descriptive Statistics (descriptive-stats, data-explorer)

Report sample size and summary statistics for each variable and group. If you show a table, keep the text brief.

```md
Participants reported moderate satisfaction (*M* = 3.82, *SD* = 0.74, *N* = 214).
```

### Frequencies (frequencies)

Report counts and percentages, noting missing data when relevant.

```md
Most participants identified as full-time students (*n* = 142, 66.4%), with 12 missing responses.
```

### Crosstabs and Chi-Square Tests (crosstabs)

Report the chi-square statistic, df, and *p* value; add an effect size (e.g., phi or Cramer's V) and confidence interval when available.

```md
A chi-square test of independence indicated an association between gender and preference, χ2(3) = 9.12, *p* = .028, Cramer's *V* = .21.
```

### Correlations (correlations)

Report the correlation coefficient, degrees of freedom (*N* - 2), and *p* value; include confidence intervals if available.

```md
Stress was positively related to burnout, *r*(198) = .41, *p* < .001.
```

### Partial Correlations (correlations)

Report the partial correlation with controls noted in text, along with *p* and df when available.

```md
Controlling for age and gender, sleep quality remained associated with stress, *r*(196) = -.29, *p* < .001.
```

### *t* Tests (t-test)

Report the *t* statistic, df, *p* value, and an effect size (e.g., Cohen's d) with a confidence interval if available.

```md
The intervention group scored higher than controls, *t*(74) = 2.56, *p* = .013, *d* = 0.59.
```

### ANOVA / ANCOVA / Mixed ANOVA (anova)

Report the *F* statistic with numerator and denominator df, *p* value, and an effect size (e.g., η2 or partial η2). When relevant, report post hoc tests and corrections.

```md
There was a main effect of condition on accuracy, *F*(2, 117) = 4.08, *p* = .019, η2 = .07.
```

### Post Hoc Comparisons (anova)

When post hoc tests are used, report the correction and the key pairwise results.

```md
Tukey-adjusted comparisons showed condition A outperformed condition C, mean difference = 0.42, 95% CI [0.10, 0.74], *p* = .008.
```

### Regression Models (regression)

Report overall model fit (e.g., R2, *F*, df, *p*), then key coefficients with SEs, test statistics, and *p* values. Tables are often clearer for multiple predictors.

```md
The model was significant, *R*2 = .18, *F*(3, 196) = 14.33, *p* < .001. Age predicted outcomes, *b* = -0.04, *t*(196) = -2.87, *p* = .005.
```

### Regression With Interactions (regression)

Report the interaction term and, when probed, the simple slopes or conditional effects.

```md
The interaction between stress and support was significant, *b* = -0.18, *t*(192) = -2.41, *p* = .017. Simple slopes showed a stronger stress effect at low support, *b* = 0.31, *p* = .002.
```

### Mixed Models (mixed-models)

Report fixed effects with estimates, SEs, test statistics, and *p* values; describe the random-effects structure and estimation method; include model fit indices when reported.

```md
The fixed effect of time was significant, *b* = 0.12, *t*(312) = 3.01, *p* = .003. The model included random intercepts for participants.
```

```md
A random-slopes model fit the data, with a significant time effect, *b* = 0.10, *t*(308) = 2.64, *p* = .009, and an ICC of .18.
```

### Nonparametric Tests (nonparametric)

Report the test statistic, df (if applicable), and *p* value; include an effect size (e.g., r or η2) when possible.

```md
Groups differed on ranks, *H*(2) = 7.84, *p* = .020.
```

### Reliability and Scale Analysis (reliability, scale)

Report reliability estimates (e.g., Cronbach's α, ω, ICC, κ) with confidence intervals when available, and summarize item behavior if relevant.

```md
The scale showed good internal consistency, α = .86.
```

```md
Interrater reliability was good, ICC(2,1) = .81, 95% CI [0.70, 0.89].
```

```md
Agreement for the categorical rating was substantial, κ = .74, *p* < .001.
```

### Factor Analysis and SEM (efa, sem)

Describe the model specification, estimation method, and fit indices, and report key loadings or paths with SEs and confidence intervals.

```md
An EFA with principal axis factoring and oblimin rotation supported a two-factor solution (KMO = .86; Bartlett’s test, *p* < .001), explaining 58% of the variance. Loadings ≥ .40 are reported in Table 2.
```

```md
Model fit was acceptable, χ2(34) = 45.12, *p* = .092, CFI = .97, RMSEA = .04.
```

```md
The indirect effect of stress on health via sleep was significant, b = 0.12, 95% CI [0.05, 0.20].
```

```md
Metric invariance was supported, with ΔCFI = .002 and ΔRMSEA = .001 relative to the configural model.
```

### Power Analyses (power)

Report the test family, effect size assumption, α level, target power, and resulting sample size or achieved power.

```md
An a priori power analysis for a two-tailed t test (*d* = 0.50, α = .05, power = .80) indicated *N* = 128.
```

```md
Sensitivity analysis indicated that with *N* = 120, α = .05, and power = .80, the minimum detectable effect was *d* = 0.52.
```

## Quotations

Short quotations are integrated into the text with quotation marks and a citation. Quotations of 40 or more words are formatted as a block quotation: start on a new line, indent 0.5 in, do not use quotation marks, and place the citation after the final punctuation.

Markdown block quote pattern (indentation is renderer-dependent):

```md
> This is a block quotation. Continue the quote on the next line if needed.
> 
> (Author, Year, p. 10)
```

## DOIs and URLs in Reference Lists

Use DOIs and URLs to help readers locate sources. APA 7 prefers DOI links when available.

- Present DOIs and URLs as clickable hyperlinks (use the full `https://` form).
- Do not label DOIs with "doi:" or "DOI:" in APA 7.
- If a DOI is available, include it even when the source was read in print.
- If no DOI is available, use a stable URL to the source.
- Use "Retrieved Month Day, Year, from URL" only when a retrieval date is needed (e.g., content designed to change over time or sources without a date).
- Database or archive names are usually omitted; include them only when the work is available exclusively in that database, then add the DOI/URL after the database name.

Examples:

```md
Author, A. A., & Author, B. B. (2020). Title of article in sentence case. *Title of Periodical, 12*(3), 45–56. https://doi.org/10.0000/xxxxx

Author, A. A. (2021). Title of article in sentence case. *Title of Periodical, 14*(2), 112–130. https://www.journalwebsite.org/article

Organization Name. (n.d.). Title of entry in sentence case. *Title of Reference Work*. Retrieved January 13, 2020, from https://www.example.com/entry

Author, A. A. (2018). Title of work in sentence case. Database Name. https://doi.org/10.0000/xxxxx
```

## Reference List

The reference list appears on a new page with the label "References" centered and bold. Entries are double-spaced, use a 0.5 in hanging indent, and are ordered alphabetically by the first author.

Author and title formatting:

- Authors are listed as surname, initials.
- List up to 20 authors; if there are 21 or more, list the first 19, then an ellipsis, then the final author.
- Titles of articles, reports, and books use Sentence Case.
- Journal titles are in Title Case and italicized.
- Italicize the source name (journal/book/report), not the article title.

Markdown does not provide a native hanging-indent feature. Use your renderer or export pipeline to apply hanging indents in the reference list.

## Tables

APA tables are numbered with Arabic numerals in the order they are mentioned. The table number is bold, and the table title is italicized in Title Case on the next line. Notes appear below the table in this order: general note, specific notes, and probability notes.

Best-Practice Hint: Use tables for precise values and comparisons (e.g., descriptive statistics, regression coefficients, correlation matrices). Place each table as close as possible to its first mention in the text.

Markdown table pattern:

```md
**Table 1**

*Title in Title Case*

| Column | Column |
| --- | --- |
| ... | ... |

*Note.* General note text. Use superscripts for specific notes and asterisks for probability notes.
```

## Figures

Figures include graphs, charts, photos, and other graphical displays. Figures are numbered with Arabic numerals in the order they are mentioned. The figure number is bold and the figure title is italicized in Title Case on the next line. Place the image under the number and title, with a legend in or under the image and any notes below.

Best-Practice Hint: Use figures to show patterns, trends, or relationships that are easier to grasp visually (e.g., distributions, interactions, trajectories). Place each figure as close as possible to its first mention in the text.

Markdown figure pattern:

```md
**Figure 1**

*Title in Title Case*

![Alt text describing the figure](path/to/figure.png)

*Note.* General note text.
```

## Sources

- Purdue OWL, APA Formatting and Style Guide: General Format - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/general_format.html
- Purdue OWL, APA Headings and Seriation - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/apa_headings_and_seriation.html
- Purdue OWL, In-Text Citations: Basics - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/in_text_citations_the_basics.html
- Purdue OWL, In-Text Citations: Author/Authors - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/in_text_citations_author_authors.html
- Purdue OWL, Reference List: Basic Rules - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/reference_list_basic_rules.html
- Purdue OWL, Changes in the 7th Edition - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/apa_changes_7th_edition.html
- Purdue OWL, Reference List: Articles in Periodicals - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/reference_list_articles_in_periodicals.html
- Purdue OWL, Reference List: Electronic Sources - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/reference_list_electronic_sources.html
- Purdue OWL, APA Tables and Figures - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/apa_tables_and_figures.html
- UNC Charlotte Writing Resources Center, APA Title Format and Capitalization - https://wrc.charlotte.edu/wp-content/uploads/sites/272/2018/06/APATitleFormatandCapitalization.pdf
- American Psychological Association. (2020). Publication manual of the American Psychological Association (7th ed.). https://studylib.net/doc/26484906/publication-manual-book
- Purdue OWL, Statistics in APA - https://owl.purdue.edu/owl/research_and_citation/apa_style/apa_formatting_and_style_guide/statistics_in_apa.html
- Scribbr, Reporting Research Results in APA Style - https://www.scribbr.com/apa-style/results-section/
- Scribbr, Reporting Statistics in APA Style - https://www.scribbr.com/apa-style/numbers-and-statistics/
- Kahn, J. H. (2021). Reporting Statistics in APA Style - https://about.illinoisstate.edu/jhkahn/apastats/
- PERRLA, Block Quotes in APA - https://www.perrla.com/the-perrlablog/apa-block-quote/
- Writing Commons, APA Format - General Formatting Guidelines - https://writingcommons.org/article/apa-format-general-formatting-guidelines/
- Scribbr, APA in-text citations - https://www.scribbr.com/apa-style/in-text-citation/
