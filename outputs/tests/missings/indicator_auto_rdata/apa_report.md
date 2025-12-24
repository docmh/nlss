# Missing Data Assessment

Analysis:

- Variables: c, cat
- Method: indicator
- Low missingness threshold: 0.05
- Moderate missingness threshold: 0.2
- High missingness threshold: 0.4
- Drop threshold: 0.6
- Indicator threshold: 0.3
- Indicator suffix: _miss
- Skewness threshold: 1
- Max patterns: 10
- Rounding digits: 2

**Table 1**

Missingness summary.

| Variable | Type | Missing n | Missing % | Decision | Impute method | Impute value | Indicator |
| --- | --- | --- | --- | --- | --- | --- | --- |
| c | numeric | 15 | 30.00 | impute + indicator | mean | 0.40 | c_miss |
| cat | factor | 15 | 30.00 | impute + indicator | mode | B | cat_miss |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| OO | None | 20 | 40.00 | 0 |
| MO | c | 15 | 30.00 | 1 |
| OM | cat | 15 | 30.00 | 1 |


*Note.* Pattern order: c, cat. M = missing, O = observed.

**Narrative**

Missingness ranged from 30.00% to 30.00%. Complete cases: 20 of 50 (40.00%). Selected handling method: single imputation with missingness indicators. Missingness indicators added for: c, cat.

c: missing = 15 (30.00%). Decision: impute + indicator. Impute mean = 0.40. Indicator: c_miss.
cat: missing = 15 (30.00%). Decision: impute + indicator. Impute mode = B. Indicator: cat_miss.

---