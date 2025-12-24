# Missing Data Assessment

Analysis:

- Variables: skew
- Method: impute
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

| Variable | Type | Missing n | Missing % | Decision | Impute method | Impute value |
| --- | --- | --- | --- | --- | --- | --- |
| skew | numeric | 5 | 10.00 | impute | median | 0.82 |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| O | None | 45 | 90.00 | 0 |
| M | skew | 5 | 10.00 | 1 |


*Note.* Pattern order: skew. M = missing, O = observed.

**Narrative**

Missingness ranged from 10.00% to 10.00%. Complete cases: 45 of 50 (90.00%). Selected handling method: single imputation.

skew: missing = 5 (10.00%). Decision: impute. Impute median = 0.82.

---