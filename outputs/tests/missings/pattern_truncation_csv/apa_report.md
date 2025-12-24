# Missing Data Assessment

Analysis:

- Variables: p1, p2, p3, p4
- Method: indicator
- Low missingness threshold: 0.05
- Moderate missingness threshold: 0.2
- High missingness threshold: 0.4
- Drop threshold: 0.6
- Indicator threshold: 0.3
- Indicator suffix: _miss
- Skewness threshold: 1
- Max patterns: 2
- Rounding digits: 2

**Table 1**

Missingness summary.

| Variable | Type | Missing n | Missing % | Decision | Impute method | Impute value |
| --- | --- | --- | --- | --- | --- | --- |
| p1 | numeric | 8 | 26.67 | impute | mean | 0.07 |
| p2 | numeric | 6 | 20.00 | impute | mean | 0.00 |
| p3 | numeric | 7 | 23.33 | impute | mean | 0.03 |
| p4 | numeric | 5 | 16.67 | impute | mean | -0.17 |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 2).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| OOOO | None | 10 | 33.33 | 0 |
| MOOO | p1 | 5 | 16.67 | 1 |
| Other patterns | Multiple | 15 | 50.00 |  |


*Note.* Pattern order: p1, p2, p3, p4. M = missing, O = observed. Other patterns grouped.

**Narrative**

Missingness ranged from 16.67% to 26.67%. Complete cases: 10 of 30 (33.33%). Selected handling method: single imputation with missingness indicators.

p1: missing = 8 (26.67%). Decision: impute. Impute mean = 0.07.
p2: missing = 6 (20.00%). Decision: impute. Impute mean = 0.00.
p3: missing = 7 (23.33%). Decision: impute. Impute mean = 0.03.
p4: missing = 5 (16.67%). Decision: impute. Impute mean = -0.17.

---