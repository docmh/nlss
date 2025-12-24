# Missing Data Assessment

Analysis:

- Variables: d, e
- Method: drop
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
| d | numeric | 40 | 80.00 | drop |  |  |  |
| e | numeric | 17 | 34.00 | impute + indicator | mean | 0.17 | e_miss |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| MO | d | 26 | 52.00 | 1 |
| MM | d, e | 14 | 28.00 | 2 |
| OO | None | 7 | 14.00 | 0 |
| OM | e | 3 | 6.00 | 1 |


*Note.* Pattern order: d, e. M = missing, O = observed.

**Narrative**

Missingness ranged from 34.00% to 80.00%. Complete cases: 7 of 50 (14.00%). Selected handling method: drop high-missing variables with imputation. Dropped variables (missing >= 60.00%): d. Missingness indicators added for: e.

d: missing = 40 (80.00%). Decision: drop.
e: missing = 17 (34.00%). Decision: impute + indicator. Impute mean = 0.17. Indicator: e_miss.

---