# Missing Data Assessment

Analysis:

- Variables: a, b
- Method: listwise
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

| Variable | Type | Missing n | Missing % | Decision |
| --- | --- | --- | --- | --- |
| a | numeric | 1 | 2.00 | listwise deletion |
| b | numeric | 0 | 0.00 | no missing |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| OO | None | 49 | 98.00 | 0 |
| MO | a | 1 | 2.00 | 1 |


*Note.* Pattern order: a, b. M = missing, O = observed.

**Narrative**

Missingness ranged from 0.00% to 2.00%. Complete cases: 49 of 50 (98.00%). Selected handling method: listwise deletion. Listwise deletion removed 1 rows (2.00%).

a: missing = 1 (2.00%). Decision: listwise deletion.
b: missing = 0 (0.00%). Decision: no missing.

---