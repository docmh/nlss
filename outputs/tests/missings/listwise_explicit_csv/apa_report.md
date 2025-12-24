# Missing Data Assessment

Analysis:

- Variables: x, y
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
| x | numeric | 10 | 20.00 | listwise deletion |
| y | numeric | 0 | 0.00 | no missing |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| OO | None | 40 | 80.00 | 0 |
| MO | x | 10 | 20.00 | 1 |


*Note.* Pattern order: x, y. M = missing, O = observed.

**Narrative**

Missingness ranged from 0.00% to 20.00%. Complete cases: 40 of 50 (80.00%). Selected handling method: listwise deletion. Listwise deletion removed 10 rows (20.00%).

x: missing = 10 (20.00%). Decision: listwise deletion.
y: missing = 0 (0.00%). Decision: no missing.

---