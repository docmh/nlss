# Missing Data Assessment

Analysis:

- Variables: all_miss, y
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

| Variable | Type | Missing n | Missing % | Decision |
| --- | --- | --- | --- | --- |
| all_miss | numeric | 50 | 100.00 | all missing |
| y | numeric | 0 | 0.00 | keep |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % | Missing count |
| --- | --- | --- | --- | --- |
| MO | all_miss | 50 | 100.00 | 1 |


*Note.* Pattern order: all_miss, y. M = missing, O = observed.

**Narrative**

Missingness ranged from 0.00% to 100.00%. Complete cases: 0 of 50 (0.00%). Selected handling method: single imputation.

all_miss: missing = 50 (100.00%). Decision: all missing.
y: missing = 0 (0.00%). Decision: keep.

---