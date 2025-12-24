# Missing Data Assessment

Analysis:

- Variables: x, y
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
| x | logical | 0 |  | keep |
| y | logical | 0 |  | keep |


*Note.* Missing % uses total N. Numeric imputation uses mean or median (skew threshold = 1.00); categorical uses mode.

**Table 2**

Missingness patterns (top 10).

| Pattern | Missing variables | n | % |
| --- | --- | --- | --- |
| No patterns |  |  |  |


*Note.* Pattern order: x, y. M = missing, O = observed.

**Narrative**

Selected handling method: single imputation.

x: missing = 0 (NA%). Decision: keep.
y: missing = 0 (NA%). Decision: keep.

---